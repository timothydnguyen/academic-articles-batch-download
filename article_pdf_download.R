# ===============================
# PACKAGES
# ===============================
# library("devtools")
# install_github("massimoaria/bibliometrix") # Needs to be installed from GitHub not CRAN
library(tools) # for file manipulations
library(tidyverse) # for general purpose data manip
library(bibliometrix) # For reading and analyzing ISI stuff
library(crminer) # for getting texts from DOI
library(XML) # for parsing elsevier xml responses
library(rvest) # for parsing html responses
library(jsonlite) # for parsing json objects


# ===============================
# CONSTANTS
# ===============================
data_dir <- './data' # Relative path to repository. To be changed as needed
isi_dir <- file.path(data_dir, 'isi_searches')

# Create the main output directory
output_dir <- file.path(data_dir, 'pdf_output')
# Check if pdf_output directory exists
dir.create(output_dir, showWarnings = FALSE)

# PDF subdirectory
pdf_output_dir <- file.path(output_dir, 'pdfs')
# Check if pdf_output_dir directory exists
dir.create(pdf_output_dir, showWarnings = FALSE)

# Non-PDF files subdirectory
nopdf_output_dir <- file.path(output_dir, 'non-pdfs')
# Check if pdf_output_dir directory exists
dir.create(nopdf_output_dir, showWarnings = FALSE)



# ===============================
# FUNCTIONS
# ===============================

# elsevier links require a special webscraping process, so I wrote a function for it 

#' This function uses webscraping techniques to download a PDF file from elseviers server
#'
#' @param elsevier_xml_link A character; elsevier link of type xml as returned by crminer's crmlinks() function
#' @param filepath A character; path to desired output location for downloaded PDF
#'
#' @return Nothing. Download PDFs to HD
#' @export
#'
#' @examples
#' elsevier_pdf_download('https://api.elsevier.com/content/article/PII:0167198795004585?httpAccept=text/xml','/Users/timothy/Documents/soilc-text_mapping/data')
elsevier_pdf_download <- function(elsevier_xml_link, filepath) {
  # getting science direct link from elsevier xml contents
  scidir_html_link <- elsevier_xml_link %>%
    read_xml() %>%
    xml_find_all('//@href') %>%
    xml_text()
  # getting route to initial pdf download link on science direct's domain
  json_obj <- scidir_html_link[2] %>% # we need to index to the second link in the `scidir_html_link` obj
    read_html() %>% # get html response from science direct link retrieved from elsevier XML response
    html_nodes('body') %>% # start from the body section
    html_nodes('div') %>% # get the div contents within that body section
    html_nodes('script') %>% # get the script contents within that div
    html_text() %>% # extract text (JSON string)
    fromJSON() # convert JSON string to R obj
  # constructing url; second arg in paste function is route to initial pdf link obtained by navigating R obj representation of the JSON string
  url_of_interest <- paste0('https://www.sciencedirect.com', json_obj$article$pdfDownload$linkToPdf) 
  # getting temporary link
  read_html(url_of_interest) %>% 
    html_node('head') %>% # start in the head section of the html
    html_nodes('meta[http-equiv="Refresh"]') %>% # from there, seek a meta tag with attribute http-equiv="Refresh"
    html_attr('content') %>% # within that tag, grab the character string containing the PDF direct access url
    substr(7, 100000000L) %>% # the first 6 chars are not part of the url we want, so we slice them off
    download.file(filepath) # download pdf to provided filepath
}


#' This function checks if a file is binary
#'
#' @param filepath A character; path to target file
#' @param max An integer; max number of characters in file to be checked (default 1000)
#'
#' @return A boolean; TRUE if file is binary and FALSE otherwise
#' @export
#'
#' @examples
#' is_binary('~/Desktop/example.pdf')
is_binary <- function(filepath,max=1000){
  f=file(filepath,"rb",raw=TRUE)
  b=readBin(f,"int",max,size=1,signed=FALSE)
  close(f)
  return(max(b)>128)
}


# ===============================
# MAIN
# ===============================

## STEP 1: READ DOIs/METADATA FROM BIB FILES
# Combine all the bib files. The user needs only add the file to the directory full of bib files
filepath_list <- as.list(file.path(isi_dir, dir(isi_dir)))
file_list <- convert2df(do.call(readFiles, filepath_list))

# removing duplicate records
# NOTE: turns out that this still lets some duplicate files through (see: 'STEP 4: POST-PROCESSING')
# Pretty time consuming step
soil.health <- duplicatedMatching(file_list, Field="TI") 


## STEP 2: ORGANIZE LINKS
message('===============================\nORGANIZING LINKS\n===============================')
# Select attributes of interest and clean the author field
my_df <- data.frame(Name=paste(gsub(";.*$", "", soil.health$AU),soil.health$PY,soil.health$JI),
                    DOI=soil.health$DI, stringsAsFactors = FALSE)
my_df <- my_df[!is.na(my_df$DOI),] # Of 8693 observations, 6406 have valid DOIs

# collecting links
# Note: This step takes ~20min to run 
my_df$links <- sapply(my_df$DOI, crm_links) # getting links for each DOI
# Remove references for which no DOI URL was found
my_df <- my_df[lapply(my_df$links, length) > 0,] # 5759 of 6406 DOIs returned links via crm_links()

# elsevier links require a separate download process, so we distinguish them here
for (i in 1:length(my_df$links)) {
  if (grepl('elsevier',my_df$links[[i]][[1]])) { # checking for string 'elsevier' in link
    my_df$elsevier[i] <- TRUE
  } else {
    my_df$elsevier[i] <- FALSE
  }
}

# selecting a single link for each DOI (up until now, there has been a list of links assoc. to each DOI)
for (i in 1:dim(my_df)[1]) {
  if (my_df$elsevier[i]) { # if it's from elsevier, we want to get the xml link
    link <- my_df$links[[i]]$xml$xml
  } else if ('pdf' %in% names(my_df$links[[i]])) { # otherwise, we prefer the 'pdf' link type
    link <- my_df$links[[i]]$pdf$pdf
  } else if ('unspecified' %in% names(my_df$links[[i]])) { # our last preference is the 'unspecified' link type
    link <- my_df$links[[i]]$unspecified$unspecified
  } else { # we don't handle links of type 'html' or 'plain', because they almost never provide pdf download; moreover, we only want xml links from elsevier because we only handle those
    link <- NA
  }
  my_df$download_link[i] <- as.character(link)
}



## STEP 3: DOWNLOAD PDFS FROM LINKS
message('===============================\nDOWNLOADING PDFS FROM LINKS\n===============================')

# Here, the elsevier_pdf_download() function is called repeatedly via a loop that iterates through the rows of the dataframe created in the preceding
# 'organize links' section of the script
for (i in 1:nrow(my_df)) {
  url <- my_df$download_link[i]
  my_df$path[i] <- paste0(file.path(pdf_output_dir, my_df$Name[i]), '.pdf')
  if (my_df$elsevier[i]) {
    
    my_df$downloaded <- tryCatch(elsevier_pdf_download(url, my_df$path[i]),
                                      error=function(cond) {
                                        message(cond)
                                        return(0)
                                      }, 
                                      finally = message(paste("\nProcessed URL:", url)))
  } else {
    # DOWNLOADING OTHER LINKS
    my_df$downloaded[i] <- tryCatch(download.file(url, my_df$path[i]),
                                 error=function(cond) {
                                   message(cond)
                                   return(0)
                                 }, 
                                 finally = message(paste("\nProcessed URL:", url)))
  }
  message('[', i, '/', dim(my_df)[1], ']')
}

message('===============================\nPDFS DOWNLOADED\n===============================')

## STEP 4: POST-PROCESSING
# distinguish real pdf files from other files (mainly html webpages)
for (i in 1:dim(my_df)[1]) {
  if (file.exists(my_df$path[i])) {
    my_df$downloaded[i] <- TRUE
    my_df$is_pdf[i] <- is_binary(my_df$path[i])
  } else {
    my_df$downloaded[i] <- FALSE
    my_df$is_pdf[i] <- FALSE
  }
}
# Add the flags for downloaded and PDF to the data frame
my_df$downloaded <- as.logical(my_df$downloaded) 
my_df$is_pdf <- as.logical(my_df$is_pdf)

# Extract some statistics
download_success <- sum(my_df$downloaded) # out of 5759 acquired links, 4604 produced downloaded files
unique_files <- length(unique(my_df$path[my_df$downloaded])) # out of 4604 downloaded files, 4539 are unique
unique_pdfs <- length(unique(my_df$path[my_df$downloaded & my_df$is_pdf])) # out of 4539 unique downloaded files, 4057 are binary files (PDFs)
message(sprintf("Over the %i acquired links, %i PDFs were succesfully downloaded", nrow(my_df), unique_pdfs))

# Extract the files info that were not PDFs
non_pdf_paths <- unique(my_df$path[my_df$downloaded & !my_df$is_pdf]) # For investigative purposes, here are the paths for the non-PDF files (482) that were downloaded

## Move the non-pdf files to a specific directory
# Create the destination list
html_paths <- file.path(nopdf_output_dir, 
                        paste0(basename(file_path_sans_ext(non_pdf_paths)),
                               ".html")
                        )
# Move the files
file.rename(from = non_pdf_paths, to = html_paths)

## Fix the double dot before file extension
pdf_files <- dir(pdf_output_dir, full.names = TRUE)
pdf_fixed <- gsub("\\.\\.pdf","\\.pdf",pdf_files)
file.rename(from = pdf_files , to = pdf_fixed)

# output information regarding the download processs to csv
summary_path <- file.path(output_dir, 'summary.csv')
write.csv(select(my_df, -links), file = summary_path, row.names = F)

message('\n Details of the PDF retrieval process have been stored in ', summary_path, '\n')