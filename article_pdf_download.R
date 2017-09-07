# ===============================
# PACKAGES
# ===============================
# library("devtools")
# install_github("massimoaria/bibliometrix")
library(tidyverse) # for general purpose data manip
library(bibliometrix) # For reading and analyzing ISI stuff
# install.packages('crminer')
library(crminer) # for getting texts from DOI
library(XML) # for parsing elsevier xml responses
library(rvest) # for parsing html responses
library(jsonlite) # for parsing json objects

# ===============================
# CONSTANTS
# ===============================
data_dir <- '/Users/timothy/Documents/soilc-text_mapping/data'
isi_dir <- file.path(data_dir, 'isi_searches')
pdf_output_dir <- file.path(data_dir, 'pdf_output')


# ===============================
# FUNCTIONS
# ===============================
# elsevier links require a special webscraping process, so I wrote a function for it 
elsevier_pdf_download <- function(elsevier_xml_link, filepath) {
  # This function uses webscraping techniques to download a PDF file from elseviers server
  # 
  # keyword arguments:
  #   elsevier_xml_link-- elsevier link of type `xml` as returned by crminer's crmlinks() function
  #   filepath-- path to desired output location for downloaded PDF
  #
  # return:
  #   none
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


is_binary <- function(filepath,max=1000){
  # This function checks if a file is binary or ASCII (all of our pdfs are binary, and all of our non-pdf files are likely ASCII)
  # source: https://stackoverflow.com/questions/16350164/native-method-in-r-to-test-if-file-is-ascii
  #
  # keyword arguemnts:
  #   filepath-- path to file to be checked
  #   max-- max number of characters in file to check (default 1000)
  #
  # return:
  #   TRUE if binary and FALSE if ASCII
  f=file(filepath,"rb",raw=TRUE)
  b=readBin(f,"int",max,size=1,signed=FALSE)
  close(f)
  return(max(b)>128)
}


# ===============================
# MAIN
# ===============================

## STEP 1: READ DOIs/METADATA FROM BIB FILES

soil.health <- convert2df(readFiles(file.path(isi_dir, 'healthy.rangelands.bib'),
                                    file.path(isi_dir, 'rangeland_SAME_soil.health.bib'),
                                    file.path(isi_dir, 'rangeland.health.bib'),
                                    file.path(isi_dir, 'soil.health_healthy.soil_1to500.bib'),
                                    file.path(isi_dir, 'soil.health_healthy.soil_501to1000.bib'),
                                    file.path(isi_dir, 'soil.health_healthy.soil_1001to1463.bib'),
                                    file.path(isi_dir, 'soil.quality_1to500.bib'),
                                    file.path(isi_dir, 'soil.quality_501to1000.bib'),
                                    file.path(isi_dir, 'soil.quality_1001to1500.bib'),
                                    file.path(isi_dir, 'soil.quality_1501to2000.bib'),
                                    file.path(isi_dir, 'soil.quality_2001to2500.bib'),
                                    file.path(isi_dir, 'soil.quality_2501to3000.bib'),
                                    file.path(isi_dir, 'soil.quality_3001to3500.bib'),
                                    file.path(isi_dir, 'soil.quality_3501to4000.bib'),
                                    file.path(isi_dir, 'soil.quality_4001to4500.bib'),
                                    file.path(isi_dir, 'soil.quality_4501to5000.bib'),
                                    file.path(isi_dir, 'soil.quality_5001to5500.bib'),
                                    file.path(isi_dir, 'soil.quality_5501to6000.bib'),
                                    file.path(isi_dir, 'soil.quality_6001to6500.bib'),
                                    file.path(isi_dir, 'soil.quality_6501to7000.bib'),
                                    file.path(isi_dir, 'soil.quality_7001to7471.bib')
))
soil.health <- convert2df(sapply(file.path(data_dir, dir(isi_dir)), readFiles))
# removing duplicate records
soil.health <- duplicatedMatching(soil.health,Field="TI") 


## STEP 2: ORGANIZE LINKS
message('===============================\nORGANIZING LINKS\n===============================')
my_df <- data.frame(paste(gsub(";.*$", "", soil.health$AU),soil.health$PY,soil.health$JI),soil.health$DI, stringsAsFactors = FALSE)
names(my_df) <- c('Name','DOI')
my_df <- my_df[!is.na(my_df$DOI),] # Of 1460 observations, 1037 have valid DOIs
# collecting links
my_df$links <- sapply(my_df$DOI, crm_links) # getting links for each DOI
my_df <- my_df[lapply(my_df$links, length) > 0,] # 929 of 1037 DOIs returned links via crm_links()


# elsevier links require a separate dl process, so we distinguish them here
for (i in 1:length(my_df$links)) {
  if (grepl('elsevier',my_df$link[[i]])) { # checking for string 'elsevier' in link
    my_df$elsevier[i] <- TRUE
  } else {
    my_df$elsevier[i] <- FALSE
  }
}

# selecting a single link for each DOI (up until now, there has been a list of links assoc. to each DOI)
for (i in 1:dim(my_df)[1]) {
  if (my_df$elsevier[i]) { # if it's from elsevier, we want to get the xml link
    link <- as.character(my_df$links[[i]]$xml)
  } else if ('pdf' %in% names(my_df$links[[i]])) { # otherwise, we prefer the 'pdf' link type
    link <- my_df$links[[i]]$pdf
  } else { # our last preference is the 'unspecified' link type
    link <- my_df$links[[i]]$unspecified
  }
  my_df$link[i] <- unlist(link)
}
my_df$link <- unlist(my_df$link)


## STEP 3: DOWNLOAD PDFS FROM LINKS
message('===============================\nDOWNLOADING PDFS FROM LINKS\n===============================')

# Here, I call the elsevier_pdf_download() function repeatedly via a loop that iterates through the rows of the dataframe created in the preceding
# 'organize links' section of the script
system.time(for (i in 1:dim(my_df)[1]) {
  url <- my_df$link[i]
  my_df$path[i] <- paste0(file.path(pdf_output_dir, my_df$Name[i]), '.pdf')
  if (my_df$elsevier[i]) {
    # DOWNLOADING ELSEVIER LINKS
    my_df$downloaded[i] <- tryCatch(elsevier_pdf_download(url, my_df$path[i]),
                                      error=function(cond) {
                                        message(cond)
                                        return(1)
                                      }, 
                                      finally = message(paste("Processed URL:", url)))
  } else {
    # DOWNLOADING OTHER LINKS
    my_df$downloaded[i] <- tryCatch(download.file(url, my_df$path[i]),
                                 error=function(cond) {
                                   message(cond)
                                   return(1)
                                 }, 
                                 finally = message(paste("Processed URL:", url)))
  }
  message('[', i, '/', dim(my_df[1]), ']')
})

message('===============================\nPDFS DOWNLOADED\n===============================')

# my_df$pdf <- sapply(my_df$path, is_binary) #

# removing non pdf files
# for (i in 1:dim(my_df)[1]) {
#   if (!my_df$downloaded) {
#     print(my_df$path[i])
#   }
# }

summary_path <- file.path(pdf_output_dir, 'summary.csv')
write.csv(my_df, file = summary_path, row.names = F)

# message('\nYou have downloaded ', sum(my_df$pdf), ' pdfs to ', pdf_output_dir)
message('\n Details of the pdf retrieval process have been stored in ', summary_path, '\n')

# ===============================
# DEPRECATED
# ===============================


# links <- data.frame()
# 
# length(has_pdf) # 560 out of 1037 have pdf links
# data_with_pdf <- my_df[has_pdf,]
# elsevier <- links[-has_pdf] # need to examine to understand if these links are of any value; if they aren't then why not?
# 
# # creating df of non-pdf links (links to be discarded)
# no_pdf <- data.frame()
# for (i in 1:length(links_without_pdf)) {
#   doi <- names(links_without_pdf[i])
#   links <- if (length(links_without_pdf[[i]]) == 0) 'NA' else unlist(links_without_pdf[[i]])
#   print(i)
#   no_pdf <- rbind(no_pdf, cbind(doi, links))
# }
# write.csv(no_pdf, file = file.path(data_dir, 'no_pdf.csv'), row.names = F)
# 
# 
# has_xml <- c()
# elsevier <- my_df$links[my_df$elsevier]
# for (i in 1:length(elsevier)) {
#   has_xml <- c(has_xml, ('xml' %in% names(elsevier[[i]])))
# }
# 
# 
# 
# 
# pdf_links <- c()
# for (links in data_with_pdf$links) {
#   if ('pdf' %in% names(links)) {
#     pdf_link <- links$pdf
#   } else {
#     pdf_link <- links$unspecified
#   }
#   pdf_links <- c(pdf_links, pdf_link)
# }
# 
# data_with_pdf$pdf_link <- unlist(pdf_links) # THIS OBJECT CONTAINS A LIST OF DIRECT LINKS TO PDFS (with high probability)
# write.csv(data_with_pdf[,c(1, 2, 4)], file = file.path(data_dir, 'pdf_info.csv'), row.names = F)
# 
# pdf_text <- list()
# failure <- c()
# for (i in 1:length(data_with_pdf)) {
#   filename <- str_replace_all(data_with_pdf$Name[i], ' ', '_') %>%
#     str_replace_all('\\.', '') %>%
#     paste('.pdf', sep = '')
#   pdf_outpath <- file.path(pdf_output_dir, filename)
#   dl_fail <- download.file(url = pdf_links[[i]], destfile = pdf_outpath)
#   failure <- c(failure, dl_fail)
# }
# 
# # PDFs_collect(aDataFrame=my_df,DOIcolumn="DOI",FileNamecolumn="Name",directory=pdf_output_dir)
