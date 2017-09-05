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


# ===============================
# MAIN
# ===============================

## STEP 1: READ DOIs/METADATA FROM BIB FILES

soil.health <- convert2df(readFiles(file.path(isi_dir, 'soil.health_healthy.soil_1to500.bib'),
                                    file.path(isi_dir, 'soil.health_healthy.soil_501to1000.bib'),
                                    file.path(isi_dir, 'soil.health_healthy.soil_1001to1463.bib')))
# removing duplicate records
soil.health <- duplicatedMatching(soil.health,Field="TI") 


## STEP 2: ORGANIZE LINKS

my_df <- data.frame(paste(gsub(";.*$", "", soil.health$AU),soil.health$PY,soil.health$JI),soil.health$DI, stringsAsFactors = FALSE)
names(my_df) <- c('Name','DOI')
my_df <- my_df[!is.na(my_df$DOI),] # Of 1460 observations, 1037 have valid DOIs
# collecting links
my_df$links <- sapply(my_df$DOI, crm_links) # getting links for each DOI
my_df <- my_df[lapply(my_df$links, length) > 0,] # 929 of 1037 DOIs returned links via crm_links()

# creating a logical vector to be used to index those documents which have link types 'pdf' or 'unspecified' 
#   elsevier links dont have pdf or unspecified links offered; they require a separate dl process, so we distinguish here
for (i in 1:length(my_df$links)) {
  link_types <- names(my_df$links[[i]])
  if (as.logical(sum(c('pdf', 'unspecified') %in% link_types))) {
    my_df$elsevier[i] <- FALSE
  } else {
    my_df$elsevier[i] <- TRUE
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

# DOWNLOADING ELSEVIER LINKS
# Here, I call the elsevier_pdf_download() function repeatedly via a loop that iterates through the rows of the dataframe created in the preceding
# 'organize links' section of the script
system.time(for (i in 1:dim(my_df)[1]) {
  if (my_df$elsevier[i]) {
    url <- my_df$link[i]
    outpath <- paste0(file.path(pdf_output_dir, my_df$Name[i]), '.pdf')
    my_df$downloaded <- tryCatch(elsevier_pdf_download(url, outpath),
                                      error=function(cond) {
                                        message(cond)
                                        return(1)
                                      }, 
                                      finally = message(paste("Processed URL:", url)))
  }
})

# DOWNLOADING OTHER LINKS


# ===============================
# TESTING
# ===============================

# 
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
# PDFs_collect(aDataFrame=my_df,DOIcolumn="DOI",FileNamecolumn="Name",directory=pdf_output_dir)


