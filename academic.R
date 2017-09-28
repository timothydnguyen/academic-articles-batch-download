# library("devtools")
# install_github("massimoaria/bibliometrix")
library(tidyverse) # for general purpose data manip
library(bibliometrix) # For reading and analyzing ISI stuff
# install.packages('crminer')
library(crminer) # for getting texts from DOI
library(XML) # for parsing elsevier xml responses
library(rvest) # for parsing html responses
library(jsonlite) # for parsing json objects
library(metagear)


# Constants ----
data_dir <- '/Users/timothy/Documents/soilc-text_mapping/data'
isi_dir <- file.path(data_dir, 'isi_searches')
pdf_output_dir <- file.path(data_dir, 'pdf_output')


## *********** ##
## SOIL HEALTH ##
## *********** ##

## Read WoS saved searches
# readFiles() actually takes multiple files as input so i rewrote this part accordingly
soil.health <- convert2df(readFiles(file.path(isi_dir, 'soil.health_healthy.soil_1to500.bib'),
                                    file.path(isi_dir, 'soil.health_healthy.soil_501to1000.bib'),
                                    file.path(isi_dir, 'soil.health_healthy.soil_1001to1463.bib')))
# soil.health <- rbind(isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.health_healthy.soil_1to500.bib")),
#               isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.health_healthy.soil_501to1000.bib")),
#               isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.health_healthy.soil_1001to1463.bib")))


## Clean data
soil.health <- duplicatedMatching(soil.health,Field="TI")

## Basic bibliographic analysis
sh.biblio <- biblioAnalysis(soil.health, sep = ";")
#summary(basic.biblio.results, pause = FALSE)
plot(sh.biblio, pause = F)

## What were the earliest papers?
soil.health <- soil.health[order(soil.health$PY),]

## Are there key cited papers?
# Create a co-citation network
ref.co.cite.network <- biblioNetwork(soil.health, analysis = "co-citation", network = "references", sep=".  ")
# # Normalize network for papers with different numbers of references
S <- normalizeSimilarity(ref.co.cite.network)
# Plot network
networkPlot(ref.co.cite.network, n = 20, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=T)

## Co-occurrences among keywords
sh.keywords <- biblioNetwork(soil.health, analysis = "co-occurrences", network = "keywords", sep = ";")
networkPlot(sh.keywords, n = 20, Title = "Keyword Co-occurrences", type = "kamada", size=T)

## Abstract term co-occurences
sh.abstract <- biblioNetwork(termExtraction(soil.health,Field="AB"), analysis = "co-occurrences", network = "abstracts")
networkPlot(sh.abstract, n = 20, Title = "Abstract Co-occurrences", type = "kamada", size=T)

## Conceptual map
CS <- conceptualStructure(termExtraction(soil.health,Field="AB"))

## Divide up into time analysis
time.data <- timeslice(soil.health,breaks=c(1990,2000,2010))

# ===============================
# organize links
# ===============================

data <- data.frame(paste(gsub(";.*$", "", soil.health$AU),soil.health$PY,soil.health$JI),soil.health$DI, stringsAsFactors = FALSE)
names(data) <- c('Name','DOI')
data_valid <- data[!is.na(data$DOI),] # Of 1460 observations, 1037 have valid DOIs

data_valid$links <- sapply(data_valid$DOI, crm_links) # getting links for each DOI
data_valid <- data_valid[lapply(data_valid$links, length) > 0,] # 929 of 1037 DOIs returned links via crm_links()


# creating a logical vector to be used to index those documents which have link types 'pdf' or 'unspecified' 
#   elsevier links dont have pdf or unspecified links offered; they require a separate dl process, so we distinguish here
for (i in 1:length(data_valid$links)) {
  link_types <- names(data_valid$links[[i]])
  if (as.logical(sum(c('pdf', 'unspecified') %in% link_types))) {
    data_valid$elsevier[i] <- FALSE
  } else {
    data_valid$elsevier[i] <- TRUE
  }
}

# creating single object (df) to feed into download process
# selecting a single link for each DOI (up until now, there has been a list of links assoc. to each DOI)
for (i in 1:dim(data_valid)[1]) {
  if (data_valid$elsevier[i]) {
    link <- as.character(data_valid$links[[i]]$xml)
  } else if ('pdf' %in% names(data_valid$links[[i]])) {
    link <- data_valid$links[[i]]$pdf
  } else {
    link <- data_valid$links[[i]]$unspecified
  }
  data_valid$link[i] <- unlist(link)
}
data_valid$link <- unlist(data_valid$link)

# ===============================
# download PDFs from links
# ===============================
## elsevier links require a special webscraping process, so I wrote a function for it
### ARGUMENTS
#### elsevier_xml_link is an elsevier link of type 'xml' as returned by crminer
#### filepath is a character string representation of the desired output location
elsevier_pdf_download <- function(elsevier_xml_link, filepath) {
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
    html_node('head') %>% within
    html_nodes('meta[http-equiv="Refresh"]') %>% 
    html_attr('content') %>% # 
    substr(7, 100000000L) %>% # the first 6 chars are not part of the url we want, so we slice them off
    download.file(filepath) # download pdf to provided filepath
}

## Here, I call the function repeatedly via a loop that iterates through the rows of the dataframe created in the preceding
##  'organize links' section of the script
system.time(for (i in 1:dim(data_valid)[1]) {
  if (data_valid$elsevier[i]) {
    url <- data_valid$link[i]
    outpath <- file.path(pdf_output_dir, data_valid$Name[i])
    data_valid$downloaded <- tryCatch(elsevier_pdf_download(url, outpath),
                                      error=function(cond) {
                                        message(cond)
                                        return(1)
                                      }, 
                                      finally = message(paste("Processed URL:", url)))
  }
})


links <- data.frame()

length(has_pdf) # 560 out of 1037 have pdf links
data_valid_with_pdf <- data_valid[has_pdf,]
elsevier <- links[-has_pdf] # need to examine to understand if these links are of any value; if they aren't then why not?

# creating df of non-pdf links (links to be discarded)
no_pdf <- data.frame()
for (i in 1:length(links_without_pdf)) {
  doi <- names(links_without_pdf[i])
  links <- if (length(links_without_pdf[[i]]) == 0) 'NA' else unlist(links_without_pdf[[i]])
  print(i)
  no_pdf <- rbind(no_pdf, cbind(doi, links))
}
write.csv(no_pdf, file = file.path(data_dir, 'no_pdf.csv'), row.names = F)


has_xml <- c()
elsevier <- data_valid$links[data_valid$elsevier]
for (i in 1:length(elsevier)) {
  has_xml <- c(has_xml, ('xml' %in% names(elsevier[[i]])))
}




pdf_links <- c()
for (links in data_valid_with_pdf$links) {
  if ('pdf' %in% names(links)) {
    pdf_link <- links$pdf
  } else {
    pdf_link <- links$unspecified
  }
  pdf_links <- c(pdf_links, pdf_link)
}

data_valid_with_pdf$pdf_link <- unlist(pdf_links) # THIS OBJECT CONTAINS A LIST OF DIRECT LINKS TO PDFS (with high probability)
write.csv(data_valid_with_pdf[,c(1, 2, 4)], file = file.path(data_dir, 'pdf_info.csv'), row.names = F)

pdf_text <- list()
failure <- c()
for (i in 1:length(data_valid_with_pdf)) {
  filename <- str_replace_all(data_valid_with_pdf$Name[i], ' ', '_') %>% 
    str_replace_all('\\.', '') %>%
    paste('.pdf', sep = '')
  pdf_outpath <- file.path(pdf_output_dir, filename)
  dl_fail <- download.file(url = pdf_links[[i]], destfile = pdf_outpath)
  failure <- c(failure, dl_fail)
}

PDFs_collect(aDataFrame=data_valid,DOIcolumn="DOI",FileNamecolumn="Name",directory=pdf_output_dir)











## ************ ##
## SOIL QUALITY ##
## ************ ##

## Read WoS saved searches
soil.quality <- rbind(isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_1to500.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_501to1000.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_1001to1500.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_1501to2000.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_2001to2500.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_2501to3000.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_3001to3500.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_3501to4000.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_4001to4500.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_4501to5000.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_5001to5500.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_5501to6000.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_6001to6500.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_6501to7000.bib")),
                      isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.quality_7001to7471.bib")))

## Read and clean data
soil.quality <- duplicatedMatching(soil.quality,Field="TI")

## Basic bibliographic analysis
sq.biblio <- biblioAnalysis(soil.quality, sep = ";")
#summary(basic.biblio.results, pause = FALSE)
plot(sq.biblio, pause = F)

## What were the earliest papers?
soil.quality <- soil.quality[order(soil.quality$PY),]

## Are there key cited papers?
# Create a co-citation network
ref.co.cite.network <- biblioNetwork(soil.quality, analysis = "co-citation", network = "references", sep=".  ")
# # Normalize network for papers with different numbers of references
# S <- normalizeSimilarity(ref.co.cite.network)
# Plot network
networkPlot(ref.co.cite.network, n = 20, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=T)

## Co-occurrences among keywords
sq.keywords <- biblioNetwork(soil.quality, analysis = "co-occurrences", network = "keywords", sep = ";")
networkPlot(sq.keywords, n = 10, Title = "Keyword Co-occurrences", type = "kamada", size=T)

## Abstract term co-occurences
sq.abstract <- biblioNetwork(termExtraction(soil.quality,Field="AB"), analysis = "co-occurrences", network = "abstracts")
networkPlot(sq.abstract, n = 20, Title = "Abstract Co-occurrences", type = "kamada", size=T)


## Divide up into time analysis
time.data <- timeslice(soil.quality,breaks=c(1990,2000,2010))
plot(biblioAnalysis(time.data[[1]], sep = ";"),pause=F)
prenineties <- networkPlot(biblioNetwork(time.data[[1]], analysis = "co-occurrences", network = "keywords", sep = ";"),n=15,Title = "Pre-90s Keyword Co-occurrences", type = "kamada", size=T)
nineties <- networkPlot(biblioNetwork(time.data[[2]], analysis = "co-occurrences", network = "keywords", sep = ";"),n=15,Title = "90s Keyword Co-occurrences", type = "kamada", size=T)
oughts <- networkPlot(biblioNetwork(time.data[[3]], analysis = "co-occurrences", network = "keywords", sep = ";"),n=15,Title = "2000s Keyword Co-occurrences", type = "kamada", size=T)
tens <- networkPlot(biblioNetwork(time.data[[4]], analysis = "co-occurrences", network = "keywords", sep = ";"),n=15,Title = "2010s Keyword Co-occurrences", type = "kamada", size=T)


## **************** ##
## RANGELAND HEALTH ##
## **************** ##

## Read WoS saved searches
rangeland.health <- rbind(isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/rangeland.health.bib")),
                     isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/rangeland_SAME_soil.health.bib")),
                     isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/healthy.rangelands.bib")))

## Clean data
rangeland.health <- duplicatedMatching(rangeland.health,Field="TI")

## Basic bibliographic analysis
rh.biblio <- biblioAnalysis(rangeland.health, sep = ";")
summary(rh.biblio, pause = FALSE)
plot(rh.biblio, pause = F)

## What were the earliest papers?
rangeland.health <- rangeland.health[order(rangeland.health$PY),]


## Are there key cited papers?
# Create a co-citation network
rh.cite.network <- biblioNetwork(rangeland.health, analysis = "co-citation", network = "references", sep=".  ")
# # Normalize network for papers with different numbers of references
S <- normalizeSimilarity(rh.cite.network)
# Plot network
networkPlot(rh.cite.network, n = 20, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=T)


## Co-occurrences among keywords
rh.keyword <- biblioNetwork(rangeland.health, analysis = "co-occurrences", network = "keywords", sep = ";")
networkPlot(rh.keyword, n = 20, Title = "Keyword Co-occurrences", type = "kamada", size=T)

## Abstract term co-occurences
rh.abstract <- biblioNetwork(termExtraction(rangeland.health,Field="AB"), analysis = "co-occurrences", network = "abstracts")
networkPlot(rh.abstract, n = 20, Title = "Abstract Co-occurrences", type = "kamada", size=T)


# Conceptual map
CS <- conceptualStructure(termExtraction(rangeland.health,Field="AB"))




