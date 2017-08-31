# -----------------------
# Soil health text mining
# -----------------------


# Packages ----
# install packages
# install.packages('tm')        # installs 'tm-Text Mining' package: https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# install.packages('quanteda')  # installs 'quanteda' package: https://github.com/kbenoit/quanteda

# load packages
library('tm')       # loads Text Mining package
library('quanteda') # loads Quanteda package

# Constants ----
pdf_dir <- '/Users/timothy/Documents/soilc-text_mapping/data/pdfs_to_scrub'


## use TM package to read in pdfs
#  create list of pdf files we want to mine
# setwd("~/Google Drive/SNAPP-Soil-Carbon/Products/cross-cutting_projects/soil_health_ecosystem_health/soil_health_references/test")
pdfs <- file.path(pdf_dir, list.files(path = pdf_dir, pattern = 'pdf$'))

# Define function to read PDF files into R as text.
pdfRead <- readPDF(control = list(text= '-layout'))

# Apply function to read in pdf files and coerce them to a TM's VCORPUS
papers <- Corpus(URISource(pdfs), readerControl = list(reader=pdfRead))





# ------------------------
# To apply next
# ------------------------

# coerces the VCorpus object created using TM to a Quanteda Corpus
myCorpus <- corpus(papers)

# visualize corpus structure and contents
summary(myCorpus) 

# add metadata to files, in this case that they are written in english
metadoc(myCorpus, 'language') <- "english" 

# visualize corpus structure and contents, now with added metadata
summary(myCorpus, showmeta = T)

# builds the 'Document-Feature Matrix' (dfm), which is the base object to further explore the texts
myDFM <- dfm(myCorpus,tolower = TRUE, stem = F, remove= c('et', 'al',stopwords('english')), remove_punct = TRUE, remove_numbers=TRUE)

# returns the top frequent words
topfeatures(myDFM, 100) 

# set the seed for wordcloud
set.seed(10)

# plots wordcloud
textplot_wordcloud(myDFM, min.freq=20, random.order=F, rot.per=.10, colors=RColorBrewer::brewer.pal(8,'Dark2')) 

# builds the dictionary of keywords
myDict <- dictionary(list(vegetation=c('forest', 'vegetation', 'trees', 'landscape'), fire= c('fire','burn', 'burned', 'burnt'), climate =c('dry', 'wet', 'cool','cold', 'warm', 'hot')))

# applies the dictonary as an argument when building the document-feature matrix 
byMyDict <- dfm(myCorpus, dictionary = myDict) 

# check the object retuned after applying dictionary
byMyDict 

# search for the keywords in the dictionary, returning the 3 words that are before and after the keyword
KeyWordsInContext <- kwic(myCorpus, myDict, window = 3) 

# check the keywords found and their context
head(KeyWordsInContext)
