# library("devtools")
# install_github("massimoaria/bibliometrix")
library(bibliometrix) # For reading and analyzing ISI stuff


## *********** ##
## SOIL HEALTH ##
## *********** ##

## Read WoS saved searches
soil.health <- rbind(isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.health_healthy.soil_1to500.bib")),
              isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.health_healthy.soil_501to1000.bib")),
              isibib2df(readFiles("~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/isi_searches/soil.health_healthy.soil_1001to1463.bib")))

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

## Download papers
data <- data.frame(paste(gsub(";.*$", "", soil.health$AU),soil.health$PY,soil.health$JI),soil.health$DI)
names(data) <- c('Name','DOI')
library(metagear)
# nrow(data[which(data$DOI != '<NA>'),]) Of 1463 observations, 1038 have valid DOIs
PDFs_collect(aDataFrame=data,DOIcolumn="DOI",FileNamecolumn="Name",directory="~/Google Drive/SNAPP-Soil-Carbon/Literature/soil_health/")




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




