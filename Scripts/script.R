##### Reading Libraries ----
library(tidyverse)
library(esquisse)
library(geosphere)
library(tibble)
library(sqldf)
library(plyr)
##### Reading files ----
accident <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")
accident_node <-  read.csv("Datasets/Road Crashes/NODE.CSV")


##### Data Transformation ----
#Converting fct to date
accident$ACCIDENTDATE <- as.Date(accident$ACCIDENTDATE, "%d/%m/%Y")

#Renaming date columns (to be used as primary key btw accidents files)
accident <- accident %>% rename (DATE = ACCIDENTDATE)

#### Merging Data ----
#Creating mergedFiles object, which contains accident and accident location
mergedFiles <- merge(accident,accident_node[!duplicated(accident_node$ACCIDENT_NO), ],by="ACCIDENT_NO")

### Rounding latitude and longitude to 2 decimal places in order to merge with rainfall dataset
mergedFiles$Lat <-  round(mergedFiles$Lat, 2)
mergedFiles$Lat <-  0.05*(round(mergedFiles$Lat/0.05))
mergedFiles$Long <-  round(mergedFiles$Long, 2)
mergedFiles$Long <-  0.05*(round(mergedFiles$Long/0.05))

#Merging the Accident tables with the rainfall data
mergedFiles <- merge(mergedFiles, daily_rain.df2019, by=c("Lat","Long","DATE"),all.x = TRUE)
mergedFiles <- mergedFiles %>% rename (Rainfall2019 = Rainfall)

# Combining all rainfall data in one column
mergedFiles$Rainfall <- coalesce(mergedFiles$Rainfall2006,mergedFiles$Rainfall2007,mergedFiles$Rainfall2008, mergedFiles$Rainfall2009, mergedFiles$Rainfall2010, mergedFiles$Rainfall2011, mergedFiles$Rainfall2012, mergedFiles$Rainfall2013, mergedFiles$Rainfall2014, mergedFiles$Rainfall2015, mergedFiles$Rainfall2016, mergedFiles$Rainfall2017, mergedFiles$Rainfall2018, mergedFiles$Rainfall2019)

# Export to CSV
acccsvfile <- "Accident_with_Rainfall"
write.table(mergedFiles, acccsvfile, row.names = FALSE, sep = ",")


 