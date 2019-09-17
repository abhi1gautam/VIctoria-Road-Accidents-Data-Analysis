##### Reading Libraries ----
library(tidyverse)
library(esquisse)
library(geosphere)
library(tibble)
##### Reading files ----
accident <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")
accident_node <-  read.csv("Datasets/Road Crashes/NODE.CSV")


##### Data Transformation ----
#Converting fct to date
accident$ACCIDENTDATE <- as.Date(accident$ACCIDENTDATE, "%d/%m/%Y")

#Renaming date columns (to be used as primary key btw accidents and climate data)
accident <- accident %>% rename (DATE = ACCIDENTDATE)

#### Merging Data ----
#Creating mergedFiles object, which contains accident, accident location and climate data
mergedFiles <- merge(accident,accident_node,by="ACCIDENT_NO")

mergedFiles$Lat <-  round(mergedFiles$Lat, 2)
mergedFiles$Lat <-  0.05*(round(mergedFiles$Lat/0.05))

mergedFiles$Long <-  round(mergedFiles$Long, 2)
mergedFiles$Long <-  0.05*(round(mergedFiles$Long/0.05))


mergedFiles <- merge(mergedFiles, daily_rain.df2017, by=c("Lat","Long","DATE"),all.x = TRUE)
