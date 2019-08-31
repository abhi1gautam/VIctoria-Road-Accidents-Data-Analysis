##### Reading Libraries ----
library(tidyverse)
library(esquisse)

##### Reading files ----
accident <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")
climate86018 <- read.csv("Datasets/Climate/86018.csv")

##### Data Transformation ----
#Converting fct to date
accident$ACCIDENTDATE <- as.Date(accident$ACCIDENTDATE, "%d/%m/%Y")
climate86018$YYYY.MM.DD  <- as.Date (climate86018$YYYY.MM.DD, "%Y-%m-%d")

#Renaming date columns
accident <- accident %>% rename (DATE = ACCIDENTDATE)
climate86018 <- climate86018 %>% rename (DATE = YYYY.MM.DD)

#### Merging Data ----
#Creating mergedFiles object, which contains accident and climate data
mergedFiles <- merge(accident, climate86018,by="DATE")