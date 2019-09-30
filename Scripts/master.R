library (tidyverse)
library (readxl)
library (sqldf)
library (dplyr)
library (lubridate)
library(geosphere)
library(tibble)
library(plyr)
library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)
library(sp)
library(rgdal)

# Reading and Transform datas ----
#Accidents Files
source("Scripts/accidents.R")
#Population data
source("Scripts/population.R")
#Rainfall data
for (i in 2006:2019) {
	source("Scripts/Reading & Exporting Rainfall data.R")
}


#Merging Datasets ----
source("Scripts/merging accident CSV files.R")
