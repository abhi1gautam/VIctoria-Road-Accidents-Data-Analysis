library(readr)
library(tidyverse)
library(leaflet)


crash <- read_csv("Datasets/Road Crashes/ACCIDENT.csv")
NODE <- read_csv("Datasets/Road Crashes/NODE.csv")


test <- crash

tog <- merge(NODE , test ,by="ACCIDENT_NO" )


  leaflet() %>% addTiles() %>%
	addMarkers(data=tog, clusterOptions = markerClusterOptions())




