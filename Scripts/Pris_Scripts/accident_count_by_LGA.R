library(tidyverse)
library(tmap)
library(rgdal)

#read accident data
accident_node <-  read.csv("Datasets/Road Crashes/NODE.CSV")
glimpse(accident_node)

#download shapefile
download.file("https://data.gov.au/dataset/bdf92691-c6fe-42b9-a0e2-a4cd716fa811/resource/7b6043d1-76b8-4ea9-b36b-51c61aa740d0/download/vic_lga_polygon_shp.zip", destfile = "VIC_LGA_Boundaries.zip") 
unzip("VIC_LGA_Boundaries.zip")

#read the shapefile using the `rgdal` library
vic_lga_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

#create a data frame to glimpse shape file
VIC_LGA_df <- as.data.frame(vic_lga_map)
glimpse(VIC_LGA_df)

#summarise accidents by LGA
accidents_by_LGA <- ddply(accident_node, .(Lga.Name.All), summarise, accident_count = length(ACCIDENT_NO))
glimpse(accidents_by_LGA)
accident_by_LGA_1 <- accidents_by_LGA %>%
	separate(Lga.Name.All,c("VIC_LGA__3","LGA_2"),sep = ",")

#rename LGA's to summarise
accident_by_LGA_1[1:2,"VIC_LGA__3"]<- "FALLS CREEK ALPINE RESORT (UNINC)"
accident_by_LGA_1[3,"VIC_LGA__3"]<- "FRENCH-ELIZABETH-SANDSTONE ISLANDS (UNINC)"
accident_by_LGA_1[4,"VIC_LGA__3"]<- "LAKE MOUNTAIN ALPINE RESORT (UNINC)"
accident_by_LGA_1[5,"VIC_LGA__3"]<- "MOUNT BAW BAW ALPINE RESORT (UNINC)"
accident_by_LGA_1[6,"VIC_LGA__3"]<- "MOUNT BULLER ALPINE RESORT (UNINC)"
accident_by_LGA_1[7,"VIC_LGA__3"]<- "MOUNT HOTHAM ALPINE RESORT (UNINC)"
accident_by_LGA_1[8,"VIC_LGA__3"]<- "MOUNT STIRLING ALPINE RESORT (UNINC)"

accident_by_LGA_2 <- ddply(accident_by_LGA_1, .(VIC_LGA__3), summarise, accident_by_LGA = sum(accident_count))
glimpse(accident_by_LGA_2)

#merge LGA to shapefile
VIC_LGA_df <- left_join(VIC_LGA_df,accident_by_LGA_2,by= "VIC_LGA__3")
VIC_LGA_df[["accident_by_LGA"]][is.na(VIC_LGA_df[["accident_by_LGA"]])]<- 0


vic_lga_map@data <-left_join(vic_lga_map@data,accident_by_LGA_2,by = "VIC_LGA__3")
head(vic_lga_map)

vic_lga_map@data[["accident_by_LGA"]][is.na(vic_lga_map@data[["accident_by_LGA"]])]<- 0

#quick map
qtm(vic_lga_map, fill = "accident_by_LGA")

