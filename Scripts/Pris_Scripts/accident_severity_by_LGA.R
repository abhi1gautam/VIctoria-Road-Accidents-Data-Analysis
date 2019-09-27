library(tidyverse)
library(tmap)
library(rgdal)

#read accident data
accident_node <-  read.csv("Datasets/Road Crashes/NODE.CSV")
glimpse(accident_node)

accident <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")
glimpse(accident)

#Separate day, month and year
accident <- accident %>% mutate(CLEANDATE = as.Date(accident$ACCIDENTDATE,"%d/%m/%y"))
accident <- separate(accident,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

#merge to get LGA and accident
death_by_LGA <- left_join(accident,accident_node,by = "ACCIDENT_NO")


#download shapefile
download.file("https://data.gov.au/dataset/bdf92691-c6fe-42b9-a0e2-a4cd716fa811/resource/7b6043d1-76b8-4ea9-b36b-51c61aa740d0/download/vic_lga_polygon_shp.zip", destfile = "VIC_LGA_Boundaries.zip") 
unzip("VIC_LGA_Boundaries.zip")

#read the shapefile using the `rgdal` library
vic_lga_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

#create a data frame to glimpse shape file
VIC_LGA_df <- as.data.frame(vic_lga_map)
glimpse(VIC_LGA_df)

#summarise accidents by LGA
death_by_LGA1 <- ddply(death_by_LGA, .(Lga.Name.All), summarise, death_count = sum(NO_PERSONS_KILLED), injury_2 = sum(NO_PERSONS_INJ_2))
glimpse(death_by_LGA1)
death_by_LGA2 <- death_by_LGA1 %>%
	separate(Lga.Name.All,c("VIC_LGA__3","LGA_2"),sep = ",")

#rename LGA's to summarise
death_by_LGA2[1:2,"VIC_LGA__3"]<- "FALLS CREEK ALPINE RESORT (UNINC)"
death_by_LGA2[3,"VIC_LGA__3"]<- "FRENCH-ELIZABETH-SANDSTONE ISLANDS (UNINC)"
death_by_LGA2[4,"VIC_LGA__3"]<- "LAKE MOUNTAIN ALPINE RESORT (UNINC)"
death_by_LGA2[5,"VIC_LGA__3"]<- "MOUNT BAW BAW ALPINE RESORT (UNINC)"
death_by_LGA2[6,"VIC_LGA__3"]<- "MOUNT BULLER ALPINE RESORT (UNINC)"
death_by_LGA2[7,"VIC_LGA__3"]<- "MOUNT HOTHAM ALPINE RESORT (UNINC)"
death_by_LGA2[8,"VIC_LGA__3"]<- "MOUNT STIRLING ALPINE RESORT (UNINC)"

death_by_LGA_3 <- ddply(death_by_LGA2, .(VIC_LGA__3), summarise, death_LGA = sum(death_count), inj_2 = sum(injury_2))
glimpse(death_by_LGA_3)

#merge LGA to shapefile
vic_lga_map2 <- vic_lga_map
VIC_LGA_df2 <- left_join(VIC_LGA_df,death_by_LGA_3,by= "VIC_LGA__3")
VIC_LGA_df2[c("death_LGA","inj_2")][is.na(VIC_LGA_df2[c("death_LGA","inj_2")])]<- 0
glimpse(VIC_LGA_df2)


vic_lga_map2@data <-left_join(vic_lga_map2@data,death_by_LGA_3,by = "VIC_LGA__3")
glimpse(vic_lga_map)

vic_lga_map2@data[["death_LGA"]][is.na(vic_lga_map@data[["death_LGA"]])]<- 0
vic_lga_map2@data[["inj_2"]][is.na(vic_lga_map@data[["inj_2"]])]<- 0

#quick map
qtm(vic_lga_map2, fill = "death_LGA")
qtm(vic_lga_map2, fill = "inj_2")
