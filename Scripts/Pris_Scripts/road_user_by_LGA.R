library(tidyverse)
library(tmap)
library(rgdal)
library(data.table)

#read accident data
accident_node <-  read.csv("Datasets/Road Crashes/NODE.CSV")
glimpse(accident_node)

accident <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")
glimpse(accident)

person <- read.csv("Datasets/Road Crashes/PERSON.csv")
glimpse(person)

#Separate day, month and year
accident <- accident %>% mutate(CLEANDATE = as.Date(accident$ACCIDENTDATE,"%d/%m/%y"))
accident <- separate(accident,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

#merge to get LGA and accident
person_by_LGA <- left_join(person,accident_node,by = "ACCIDENT_NO")
person_by_LGA <- left_join(person_by_LGA,accident,by = "ACCIDENT_NO")

#summarise accidents by LGA-----
summary_by_LGA <- ddply(person_by_LGA, .(Lga.Name.All,YEAR, SEX, Road.User.Type.Desc,Inj.Level.Desc), summarise, count = length(ACCIDENT_NO))
glimpse(summary_by_LGA)

summary_by_LGA2 <- summary_by_LGA %>%
	separate(Lga.Name.All,c("VIC_LGA__3","LGA_2"),sep = ",")
glimpse(summary_by_LGA2)

#rename LGA's to summarise
summary_by_LGA2[1:49,"VIC_LGA__3"]<- "FALLS CREEK ALPINE RESORT (UNINC)"
summary_by_LGA2[50:55,"VIC_LGA__3"]<- "FRENCH-ELIZABETH-SANDSTONE ISLANDS (UNINC)"
summary_by_LGA2[56:79,"VIC_LGA__3"]<- "LAKE MOUNTAIN ALPINE RESORT (UNINC)"
summary_by_LGA2[80:100,"VIC_LGA__3"]<- "MOUNT BAW BAW ALPINE RESORT (UNINC)"
summary_by_LGA2[101:141,"VIC_LGA__3"]<- "MOUNT BULLER ALPINE RESORT (UNINC)"
summary_by_LGA2[142:213,"VIC_LGA__3"]<- "MOUNT HOTHAM ALPINE RESORT (UNINC)"
summary_by_LGA2[214:217,"VIC_LGA__3"]<- "MOUNT STIRLING ALPINE RESORT (UNINC)"

summary_by_LGA2 <- summary_by_LGA2 %>% select(-"LGA_2")

summary_by_LGA3 <- ddply(summary_by_LGA2, .(VIC_LGA__3,YEAR,SEX,Road.User.Type.Desc,Inj.Level.Desc), summarise, count_LGA = sum(count))
glimpse(summary_by_LGA3)

#create 2006 subsets to merge with map-----
fatal_LGA_years <-ddply(summary_by_LGA3, .(VIC_LGA__3,YEAR,Road.User.Type.Desc), summarise, fatal = sum(count_LGA))
fatal_2006 <- fatal_LGA_years %>%
	filter(YEAR==2006)

#read the shapefile using the `rgdal` library
fatal_2006_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

#merge LGA to shapefile
fatal_2006 <- fatal_2006 %>%
	spread(key = "Road.User.Type.Desc", value = "fatal")
fatal_2006_map@data <- left_join(fatal_2006_map@data,fatal_2006,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")

tm_shape(fatal_2006_map)+
	tm_polygons(c("Drivers","Motorcyclists","Bicyclists","Passengers","Pillion Passengers","Pedestrians"))+
	tm_layout(main.title = "2006 Accidents - Road User")+
	tm_legend(legend.position = c("right","top"))
