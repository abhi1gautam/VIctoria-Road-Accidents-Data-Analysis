library(tidyverse)
library(data.table)
library(plyr)
library(tmap)

accident <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")
accident_location <- read.csv("Datasets/Road Crashes/ACCIDENT_LOCATION.csv")
accident_node <-  read.csv("Datasets/Road Crashes/NODE.CSV")

accident <- separate(accident,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

road_type <- left_join(accident,accident_location,by = "ACCIDENT_NO")
road_type <- left_join(road_type,accident_node, by = "ACCIDENT_NO")
glimpse(road_type)

road_type2 <- ddply(road_type, .(Lga.Name.All,ROAD_TYPE), summarise, count = length(ACCIDENT_NO))
glimpse(road_type2)

road_type3 <- road_type2 %>%
	separate(Lga.Name.All,c("VIC_LGA__3","LGA_2"),sep = ",")

road_type3[1:4,"VIC_LGA__3"]<- "FALLS CREEK ALPINE RESORT (UNINC)"
road_type3[5,"VIC_LGA__3"]<- "FRENCH-ELIZABETH-SANDSTONE ISLANDS (UNINC)"
road_type3[6,"VIC_LGA__3"]<- "LAKE MOUNTAIN ALPINE RESORT (UNINC)"
road_type3[7,"VIC_LGA__3"]<- "MOUNT BAW BAW ALPINE RESORT (UNINC)"
road_type3[8:9,"VIC_LGA__3"]<- "MOUNT BULLER ALPINE RESORT (UNINC)"
road_type3[10:11,"VIC_LGA__3"]<- "MOUNT HOTHAM ALPINE RESORT (UNINC)"
road_type3[12,"VIC_LGA__3"]<- "MOUNT STIRLING ALPINE RESORT (UNINC)"

road_type3 <- road_type3 %>%
	select(-LGA_2)
glimpse(road_type3)

road_type4 <- ddply(road_type3, .(VIC_LGA__3,ROAD_TYPE), summarise, count = sum(count))
glimpse(road_type4)

road_type5 <- road_type4 %>% 
	spread(key = "ROAD_TYPE",value = "count")

glimpse(road_type5)

road_type_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

road_type_map@data <- left_join(road_type_map@data,road_type5,by = "VIC_LGA__3")

tmap_mode ("plot")
tm_shape(road_type_map)+
	tm_polygons("HIGHWAY")+
	tm_layout(main.title = "Accident Highway")+
	tm_legend(legend.position = c("right","top"))

road_type_missing <- ddply(road_type, .(ROAD_TYPE), summarise, count = length(ACCIDENT_NO))

#Road Route Number--------
accident <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")
accident_location <- read.csv("Datasets/Road Crashes/ACCIDENT_LOCATION.csv")
accident_node <-  read.csv("Datasets/Road Crashes/NODE.CSV")

accident <- separate(accident,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

road_type <- left_join(accident,accident_location,by = "ACCIDENT_NO")
road_type <- left_join(road_type,accident_node, by = "ACCIDENT_NO")
glimpse(road_type)

road_route1 <- road_type %>%
	filter(ROAD_ROUTE_1 >2000) %>% 
	filter(ROAD_ROUTE_1<3000)

road_route2 <- ddply(road_route1, .(Lga.Name.All), summarise, count = length(ACCIDENT_NO))
glimpse(road_route2)

road_route3 <- road_route2 %>%
	separate(Lga.Name.All,c("VIC_LGA__3","LGA_2"),sep = ",")

road_route4 <- road_route3 %>%
	select(-LGA_2)
glimpse(road_route4)

road_route5 <- ddply(road_route4, .(VIC_LGA__3), summarise, count = sum(count))
glimpse(road_route5)

road_route_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

road_route_map@data <- left_join(road_route_map@data,road_route5,by = "VIC_LGA__3")

tmap_mode ("plot")
tm_shape(road_route_map)+
	tm_polygons("count")+
	tm_layout(main.title = "Accident Highway")+
	tm_legend(legend.position = c("right","top"))
