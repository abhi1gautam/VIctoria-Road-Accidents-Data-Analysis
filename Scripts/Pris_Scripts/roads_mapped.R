library(tidyverse)
library(rgdal)
library(tmap)
library(raster)

#Australian Data-----
roads <- readOGR(dsn = ("Datasets/Shapefiles/VIC_ROADS/ll_gda94/sde_shape/whole/VIC/VMTRANS/layer/tr_road_infrastructure.shp"))
roads_df <- as.data.frame(roads)
ggplot()+
	geom_point(data = roads_df, aes(x=coords.x1,y=coords.x2), colour = "black", fill = "white", size = 0.1)

roads_int <- roads_df %>% 
	filter(FTYPE_CODE == "int_nosignal")
ggplot()+
	geom_point(data = roads_int, aes(x=coords.x1,y=coords.x2), colour = "black", fill = "white", size = 0.1)

roads_2 <- roads_df %>% 
	filter(FTYPE_CODE == "int_paper")
ggplot()+
	geom_point(data = roads_2, aes(x=coords.x1,y=coords.x2), colour = "black", fill = "white", size = 0.1)

roads_3 <- roads_df %>% 
	filter(FTYPE_CODE == "int_attribute")
ggplot()+
	geom_point(data = roads_3, aes(x=coords.x1,y=coords.x2), colour = "black", fill = "white", size = 0.1)

roads_4 <- roads_df %>% 
	filter(FTYPE_CODE == "int_locality")
ggplot()+
	geom_point(data = roads_4, aes(x=coords.x1,y=coords.x2), colour = "black", fill = "white", size = 0.1)

roads_5 <- roads_df %>% 
	filter(FTYPE_CODE == "int_signal")
ggplot()+
	geom_point(data = roads_5, aes(x=coords.x1,y=coords.x2), colour = "black", fill = "white", size = 0.1)

roads_6 <- roads_df %>% 
	filter(FTYPE_CODE == "road_end")
ggplot()+
	geom_point(data = roads_6, aes(x=coords.x1,y=coords.x2), colour = "black", fill = "white", size = 0.1)

#OES DATA ---------
rd_1 <- readOGR(dsn = ("Datasets/Shapefiles/groads-v1-oceania-east-shp/gROADS-v1-oceania-east.shp"))
rd_df <- as.data.frame(rd_1)
rd_2 <- fortify(rd_1)
rd_3 <- rd_1[rd_1@data$SOURCEID == "s034_0001",]

ggplot()+
	geom_path(data = rd_3, aes(x=long,y=lat, group = group), colour = "black", size = 0.1)


#TR_ROADS--------
#C:\Users\pfspl\Documents\R\STDS\STDS-2019\Datasets\Shapefiles\SDM644761\ll_gda2020\shape\whole_of_dataset\vic\VMTRANS

TR_ROAD <- readOGR(dsn = ("Datasets/Shapefiles/SDM644761/ll_gda2020/shape/whole_of_dataset/vic/VMTRANS/TR_ROAD.shp"))
TR_ROAD_DF <- as.data.frame(TR_ROAD)
roads <- TR_ROAD_DF %>% 
	filter(FTYPE_CODE == "road")
highways <- roads %>% 
	filter(ROAD_TYPE == "HIGHWAY")

TR_HIGHWAYS <- TR_ROAD[TR_ROAD@data$ROAD_TYPE == "HIGHWAY" ,]
TR_HIGHWAYS_1

ggplot()+
	geom_path(data = TR_HIGHWAYS, aes(x=long,y=lat, group = group), colour = "black")

#VICTORIA SHAPEFILE-----
vic_state_map <- readOGR(dsn = ("Datasets/Shapefiles/VIC_STATE_POLYGON/VIC_STATE_POLYGON_shp.shp"))
vic_state_df <- fortify(vic_state_map)


#FATALITIES-----
crash <- read_csv("Datasets/Road Crashes/ACCIDENT.csv")
NODE <- read_csv("Datasets/Road Crashes/NODE.csv")

factor(crash$SEVERITY)

crash$SEVERITY[crash$SEVERITY==1] <- "Fatal Accident"
test$SEVERITY[test$SEVERITY==2] <- "Serious Injury Accident"
test$SEVERITY[test$SEVERITY==3] <- "Minor Injury Accident"
test$SEVERITY[test$SEVERITY==4] <- "No Injury"

fatal <- merge(NODE , crash ,by="ACCIDENT_NO" )
fatal_1 <- fatal %>% 
	filter(SEVERITY == 1)

#MAP ALL LAYERS------
ggplot()+
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = "white")+
	geom_point(data = fatal_1, aes(x = Long, y = Lat), colour = "deep pink",size = 0.5)+
	geom_path(data = TR_HIGHWAYS, aes(x=long,y=lat, group = group), colour = "grey")+
	ggtitle("Fatalities Across Victoria - 2006-19")

###fatalities by year
fatal_2 <- separate(fatal_1,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

tmap_mode ("plot")

glimpse(metro)

tm_shape (vic_state_map)+ tm_polygons()+
	tm_shape(TR_HIGHWAYS) + tm_lines()+
	tm_shape(fatal_2) + tm_dots(size = "SEVERITY")

tm_shape(fatal_2) + tm_dots(size = "SEVERITY")
	