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


#TR_ROADS - USE THIS ONE--------
#C:\Users\pfspl\Documents\R\STDS\group\Datasets\Shapefiles\SDM644761\ll_gda2020\shape\whole_of_dataset\vic\VMTRANS

TR_ROAD <- readOGR(dsn = ("Datasets/Shapefiles/SDM644761/ll_gda2020/shape/whole_of_dataset/vic/VMTRANS/TR_ROAD.shp"))
TR_ROAD_DF <- as.data.frame(TR_ROAD)

highways <- TR_ROAD_DF %>% 
	filter(ROAD_TYPE == "HIGHWAY")

highways_freeways <- TR_ROAD_DF %>% 
	filter(ROAD_TYPE == "HIGHWAY" | ROAD_TYPE == "FREEWAY")

TR_HIGHWAYS <- TR_ROAD[TR_ROAD@data$ROAD_TYPE == "HIGHWAY" ,]
TR_HIGHWAY_FREEWAY <- TR_ROAD[TR_ROAD@data$ROAD_TYPE == "HIGHWAY" | TR_ROAD@data$ROAD_TYPE == "FREEWAY" ,]

ggplot()+
	geom_path(data = TR_HIGHWAYS1, aes(x=long,y=lat, group = group), colour = "black")

#VICTORIA SHAPEFILE-----
vic_state_map <- readOGR(dsn = ("Datasets/Shapefiles/VIC_STATE_POLYGON/VIC_STATE_POLYGON_shp.shp"))
vic_state_df <- fortify(vic_state_map)

#Only intersecting lines-------
vic_highways <- intersect(TR_HIGHWAYS,vic_state_map)
vic_highways_freeways <- intersect(TR_HIGHWAY_FREEWAY,vic_state_map)

#FATALITIES-----
crash <- read_csv("Datasets/Road Crashes/ACCIDENT.csv")
NODE <- read_csv("Datasets/Road Crashes/NODE.csv")

factor(crash$SEVERITY)

fatal <- merge(NODE , crash ,by="ACCIDENT_NO" )
fatal_1 <- fatal %>% 
	filter(SEVERITY == 1)
fatal_1 <- separate(fatal_1,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

serious <- fatal %>% 
	filter(SEVERITY == 2)
serious <- separate(serious,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

#MAP ALL LAYERS------
ggplot()+
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = "white")+
	geom_point(data = fatal_1, aes(x = Long, y = Lat), colour = "deep pink",size = 0.5)+
	geom_path(data = TR_HIGHWAYS, aes(x=long,y=lat, group = group), colour = "grey")+
	theme_classic()+
	ggtitle("Fatalities Across Victoria - 2006-19")

#Fatalities
ggplot()+
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = "white")+
	geom_point(data = fatal_1, aes(x = Long, y = Lat),colour = "deep pink", show.legend = TRUE, size = 0.5)+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), show.legend = TRUE, colour = "slate gray")+
	theme_classic()+
	ggtitle("Fatalities Across Victoria - 2006-19")+
	coord_equal()+
	labs(x = NULL,
			 y = NULL)

#Serious Injuries
ggplot()+
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = "white")+
	geom_point(data = serious, aes(x = Long, y = Lat), colour = "light sky blue",size = 0.5)+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), colour = "slate gray")+
	theme_classic()+
	ggtitle("Serious Injuries Across Victoria - 2006-19")+
	coord_fixed()

#Extract Highways and Freeways as a shapefile
writeOGR(obj = TR_HIGHWAY_FREEWAY, dsn = "Datasets/Shapefiles", layer = "VIC_HIGHWAYS_FREEWAYS", driver = "ESRI Shapefile")
