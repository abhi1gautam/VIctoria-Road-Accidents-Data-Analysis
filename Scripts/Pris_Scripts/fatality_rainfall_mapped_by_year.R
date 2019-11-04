library(viridis)
library (dplyr)
library (tidyverse)
library(rgdal)

#2018------------
rain2018 <- read.csv("Datasets/Climate/daily_rain.dfVictoria2018.csv")

#Creating a new object to get the avg rainfall values per LONG and LAT 
rain_avg_2018 <- rain2018  %>%
	group_by(LONG,LAT) %>%
	dplyr::summarize(Mean = mean(RAINFALL))

#Plotting the lats and longs values with the mean value of rainfall
ggplot() + geom_raster(data = rain_avg_2018, aes(x=rain_avg_2018$LONG, y = rain_avg_2018$LAT, fill=Mean)) + 
	coord_fixed(ratio = 1) +
	scale_fill_gradientn(colours=c("#FFFFFFFF","#0000FFFF")) +
	theme_bw()

#read shapefile for highways and freeways + the State of victoria
library(rgdal)
vic_highway <- readOGR(dsn = "Datasets/Shapefiles/VIC_HIGHWAYS_FREEWAYS.shp")
vic_state_map <- readOGR(dsn = ("Datasets/Shapefiles/VIC_STATE_POLYGON/VIC_STATE_POLYGON_shp.shp"))

#change polygon shapefiles for reading into ggplots
library(raster)
vic_state_df <- fortify(vic_state_map)

#Only include roads within the victorian state border - as a spatial line data frame
vic_highways_freeways <- intersect(vic_highway,vic_state_map)

#plotting roads and state
ggplot()+
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = "white")+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), colour = "slate gray")+
	theme_classic()+
	coord_equal()+
	labs(x = NULL,
			 y = NULL)

#read fatalities
crash <- read_csv("Datasets/Road Crashes/ACCIDENT.csv")
NODE <- read_csv("Datasets/Road Crashes/NODE.csv")

fatal <- left_join(crash , NODE ,by="ACCIDENT_NO" )
fatal_1 <- fatal

	filter(SEVERITY == 1| SEVERITY ==2)
fatal_1 <- separate(fatal_1,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

non_fatal <- fatal %>% 
	filter(SEVERITY == 3| SEVERITY ==4)
non_fatal <- separate(non_fatal,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

#plot fatalities
ggplot()+
	geom_point(data = fatal_1, aes(x = Long, y = Lat), colour = "deep pink",size = 0.5)+
	theme_classic()+
	coord_equal()

#filter for only 2018 fatalities
fatal_2018 <- fatal_1 %>% 
	filter(YEAR == "2018")

#plot 2018 fatalities and rainfall
ggplot() + 
	geom_raster(data = rain_avg_2018, aes(x=rain_avg_2018$LONG, y = rain_avg_2018$LAT, fill=Mean)) + 
	scale_fill_viridis(direction = -1, name = "Avg. Rainfall (mm)") +
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = NA)+
	geom_point(data = fatal_2018, aes(x = Long, y = Lat), colour = "red", alpha = 0.5, size = 0.5)+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), colour = "slate gray")+
	theme_classic()+
	coord_equal()+
	ggtitle("Road Accidents and Average Rainfall - 2018")+
	labs(x = NULL,
			 y = NULL)+
	theme(plot.title = element_text(hjust = 0.5))

#2017-------
rain2017 <- read.csv("Datasets/Climate/daily_rain.dfVictoria2017.csv")
rain_avg_2017 <- rain2017  %>%
	group_by(LONG,LAT) %>%
	dplyr::summarize(Mean = mean(RAINFALL))

fatal_2017 <- fatal_1 %>% 
	filter(YEAR == "2017")

ggplot() + 
	geom_raster(data = rain_avg_2017, aes(x=rain_avg_2017$LONG, y = rain_avg_2017$LAT, fill=Mean)) + 
	scale_fill_viridis(direction = -1, name = "Avg. Rainfall (mm)") +
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = NA)+
	geom_point(data = fatal_2017, aes(x = Long, y = Lat), colour = "red", alpha = 0.5, size = 0.5)+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), colour = "slate gray")+
	theme_classic()+
	coord_equal()+
	ggtitle("Road Accidents and Average Rainfall - 2017")+
	labs(x = NULL,
			 y = NULL)+
	theme(plot.title = element_text(hjust = 0.5))

#2016-------
rain2016 <- read.csv("Datasets/Climate/daily_rain.dfVictoria2016.csv")
rain_avg_2016 <- rain2016  %>%
	group_by(LONG,LAT) %>%
	dplyr::summarize(Mean = mean(RAINFALL))

fatal_2016 <- fatal_1 %>% 
	filter(YEAR == "2016")

ggplot() + 
	geom_raster(data = rain_avg_2016, aes(x=rain_avg_2016$LONG, y = rain_avg_2016$LAT, fill=Mean)) + 
	scale_fill_viridis(direction = -1, name = "Avg. Rainfall (mm)") +
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = NA)+
	geom_point(data = fatal_2016, aes(x = Long, y = Lat), colour = "red", alpha = 0.5, size = 0.5)+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), colour = "slate gray")+
	theme_classic()+
	coord_equal()+
	ggtitle("Road Accidents and Average Rainfall - 2016")+
	labs(x = NULL,
			 y = NULL)+
	theme(plot.title = element_text(hjust = 0.5))

#2015-------
rain2015 <- read.csv("Datasets/Climate/daily_rain.dfVictoria2015.csv")
rain_avg_2015 <- rain2015  %>%
	group_by(LONG,LAT) %>%
	dplyr::summarize(Mean = mean(RAINFALL))

fatal_2015 <- fatal_1 %>% 
	filter(YEAR == "2015")

non_fatal_2015 <- non_fatal %>% 
	filter(YEAR == "2015")

ggplot() + 
	geom_raster(data = rain_avg_2015, aes(x=rain_avg_2015$LONG, y = rain_avg_2015$LAT, fill=Mean)) + 
	scale_fill_viridis(direction = -1, name = "Avg. Rainfall (mm)") +
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = NA)+
	geom_point(data = fatal_2015, aes(x = Long, y = Lat), colour = "red", size = 0.5, alpha = 0.5)+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), colour = "slate gray")+
	theme_classic()+
	coord_equal()+
	ggtitle("Road Accidents and Average Rainfall - 2015")+
	labs(x = NULL,
			 y = NULL)+
	theme(plot.title = element_text(hjust = 0.5))

#minor injuries 2015
ggplot() + 
	geom_raster(data = rain_avg_2015, aes(x=rain_avg_2015$LONG, y = rain_avg_2015$LAT, fill=Mean)) + 
	scale_fill_viridis(direction = -1, name = "Avg. Rainfall (mm)") +
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = NA)+
	geom_point(data = non_fatal_2015, aes(x = Long, y = Lat), colour = "coral", size = 0.5, alpha = 0.4)+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), colour = "slate gray")+
	theme_classic()+
	coord_equal()+
	ggtitle("Minor Injuries, No Injury and Average Rainfall - 2015")+
	labs(x = NULL,
			 y = NULL)+
	theme(plot.title = element_text(hjust = 0.5))

#2014-------
rain2014 <- read.csv("Datasets/Climate/daily_rain.dfVictoria2014.csv")
rain_avg_2014 <- rain2014  %>%
	group_by(LONG,LAT) %>%
	dplyr::summarize(Mean = mean(RAINFALL))

fatal_2014 <- fatal_1 %>% 
	filter(YEAR == "2014")

ggplot() + 
	geom_raster(data = rain_avg_2014, aes(x=rain_avg_2014$LONG, y = rain_avg_2014$LAT, fill=Mean)) + 
	scale_fill_viridis(direction = -1, name = "Avg. Rainfall (mm)") +
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = NA)+
	geom_point(data = fatal_2014, aes(x = Long, y = Lat), colour = "red", alpha = 0.5, size = 0.5)+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), colour = "slate gray")+
	theme_classic()+
	coord_equal()+
	ggtitle("Road Accidents and Average Rainfall - 2014")+
	labs(x = NULL,
			 y = NULL)+
	theme(plot.title = element_text(hjust = 0.5))

#2013-------
rain2013 <- read.csv("Datasets/Climate/daily_rain.dfVictoria2013.csv")
rain_avg_2013 <- rain2013  %>%
	group_by(LONG,LAT) %>%
	dplyr::summarize(Mean = mean(RAINFALL))

fatal_2013 <- fatal_1 %>% 
	filter(YEAR == "2013")

ggplot() + 
	geom_raster(data = rain_avg_2013, aes(x=rain_avg_2013$LONG, y = rain_avg_2013$LAT, fill=Mean)) + 
	scale_fill_viridis(direction = -1, name = "Avg. Rainfall (mm)") +
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = NA)+
	geom_point(data = fatal_2013, aes(x = Long, y = Lat), colour = "red", alpha = 0.5, size = 0.5)+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), colour = "slate gray")+
	theme_classic()+
	coord_equal()+
	ggtitle("Road Accidents and Average Rainfall - 2013")+
	labs(x = NULL,
			 y = NULL)+
	theme(plot.title = element_text(hjust = 0.5))

#All years Facetwraped
fatal_13_18 <- fatal_1 %>% 
	filter(YEAR>2012) %>% 
	filter(YEAR<2019)

rain_avg_2013$YEAR<-2013
rain_avg_2014$YEAR<-2014
rain_avg_2015$YEAR<-2015
rain_avg_2016$YEAR<-2016
rain_avg_2017$YEAR<-2017
rain_avg_2018$YEAR<-2018

rain_avg <- rbind(rain_avg_2013,rain_avg_2014,rain_avg_2015, rain_avg_2016, rain_avg_2017, rain_avg_2018)

ggplot() + 
	geom_raster(data = rain_avg, aes(x=rain_avg$LONG, y = rain_avg$LAT, fill=Mean)) + 
	scale_fill_viridis(direction = -1, name = "Avg. Rainfall (mm)") +
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = NA)+
	geom_point(data = fatal_13_18, aes(x = Long, y = Lat), colour = "red", size = 0.1, alpha = 0.2)+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), colour = "slate gray")+
	theme_classic()+
	coord_equal()+
	ggtitle("Road Accidents and Average Rainfall  - 2013-2018")+
	labs(x = NULL,
			 y = NULL)+
	theme(plot.title = element_text(hjust = 0.5))+
	facet_wrap(~YEAR, ncol = 2)
