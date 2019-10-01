library(viridis)
library (dplyr)
library (tidyverse)


rain2018 <- read.csv("Datasets/Climate/daily_rain.dfVictoria2018.csv")
rain2018 <- rain2018[!(rain2018$RAINFALL == "-3276.7"),]  #For some reason the rainfall data has some "-3276.7" values so I just deleted them

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

fatal <- merge(NODE , crash ,by="ACCIDENT_NO" )
fatal_1 <- fatal %>% 
	filter(SEVERITY == 1)
fatal_1 <- separate(fatal_1,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

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
	geom_point(data = fatal_2018, aes(x = Long, y = Lat), colour = "red")+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), colour = "slate gray")+
	theme_classic()+
	coord_equal()+
	ggtitle("Fatalities and Average Rainfall - 2018")+
	labs(x = NULL,
			 y = NULL)+
	theme(plot.title = element_text(hjust = 0.5))
