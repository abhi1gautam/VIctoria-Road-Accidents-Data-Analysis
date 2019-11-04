library(plyr)
library(rgdal)
library(raster)
library(esquisse)

esquisse::esquisser()

library(ggplot2)

ggplot(accident_5) +
 aes(x = LIGHT_CONDITION, y = Road.User.Type.Desc, fill = portion) +
 geom_tile(size = 1L) +
 scale_fill_distiller(palette = "Spectral") +
 labs(x = "Light Condition", y = "Road User", title = "Proportion of Road User injury severity in diffent light conditions", fill = "Proportion of Road User") +
 theme_minimal() +
 theme(legend.position = "bottom") +
 facet_wrap(vars(INJ_LEVEL))rain2013$YEAR<-2013
rain2014$YEAR<-2014
rain2015$YEAR<-2015
rain2016$YEAR<-2016
rain2017$YEAR<-2017
rain2018$YEAR<-2018

rain <- rbind(rain2013,rain2014,rain2015, rain2016, rain2017, rain2018)

rm(rain_day_count)

count <- ddply(rain, .(YEAR, LONG, LAT), summarise, count = length(RAINFALL))

person <- read.csv("Datasets/Road Crashes/PERSON.csv")

age_count <- ddply(person, .(Age.Group), summarise, count = length(ACCIDENT_NO))
acc_count <- ddply(accident, .(YEAR), summarise, count = length(ACCIDENT_NO))

#basic shapefile maps-------

vic_highway <- readOGR(dsn = "Datasets/Shapefiles/VIC_HIGHWAYS_FREEWAYS.shp")
vic_state_map <- readOGR(dsn = ("Datasets/Shapefiles/VIC_STATE_POLYGON/VIC_STATE_POLYGON_shp.shp"))
vic_lga_map <- readOGR(dsn = ("Datasets/Shapefiles/VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

vic_state_df <- fortify(vic_state_map)
vic_lga_map_df <- fortify(vic_lga_map)
vic_highways_freeways <- intersect(vic_highway,vic_state_map)

ggplot()+
	geom_polygon(data = vic_lga_map_df, aes(x=long, y=lat, group=group),colour = "light coral", fill = NA)+
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = NA)+
	geom_path(data = vic_highways_freeways, aes(x=long,y=lat, group = group), colour = "slate gray")+
	theme_classic()+
	coord_equal()+
	labs(x = NULL,
			 y = NULL)+
	ggtitle("State of Victoria", subtitle = "LGA's and Major roads")+
	theme(plot.title = element_text(hjust = 0.5),
				plot.subtitle = element_text(hjust = 0.5))


#driver fatality sex age----------
person_1 <- person %>%
	filter(!(Age.Group %in% "unknown")) %>%
	filter(INJ_LEVEL == 1|INJ_LEVEL == 2) %>% 
	filter(Road.User.Type.Desc == "Drivers") %>% 
	filter(Age.Group != "5-12") %>% 
	filter(SEX != "U")

ggplot(person_1) +
	aes(x = Age.Group, fill = SEX) +
	geom_bar() +
	theme_classic()+
	ggtitle("Driver Fatalities by Age Group and Gender")+
	theme(plot.title = element_text(hjust = 0.5))+
	facet_wrap(~SEX)


#LIght Condition heat map---------
accident_1 <- accident

accident_1$LIGHT_CONDITION [accident_1$LIGHT_CONDITION == "1"] <- "Day" 
accident_1$LIGHT_CONDITION [accident_1$LIGHT_CONDITION == "2"] <- "Dusk/Dawn" 
accident_1$LIGHT_CONDITION [accident_1$LIGHT_CONDITION == "3"] <- "Night" 
accident_1$LIGHT_CONDITION [accident_1$LIGHT_CONDITION == "4"] <- "Night" 
accident_1$LIGHT_CONDITION [accident_1$LIGHT_CONDITION == "5"] <- "Night" 
accident_1$LIGHT_CONDITION [accident_1$LIGHT_CONDITION == "6"] <- "Night" 
accident_1$SEVERITY<- as.factor(accident_1$SEVERITY)
glimpse(accident_2)
accident_2 <- left_join(person,accident_1, by = "ACCIDENT_NO")
accident_3 <- ddply(accident_2, .(LIGHT_CONDITION,Road.User.Type.Desc, INJ_LEVEL), summarise, No_People = length(ACCIDENT_NO))
accident_4 <- ddply(accident_2, .(Road.User.Type.Desc, INJ_LEVEL), summarise, No_Population = length(ACCIDENT_NO))

accident_5<- left_join(accident_3,accident_4, by = c("Road.User.Type.Desc","INJ_LEVEL"))
accident_5$portion <- accident_5$No_People/accident_5$No_Population*100

accident_6 <- accident_5 %>%
	filter(!(LIGHT_CONDITION %in% "9")) %>%
	filter(!(Road.User.Type.Desc %in% 
					 	"Unknown")) %>%
	filter(INJ_LEVEL >= 1 & INJ_LEVEL <= 3.02)

accident_6$INJ_LEVEL [accident_6$INJ_LEVEL == "1"] <- "Fatality"
accident_6$INJ_LEVEL [accident_6$INJ_LEVEL == "2"] <- "Serious Injury"
accident_6$INJ_LEVEL [accident_6$INJ_LEVEL == "3"] <- "Minor Injury"

ggplot(accident_6) +
	aes(x = LIGHT_CONDITION, y = Road.User.Type.Desc, fill = portion) +
	geom_tile(size = 1L) +
	scale_fill_distiller(palette = "RdYlGn") +
	labs(x = "Light Condition", y = "Road User", title = "Proportion of Road User Injury Severity in Different Light Conditions", fill = "Proportion of Road User by Severity") +
	theme_classic() +
	theme(legend.position = "bottom") +
	facet_wrap(vars(INJ_LEVEL))+
	theme(plot.title = element_text(hjust = 0.5))

#Speed Zone
speed <- left_join(person, accident, by = "ACCIDENT_NO")
