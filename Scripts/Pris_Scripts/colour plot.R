library(readr)
library(tidyverse)
library(rgdal)
library(ggmaps)


crash <- read_csv("Datasets/Road Crashes/ACCIDENT.csv")
NODE <- read_csv("Datasets/Road Crashes/NODE.csv")

test <- crash
factor(test$SEVERITY)

test$SEVERITY[test$SEVERITY==1] <- "Fatal Accident"
test$SEVERITY[test$SEVERITY==2] <- "Serious Injury Accident"
test$SEVERITY[test$SEVERITY==3] <- "Minor Injury Accident"
test$SEVERITY[test$SEVERITY==4] <- "No Injury"

tog <- merge(NODE , test ,by="ACCIDENT_NO" )

ggplot(tog, aes(Long, Lat, colour = SEVERITY)) + 
	geom_point()+
	scale_colour_manual(values = c("Red", "blue", "Green", "Orange"))

#map only fatalities
tog_1<- tog %>% 
	filter(SEVERITY == 1)

tog_2 <- separate(tog_1,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

tog_1 %>% 
	ggplot(aes(Long, Lat))+
	geom_point(colour = "red")

#read in shapefile + create data frame from shapefile
vic_state_map <- readOGR(dsn = ("VIC_STATE_POLYGON/VIC_STATE_POLYGON_shp.shp"))
vic_state_df <- fortify(vic_state_map)

#plot polygon and points
ggplot()+
	geom_polygon(data = vic_state_df, aes(x=long,y=lat, group=group), colour = "black", fill = "white")+
	geom_point(data = tog_1, aes(x = Long, y = Lat), colour = "deep pink",size = 0.5)+
	theme_bw()+
	ggtitle("Fatalities Across Victoria - 2006-19")
	