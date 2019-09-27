library(tidyverse)
library(tmap)
library(rgdal)
library(data.table)

#create a data frame to glimpse shape file
VIC_LGA_df3 <- as.data.frame(vic_lga_map3)
glimpse(VIC_LGA_df3)

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
fatal_LGA_years <-ddply(summary_by_LGA3, .(VIC_LGA__3,YEAR,Inj.Level.Desc), summarise, fatal = sum(count_LGA))
fatal_2006 <- fatal_LGA_years %>%
	filter(YEAR==2006)

#read the shapefile using the `rgdal` library
fatal_2006_map_spread <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

#merge LGA to shapefile
fatal_2006_gather <- fatal_2006 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2006_map_spread@data <- left_join(fatal_2006_map_spread@data,fatal_2006_gather,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2006_map_spread)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2006 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2007----
fatal_2007 <- fatal_LGA_years %>%
	filter(YEAR==2007)

fatal_2007_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2007 <- fatal_2007 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2007_map@data <- left_join(fatal_2007_map@data,fatal_2007,by = "VIC_LGA__3")

tmap_mode ("plot")
tm_shape(fatal_2007_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2007 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2008-----
fatal_2008 <- fatal_LGA_years %>%
	filter(YEAR==2008)

fatal_2008_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2008 <- fatal_2008 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2008_map@data <- left_join(fatal_2008_map@data,fatal_2008,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2008_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2008 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2009-----
fatal_2009 <- fatal_LGA_years %>%
	filter(YEAR==2009)

fatal_2009_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2009 <- fatal_2009 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2009_map@data <- left_join(fatal_2009_map@data,fatal_2009,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2009_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2009 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2010-----
fatal_2010 <- fatal_LGA_years %>%
	filter(YEAR==2010)

fatal_2010_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2010 <- fatal_2010 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2010_map@data <- left_join(fatal_2010_map@data,fatal_2010,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2010_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2010 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2011-----
fatal_2011 <- fatal_LGA_years %>%
	filter(YEAR==2011)

fatal_2011_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2011 <- fatal_2011 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2011_map@data <- left_join(fatal_2011_map@data,fatal_2011,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2011_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2011 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2012-----
fatal_2012 <- fatal_LGA_years %>%
	filter(YEAR==2012)

fatal_2012_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2012 <- fatal_2012 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2012_map@data <- left_join(fatal_2012_map@data,fatal_2012,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2012_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2012 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2013-----
fatal_2013 <- fatal_LGA_years %>%
	filter(YEAR==2013)

fatal_2013_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2013 <- fatal_2013 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2013_map@data <- left_join(fatal_2013_map@data,fatal_2013,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2013_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2013 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2014-----
fatal_2014 <- fatal_LGA_years %>%
	filter(YEAR==2014)

fatal_2014_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2014 <- fatal_2014 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2014_map@data <- left_join(fatal_2014_map@data,fatal_2014,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2014_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2014 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2015-----
fatal_2015 <- fatal_LGA_years %>%
	filter(YEAR==2015)

fatal_2015_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2015 <- fatal_2015 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2015_map@data <- left_join(fatal_2015_map@data,fatal_2015,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2015_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2015 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2016-----
fatal_2016 <- fatal_LGA_years %>%
	filter(YEAR==2016)

fatal_2016_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2016 <- fatal_2016 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2016_map@data <- left_join(fatal_2016_map@data,fatal_2016,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2016_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2016 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2017-----
fatal_2017 <- fatal_LGA_years %>%
	filter(YEAR==2017)

fatal_2017_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2017 <- fatal_2017 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2017_map@data <- left_join(fatal_2017_map@data,fatal_2017,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2017_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2017 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2018-----
fatal_2018 <- fatal_LGA_years %>%
	filter(YEAR==2018)

fatal_2018_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2018 <- fatal_2018 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2018_map@data <- left_join(fatal_2018_map@data,fatal_2018,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2018_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2018 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))

#2019-----
fatal_2019 <- fatal_LGA_years %>%
	filter(YEAR==2019)

fatal_2019_map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

fatal_2019 <- fatal_2019 %>%
	spread(key = "Inj.Level.Desc", value = "fatal")
fatal_2019_map@data <- left_join(fatal_2019_map@data,fatal_2019,by = "VIC_LGA__3")

#map by fatalities and year
tmap_mode ("plot")
tm_shape(fatal_2019_map)+
	tm_polygons(c("Fatality","Serious injury","Other injury"))+
	tm_layout(main.title = "2019 Accidents - Injury Severity")+
	tm_legend(legend.position = c("right","top"))
