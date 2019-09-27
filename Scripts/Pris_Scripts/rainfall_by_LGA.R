#read in rainfall data
rainfall_data <- read.csv("Datasets/Climate/Accidents2.csv")
rainfall_data <- separate(rainfall_data,"DATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)
glimpse(rainfall_data)

melbourne_2018 <- rainfall_by_LGA2 %>% 
	filter(YEAR == 2018) %>% 
	filter(VIC_LGA__3 == "MELBOURNE")

melbourne_2018 %>% 
	distinct(DATE)

#ARRAT TEST FOR AVERAGE RAINFALL----
arrat_2010 <- rainfall_by_LGA2 %>%
	filter(VIC_LGA__3 == "ARARAT") %>%
	filter(YEAR == 2010)

arrat_2010_1 <- ddply(arrat_2010, .(VIC_LGA__3,YEAR,SEVERITY), summarise, avg_rain = crossprod(Rainfall,count)/sum(count))

#summarise by LGA----
rainfall_by_LGA <- ddply(rainfall_data, .(Lga.Name.All,YEAR,SEVERITY,Rainfall), summarise, count = length(ACCIDENT_NO))

rainfall_by_LGA2 <- rainfall_by_LGA %>%
	separate(Lga.Name.All,c("VIC_LGA__3","LGA_2"),sep = ",")

rainfall_by_LGA2[1:24,"VIC_LGA__3"]<- "FALLS CREEK ALPINE RESORT (UNINC)"
rainfall_by_LGA2[25:26,"VIC_LGA__3"]<- "FRENCH-ELIZABETH-SANDSTONE ISLANDS (UNINC)"
rainfall_by_LGA2[27:43,"VIC_LGA__3"]<- "LAKE MOUNTAIN ALPINE RESORT (UNINC)"
rainfall_by_LGA2[44:55,"VIC_LGA__3"]<- "MOUNT BAW BAW ALPINE RESORT (UNINC)"
rainfall_by_LGA2[56:78,"VIC_LGA__3"]<- "MOUNT BULLER ALPINE RESORT (UNINC)"
rainfall_by_LGA2[79:121,"VIC_LGA__3"]<- "MOUNT HOTHAM ALPINE RESORT (UNINC)"
rainfall_by_LGA2[122:123,"VIC_LGA__3"]<- "MOUNT STIRLING ALPINE RESORT (UNINC)"

rainfall_by_LGA3 <- ddply(rainfall_by_LGA2, .(VIC_LGA__3,YEAR,SEVERITY), summarise, avg_rain = crossprod(Rainfall,count)/sum(count))

rainfall_by_LGA4 <- rainfall_by_LGA3 %>%
	spread(key = "SEVERITY",value = "avg_rain")

rainfall_by_year <- ddply(rainfall_by_LGA2, .(VIC_LGA__3,YEAR), summarise, avg_rain = crossprod(Rainfall,count)/sum(count))

#Yearly Data----
rain_2015 <- rainfall_by_LGA4 %>%
	filter(YEAR==2015)

#map average rainfall----
map <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))


#2015----
map_2015 <- readOGR(dsn = ("VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))
map_2015@data <- left_join(map_2015@data,rain_2015,by = "VIC_LGA__3")

tmap_mode ("plot")
tm_shape(map_2015)+
	tm_polygons(c("1","2","3","4"),
							title = c("Rainfall_mm","Rainfall_mm","Rainfall_mm","Rainfall_mm"))+
	tm_layout(main.title = "2015 Accidents - Rainfall and Accident Severity",
						panel.show = TRUE,
						panel.labels = c("Fatality","Severe Injury","Other Injury","No Injury"))+
	tm_legend(legend.position = c("right","top"))


