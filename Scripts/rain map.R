library(geosphere)
library(tibble)
library(plyr)
library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)
library(sp)
library(rgdal)
library(ggplot2)
library(ggmap)
library (dplyr)
rain2018 <- read.csv("New/daily_rain.dfVictoria2018.csv")
wa.map <- readOGR(dsn = "Standard", layer = "VIC_STATE_POLYGON_shp")
plot(rain2018)
coordinates(rain2018) <- ~ LONG + LAT
proj4string(rain2018) <- proj4string(wa.map)
plot (rain2018)

library(viridis)

daily_rain.dfVictoria2019 <- daily_rain.dfVictoria2019 %>% 
	mutate(RAINFALL = na_if(RAINFALL, "-3276.7"))

daily_rain.df2017[daily_rain.df2017 == -3276.7]<-NA
daily_rain.df2017 <- daily_rain.df2017[!(daily_rain.df2017$RAINFALL == "-3276.7"),]
rain_avg_2018 <- rain2018  %>%
	group_by(LONG,LAT) %>%
	dplyr::summarize(Mean = mean(RAINFALL))

ggplot() + geom_raster(data = rain_avg_2018, aes(x=rain_avg_2018$LONG, y = rain_avg_2018$LAT, fill=Mean)) + 
	coord_fixed(ratio = 1) +
	scale_fill_gradientn(colours=c("#FFFFFFFF","#0000FFFF")) +
	theme_bw()

