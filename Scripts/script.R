##### Reading Libraries ----
library(tidyverse)
library(esquisse)
library(geosphere)

##### Reading files ----
accident <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")
climate86018 <- read.csv("Datasets/Climate/86018.csv")
accident_node <-  read.csv("Datasets/Road Crashes/NODE.CSV")
accident_chainage <- read.csv("Datasets/Road Crashes/ACCIDENT_CHAINAGE.csv")
accident_event <- read.csv("Datasets/Road Crashes/ACCIDENT_EVENT.csv")
accident_location <- read.csv("Datasets/Road Crashes/ACCIDENT_LOCATION.csv")
atmospheric_cond <- read.csv("Datasets/Road Crashes/ATMOSPHERIC_COND.csv")
node_id_complex_int_id <- read.csv("Datasets/Road Crashes/NODE_ID_COMPLEX_INT_ID.csv")
person <- read.csv("Datasets/Road Crashes/PERSON.csv")
road_surface_cond <- read.csv("Datasets/Road Crashes/ROAD_SURFACE_COND.csv")
subdca <- read.csv("Datasets/Road Crashes/SUBDCA.csv")
vehicle <- read.csv("Datasets/Road Crashes/VEHICLE.csv")


##### Data Transformation ----
#Converting fct to date
accident$ACCIDENTDATE <- as.Date(accident$ACCIDENTDATE, "%d/%m/%Y")
climate86018$YYYY.MM.DD  <- as.Date (climate86018$YYYY.MM.DD, "%Y-%m-%d")

#Adding station latitude and longitude to all rows (to calc distance btw acc and climate station)
climate86018$station_lat <- rep(-37.8795,nrow(climate86018))
climate86018$station_long <- rep(145.0368,nrow(climate86018))

#Renaming date columns (to be used as primary key btw accidents and climate data)
accident <- accident %>% rename (DATE = ACCIDENTDATE)
climate86018 <- climate86018 %>% rename (DATE = YYYY.MM.DD)

#### Merging Data ----
#Creating mergedFiles object, which contains accident, accident location and climate data
mergedFiles <- merge(accident,accident_node,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, climate86018,by="DATE")
mergedFiles <- merge(mergedFiles, accident_chainage,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, accident_event,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, accident_location,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, atmospheric_cond,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, node_id_complex_int_id,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, person,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, road_surface_cond,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, subdca,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, vehicle,by="ACCIDENT_NO")





#Adding dist_station_crash column with distance between accident location and climate observatory
mergedFiles <- mergedFiles %>% 
	mutate(dist_station_crash = distHaversine(cbind(station_long, station_lat), cbind(Long, Lat)))
compactTable <- select(mergedFiles, )

