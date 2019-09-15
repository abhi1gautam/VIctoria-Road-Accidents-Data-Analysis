##### Reading Libraries ----
library(tidyverse)

##### Reading files ----
accident <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")
accident_chainage <-  read.csv("Datasets/Road Crashes/ACCIDENT_CHAINAGE.CSV")
accident_event<- read.csv("Datasets/Road Crashes/ACCIDENT_EVENT.CSV")
accident_location<- read.csv("Datasets/Road Crashes/ACCIDENT_LOCATION.CSV")
atmospheric_condition<- read.csv("Datasets/Road Crashes/ATMOSPHERIC_COND.CSV")
accident_node <-  read.csv("Datasets/Road Crashes/NODE.CSV")
accident_node_ID <-  read.csv("Datasets/Road Crashes/NODE_ID_COMPLEX_INT_ID.CSV")
accident_person <-  read.csv("Datasets/Road Crashes/PERSON.CSV")
road_condition <-  read.csv("Datasets/Road Crashes/ROAD_SURFACE_COND.CSV")
accident_subdca <-  read.csv("Datasets/Road Crashes/SUBDCA.CSV")
accident_vehicle <-  read.csv("Datasets/Road Crashes/VEHICLE.CSV")

glimpse(accident)
glimpse(accident_mergerd)

accident$ACCIDENTDATE <- as.Date(accident$ACCIDENTDATE, "%d/%m/%Y")

#Separate month and year
accident <- separate(accident, "ACCIDENTDATE", c("Year", "Month", "Day"), sep = "-", remove = FALSE)


accident_mergerd <- merge(accident,accident_node,by="ACCIDENT_NO")


accident %>%
	ggplot(aes(x=))+
	geom_bar()

?geom_histogram


ggplot(data = TTM, aes(x = Type.of.Behavior, y = Sample.Size, fill = Stage.of.Change)) + 
	geom_bar()