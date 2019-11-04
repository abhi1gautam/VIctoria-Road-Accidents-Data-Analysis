library(tidyverse)

population <- read.csv("Datasets/Population/Master Table.csv")
glimpse(population)

corr_pop <- cor(population)

pop_1 <- population %>% 
	select(AGE,INJ_LEVEL)
corr_pop1 <- cor(pop_1)

accident <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")
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

car_sev <- left_join(vehicle,accident, by = "ACCIDENT_NO")

rm(car_sev)

accident <- separate(accident,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

#accident and vehicle
acc_vehicle <- left_join(vehicle,accident,by = "ACCIDENT_NO")

#categorise vehicles
acc_vehicle_1<- acc_vehicle
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 1] <- "CAR"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 2] <- "CAR"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 3] <- "CAR"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 4] <- "CAR"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 5] <- "CAR"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 6] <- "TRUCK"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 7] <- "TRUCK"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 8] <- "BUS"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 9] <- "BUS"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 10] <- "MOTORCYCLE"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 11] <- "MOTORCYCLE"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 12] <- "MOTORCYCLE"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 13] <- "BICYCLE"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 14] <- "HORSE"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 15] <- "TRAM/TRAIN"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 16] <- "TRAM/TRAIN"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 17] <- "OTHER"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 18] <- "OTHER"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 19] <- "OTHER"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 20] <- "OTHER"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 27] <- "OTHER"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 60] <- "OTHER"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 61] <- "OTHER"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 62] <- "OTHER"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 63] <- "OTHER"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 71] <- "OTHER"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 72] <- "OTHER"
acc_vehicle_1$VEHICLE_TYPE [acc_vehicle_1$VEHICLE_TYPE == 99] <- "UNKNOWN"

acc_vehicle_1$SEVERITY <- as.factor(acc_vehicle_1$SEVERITY)
glimpse(acc_vehicle_1)

acc_vehicle_1 %>% 
	filter(VEHICLE_TYPE != "UNKNOWN") %>% 
	ggplot(aes(x = VEHICLE_TYPE, fill = SEVERITY))+
	geom_bar(position = "fill")

#by person-----
acc_veh_per <- left_join(person,vehicle, by = c("ACCIDENT_NO","VEHICLE_ID"))

acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 1] <- "CAR"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 2] <- "CAR"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 3] <- "CAR"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 4] <- "CAR"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 5] <- "CAR"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 6] <- "TRUCK"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 7] <- "TRUCK"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 8] <- "BUS"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 9] <- "BUS"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 10] <- "MOTORCYCLE"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 11] <- "MOTORCYCLE"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 12] <- "MOTORCYCLE"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 13] <- "BICYCLE"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 14] <- "HORSE"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 15] <- "TRAM/TRAIN"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 16] <- "TRAM/TRAIN"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 17] <- "OTHER"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 18] <- "OTHER"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 19] <- "OTHER"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 20] <- "OTHER"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 27] <- "OTHER"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 60] <- "OTHER"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 61] <- "OTHER"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 62] <- "OTHER"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 63] <- "OTHER"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 71] <- "OTHER"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 72] <- "OTHER"
acc_veh_per$VEHICLE_TYPE [acc_veh_per$VEHICLE_TYPE == 99] <- "UNKNOWN"

acc_veh_per$INJ_LEVEL [acc_veh_per$INJ_LEVEL == 1] <- "Fatal"
acc_veh_per$INJ_LEVEL [acc_veh_per$INJ_LEVEL == 2] <- "Serious Injury"




acc_veh_per %>% 
	filter(VEHICLE_TYPE != "UNKNOWN") %>% 
	filter(VEHICLE_TYPE != "OTHER") %>% 
	filter(INJ_LEVEL == "Fatal" | INJ_LEVEL == "Serious Injury") %>% 
	ggplot(aes(x = VEHICLE_TYPE, y = count(), fill = INJ_LEVEL))+
	geom_bar(stat = "identity" ,position = "dodge")

