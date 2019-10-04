library(tidyverse)
library (dplyr)
vehicle$Vehicle.Type.Desc <- as.factor(person$HELMET_BELT_WORN)
vehicle <- vehicle %>% cbind(with(vehicle, model.matrix(~ Vehicle.Type.Desc + 0)))
vehicle <- vehicle %>%
	group_by(ACCIDENT_NO) %>%
	mutate(Vehicle.Type.DescBicycle = sum(Vehicle.Type.DescBicycle)) %>%
	mutate(Vehicle.Type.DescCar = sum(Vehicle.Type.DescCar)) %>%
	mutate(`Vehicle.Type.DescStation Wagon` = sum(`Vehicle.Type.DescStation Wagon`)) %>%
	mutate(`Vehicle.Type.DescMotor Cycle` = sum(`Vehicle.Type.DescMotor Cycle`)) %>%
	mutate(Vehicle.Type.DescUtility = sum(Vehicle.Type.DescUtility)) %>%
	mutate(`Vehicle.Type.DescPanel Van` = sum(`Vehicle.Type.DescPanel Van`)) %>%
	mutate(Others = sum(`Vehicle.Type.DescBus/Coach`,`Vehicle.Type.DescHorse (ridden or drawn)`,`Vehicle.Type.DescLight Commercial Vehicle (Rigid) <= 4.5 Tonnes GVM`,`Vehicle.Type.DescMini Bus(9-13 seats)`,Vehicle.Type.DescMoped,`Vehicle.Type.DescMotor Scooter`,`Vehicle.Type.DescNot Applicable`,`Vehicle.Type.DescOther Vehicle`,`Vehicle.Type.DescParked trailers`,`Vehicle.Type.DescPlant machinery and Agricultural equipment`,`Vehicle.Type.DescQuad Bike`,`Vehicle.Type.DescRigid Truck(Weight Unknown)`,Vehicle.Type.DescTaxi,Vehicle.Type.DescTrain,Vehicle.Type.DescTram,Vehicle.Type.DescUnknown,`Vehicle.Type.DescPrime Mover Only`,`Vehicle.Type.DescPrime Mover Only`,`Vehicle.Type.DescPrime Mover - Single Trailer`,`Vehicle.Type.DescPrime Mover B-Triple`,`Vehicle.Type.DescPrime Mover (No of Trailers Unknown)`,`Vehicle.Type.DescHeavy Vehicle (Rigid) > 4.5 Tonnes`)) %>%
	ungroup()

personFiltered <-	filter(person, ROAD_USER_TYPE == 2 | ROAD_USER_TYPE == 4 | ROAD_USER_TYPE == 6 | ROAD_USER_TYPE == 7)

personFiltered2 <- person 
personFiltered2$INJ_LEVEL <- as.factor(personFiltered2$INJ_LEVEL)
personFiltered2 <- personFiltered2 %>% cbind(with(personFiltered2, model.matrix(~ INJ_LEVEL + 0)))
personFiltered2$HELMET_BELT_WORN <- as.factor(personFiltered2$HELMET_BELT_WORN)
personFiltered2 <- personFiltered2 %>% cbind(with(personFiltered2, model.matrix(~ HELMET_BELT_WORN + 0)))
personFiltered2 <- personFiltered2 %>% cbind(with(personFiltered2, model.matrix(~ AGE.GROUP + 0)))

personFiltered2 <-	filter(personFiltered2, ROAD_USER_TYPE != 2 & ROAD_USER_TYPE != 4 & ROAD_USER_TYPE != 6 & ROAD_USER_TYPE != 7)

personFiltered2 <- personFiltered2 %>%
	group_by(ACCIDENT_NO, VEHICLE_ID) %>%
	mutate (Old = sum(`AGE.GROUP70+`)) %>%
	ungroup()

personFiltered3 <-	filter(personFiltered2, ROAD_USER_TYPE == 2 | ROAD_USER_TYPE == 4 | ROAD_USER_TYPE == 6 | ROAD_USER_TYPE == 7)


merged <- merge(MergedFilesFiltered, personFiltered[, c(1,3,4,6)], by = c("ACCIDENT_NO","VEHICLE_ID"))
merged <- merge(merged, personFiltered3[, c(1,3,31,32)],by = c("ACCIDENT_NO","VEHICLE_ID"))
merged <- merge(merged, accident_location[!duplicated(accident_location$ACCIDENT_NO), c(1,5)],by = "ACCIDENT_NO")
merged <- merge(merged, road_surface_cond[!duplicated(road_surface_cond$ACCIDENT_NO), c(1,3)],by = "ACCIDENT_NO")
merged <- merge(merged, road_surface_cond[!duplicated(road_surface_cond$ACCIDENT_NO), c(1,3)],by = "ACCIDENT_NO")


accident_rain <- merge(accident_rain,vehicle[!duplicated(vehicle$ACCIDENT_NO), c(1,39,41,47,51,61,66,67)],by = "ACCIDENT_NO", all.x = TRUE)
accident_rain[, 38:51][is.na(accident_rain[, 38:51])] <- 0

accident_rain <- merge(accident_rain,accident_location[!duplicated(accident_location$ACCIDENT_NO), c(1,5)],by = "ACCIDENT_NO", all.x = TRUE)


accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group0-4` = `Age.Group0.4`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group5-12` = `Age.Group5.12`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group13-15` = `Age.Group13.15`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group16-17` = `Age.Group16.17`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group17-21` = `Age.Group17.21`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group22-25` = `Age.Group22.25`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group26-29` = `Age.Group26.29`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group30-39` = `Age.Group30.39`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group40-49` = `Age.Group40.49`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group50-59` = `Age.Group50.59`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group60-64` = `Age.Group60.64`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group64-69` = `Age.Group64.69`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Group70+` = `Age.Group70.`)
accident_rain <- accident_rain  %>% dplyr::rename (`Deaths.Age.Groupunknown` = `Age.Groupunknown`)
