library(esquisse)
esquisse::esquisser()

accident <- separate(accident,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

#accident and vehicle
acc_vehicle <- left_join(vehicle,accident,by = "ACCIDENT_NO")

acc_vehicle2 <- acc_vehicle %>% 
	filter(VEHICLE_TYPE %in% 1:9 | VEHICLE_TYPE == 17| VEHICLE_TYPE %in% 60:72)

acc_vehicle3 <- acc_vehicle2 %>% 
	filter(VEHICLE_YEAR_MANUF %in% 1950:2020)

acc_vehicle3$YEAR <- as.integer(acc_vehicle3$YEAR)
acc_vehicle3$SEVERITY <- as.factor(acc_vehicle3$SEVERITY)

acc_vehicle4 <- acc_vehicle3 %>% 
	mutate(CAR_AGE = (YEAR - VEHICLE_YEAR_MANUF))

acc_vehicle5 <- acc_vehicle4
acc_vehicle5$SEVERITY [acc_vehicle5$SEVERITY == 1] <- "Fatal Injury"
acc_vehicle5$SEVERITY [acc_vehicle5$SEVERITY == 2] <- "Serious Injury"
acc_vehicle5$SEVERITY [acc_vehicle5$SEVERITY == 3] <- "Minor Injury"
acc_vehicle5$SEVERITY [acc_vehicle5$SEVERITY == 4] <- "No Injury"

acc_vehicle5 %>% 
	filter(CAR_AGE<50) %>% 
	ggplot(aes(x = CAR_AGE, fill = SEVERITY)) +
	geom_bar ()

acc_vehicle5 %>% 
	filter(CAR_AGE<50) %>% 
	ggplot(aes(x = CAR_AGE, fill = SEVERITY)) +
	geom_bar(position = "fill")

motor_1 <- acc_vehicle %>% 
	filter(VEHICLE_TYPE %in% 10:12)

motor_1$YEAR <- as.integer(motor_1$YEAR)

motor_2 <- motor_1 %>% 
	filter(VEHICLE_YEAR_MANUF >1900)

motor_3 <- motor_2 %>% 
	mutate(motorcycle_age = YEAR - VEHICLE_YEAR_MANUF)

motor_3$SEVERITY[motor_3$SEVERITY ==1] <- "Fatal Injury"	
motor_3$SEVERITY[motor_3$SEVERITY ==2] <- "Serious Injury"	
motor_3$SEVERITY[motor_3$SEVERITY ==3] <- "Minor Injury"	
motor_3$SEVERITY[motor_3$SEVERITY ==4] <- "No Injury"	

motor_3 %>% 
	filter(motorcycle_age<30) %>% 
	ggplot(aes(x = motorcycle_age, fill = SEVERITY))+
	geom_bar()

motor_3 %>% 
	filter(motorcycle_age<30) %>% 
	ggplot(aes(x = motorcycle_age, fill = SEVERITY))+
	geom_bar(position = "fill")
