library(ggplot2)

d <-  t(d1)
e <- c(1,2,3)

e<- as.character(e)






aa <- data.frame(i, j, value, stringsAsFactors=FALSE)
aa <- aa[0,]

for(i in d){
	for(j in e){
		tester <- data.frame(ACCIDENT.TYPE.DESC  = i,
												 LIGHT_CONDITION = "1", 
												 SPEED_ZONE = 80,
												 WET = 1,
												 NO_OF_VEHICLES = 2,
												 SECURITY_EQUIPS_NOT_WORN = 1,
												 ROAD_TYPE_GROUP = "HIGHWAY",
												 CAR_AGE = 2,
												 Vehicle.Type.Desc = "Car",
												 OLD_COUNT = 1,
												 ROAD_SURFACE_TYPE = as.factor(j), stringsAsFactors=FALSE)
		value = predict(mdlWet2018, tester, type = "response") *100
		aa <- rbind(aa, data.frame(i, j, value, stringsAsFactors=FALSE))
	}
}



colnames(aa) <- c("Accident_Type", "Road_Surface_Type", "Probability")

aa$Speed_Limit <- as.factor(aa$Speed_Limit)
aa$Road_Surface_Type[aa$Road_Surface_Type == "1"] <- "Dry"
aa$Road_Surface_Type[aa$Road_Surface_Type == "2"] <- "Wet"
aa$Road_Surface_Type[aa$Road_Surface_Type == "3"] <- "Muddy"


ggplot(data = aa, aes(x = Road_Surface_Type, y = Accident_Type)) +
	geom_tile(aes(fill = Probability))+
	theme_classic()+
	labs(x= "Road Surface Type", y="Type of Accident",
			 title = "Probability- Type of Accident VS Road Surface Type")+
	theme(plot.title = element_text(hjust = 0.5))+
	scale_fill_gradient(low = "#56B1F7", high = "#132B43")


#Varied Accident Type and Road Surface Type
tester <- data.frame(ACCIDENT.TYPE.DESC  = i,
										 LIGHT_CONDITION = "1", 
										 SPEED_ZONE = 80,
										 WET = 1,
										 NO_OF_VEHICLES = 2,
										 SECURITY_EQUIPS_NOT_WORN = 1,
										 ROAD_TYPE_GROUP = "HIGHWAY",
										 CAR_AGE = 2,
										 Vehicle.Type.Desc = "Car",
										 OLD_COUNT = 1,
										 ROAD_SURFACE_TYPE = as.factor(j), stringsAsFactors=FALSE)




