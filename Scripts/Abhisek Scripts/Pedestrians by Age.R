vehicle_data <- read.csv("Datasets/Computed/Vehicle Data Cleaned.csv")

person_data <- read.csv("Datasets/Road Crashes/PERSON.csv")

accidents_with_pedestrians <- sqldf("select ACCIDENT_NO, AGE, 
		INJ_LEVEL
			from person_data where \"Road.User.Type.Desc\" ='Pedestrians'
																		AND AGE IS NOT NULL")

pedestrians <- sqldf("SELECT AGE, INJ_LEVEL, COUNT(1) AS FREQUENCY
										 FROM accidents_with_pedestrians group by
										 AGE, INJ_LEVEL")


pedestrians$INJ_LEVEL <- as.factor(pedestrians$INJ_LEVEL)

ggplot(pedestrians, aes(AGE, FREQUENCY)) +
	geom_line(aes(color = INJ_LEVEL), size= 0.5)+
	labs(x = "Age", 
			 y = "Number of Accidents", fill = "Injuiry Level", size = 4) +
	ggtitle("Number of Pedestrians Involved in an Accident by Age")+
	theme_classic()+
	theme(plot.title = element_text(hjust = 0.5), 
				plot.subtitle = element_text(hjust = 0.5))