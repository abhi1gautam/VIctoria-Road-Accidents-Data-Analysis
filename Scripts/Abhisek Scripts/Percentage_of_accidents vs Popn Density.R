library(sqldf)
library(ggplot2)
library(dplyr)
library(sqldf)
library(RColorBrewer)



accident_data <- read.csv("Datasets/Computed/Accident_LVL_Table_all.csv")
person_data <- read.csv("Datasets/Road Crashes/PERSON.csv")

accident_data$DATE <- as.Date(accident_data$DATE, "%Y-%m-%d")


accident_data_1 <- accident_data %>% filter(DATE >=  as.Date('2013-01-01'),
																						DATE <= as.Date('2018-12-31'))

accident_data_1 <- accident_data_1 %>% filter(SEVERITY !=  4)


accident_data2 <- sqldf("select CASE WHEN \"PERSONS.KM2\" IS NULL THEN 99000
					WHEN \"PERSONS.KM2\" LIKE '-' THEN 99000
					ELSE \"PERSONS.KM2\" 
				END as popn_density, SEVERITY from accident_data_1")

accident_data3 <- sqldf("select popn_density, severity, sum(1) as accident_frequency,
						floor(popn_density / 1000)  as popn_density_mod
						from accident_data2 where floor(popn_density / 1000) <>99
												group by popn_density, severity")


accident_data3$SEVERITY <- as.factor(accident_data3$SEVERITY)

accident_data3$popn_density_mod <- as.factor(accident_data3$popn_density_mod)

accident_data3$popn_density <- as.numeric(accident_data3$popn_density)

ggplot(accident_data3, aes(x = popn_density_mod, y = accident_frequency, fill = SEVERITY)) + 
	geom_bar(stat = "identity") +
	labs(x = "Population Density (In Thousands)", 
			 y = "Number of Accidents", fill = "Severity", size = 4) +
	ggtitle("Accident Frequency with LGA Population Density")+
	theme_classic()+
	theme(plot.title = element_text(hjust = 0.5))

ggplot(accident_data3, aes(x = popn_density_mod, y = accident_frequency, fill = SEVERITY)) + 
	geom_bar(position = "fill",stat = "identity") +
	scale_y_continuous(labels = scales::percent_format())+
	labs(x = "Population Density (In Thousands)", 
			 y = "Percentage of Accidents", fill = "Severity", size = 4) +
	ggtitle("Accident ratios with LGA Population Density")+
	theme_classic()+
	theme(plot.title = element_text(hjust = 0.5))





