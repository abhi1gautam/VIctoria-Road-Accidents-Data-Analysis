library(sqldf)
library(ggplot2)
library(dplyr)
library(sqldf)
library(RColorBrewer)
library(lubridate)


accident_data <- read.csv("Datasets/Computed/Vehicle Data.csv")

accident_data$DATE <- as.Date(accident_data$DATE, "%Y-%m-%d")

accident_data$SPEED_ZONE <- as.factor(accident_data$SPEED_ZONE)

accident_data$SEVERITY <- as.factor(accident_data$SEVERITY)

accident_data1 <- sqldf("select SEVERITY, SPEED_ZONE, SEX, \"AGE.GROUP\" from accident_data
where speed_zone not in (777, 888, 999)
and severity <> 4")

#Speed Zone VS Severity
ggplot(accident_data1, aes(AGE.GROUP, SPEED_ZONE)) +
	geom_jitter(aes(color = AGE.GROUP), size= 0.2, alpha(0.5))+
	labs(x = "Age Groups", 
			 y = "Speed Zone", fill = "Severity", size = 4) +
	ggtitle("Accident Severity by Speed Zone")+
	theme_classic()+
	theme(plot.title = element_text(hjust = 0.5))

accident_data1 <- sqldf("select SEVERITY, SPEED_ZONE, SEX, \"AGE.GROUP\" from accident_data
where speed_zone not in (777, 888, 999)
and severity <> 4")

accident_data1 <- sqldf("select SEVERITY, SPEED_ZONE from accident_data
where speed_zone not in (777, 888, 999)
and severity =1")

#Speed Zone VS Severity
ggplot(accident_data1, aes(SEVERITY, SPEED_ZONE)) +
	geom_jitter(aes(color = SEVERITY), size= 0.5)+
	labs(x = "Severity of Accident", 
			 y = "Speed Zone", fill = "Severity", size = 4) +
	ggtitle("Accident Severity by Speed Zone")+
	theme_classic()+
	theme(plot.title = element_text(hjust = 0.5),
				legend.position = "none")









