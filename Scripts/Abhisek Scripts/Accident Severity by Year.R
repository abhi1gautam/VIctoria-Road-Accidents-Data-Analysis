library(sqldf)
library(ggplot2)
library(dplyr)
library(sqldf)
library(RColorBrewer)
library(lubridate)

accident_data <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")

accident_data$DATE <- as.Date(accident_data$ACCIDENTDATE, "%d/%m/%Y")

accident_data$YEAR <- year(accident_data$DATE)

accident_data1 <- accident_data %>% select(YEAR, SEVERITY)

accident_data2 <- sqldf("select YEAR, SEVERITY, COUNT(1) AS FREQUENCY from accident_data1
			where SEVERITY <> 4 and YEAR between 2013 and 2018
												group by YEAR, SEVERITY")

accident_data3 <- sqldf("select * from accident_data2 where YEAR = 2013")

accident_data3$FREQUENCY <- as.numeric(accident_data3$FREQUENCY)
accident_data2$FREQUENCY <- as.numeric(accident_data2$FREQUENCY)


accident_data4 <- sqldf("select A.YEAR, A.SEVERITY as Severity, A.FREQUENCY, B.FREQUENCY AS F2,
												A.FREQUENCY/B.FREQUENCY * 100 AS REL_FREQUENCY
												FROM accident_data2 A INNER JOIN accident_data3 B
												ON A.SEVERITY = B.SEVERITY")

accident_data4$Severity <- as.factor(accident_data4$Severity)

ggplot(accident_data4, aes(YEAR, REL_FREQUENCY)) +
	geom_line(aes(color = Severity), size= 0.5)+
	labs(x = "Year", 
			 y = "Frequency", fill = "Severity", size = 4) +
	ggtitle("Severity of Accidents by Year",
					subtitle = "Total Accidents in 2013 is taken as 100")+
	theme_classic()+
	theme(plot.title = element_text(hjust = 0.5), 
				plot.subtitle = element_text(hjust = 0.5))+
	ylim(0, 140)

