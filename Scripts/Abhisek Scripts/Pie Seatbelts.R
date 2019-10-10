library(tidyverse)
library(lmtest)
library(Amelia)
library(mlbench)
library(sqldf)
library(dplyr)

person_data <- read.csv("Datasets/Road Crashes/PERSON.csv")

person_data1 <- sqldf("select INJ_LEVEL, HELMET_BELT_WORN FROM person_data
											where HELMET_BELT_WORN not in (8, 9)")

person_data1$HELMET_BELT_WORN[person_data1$HELMET_BELT_WORN==1] <-'With Safety Equipments'
person_data1$HELMET_BELT_WORN[person_data1$HELMET_BELT_WORN==3] <-'With Safety Equipments'
person_data1$HELMET_BELT_WORN[person_data1$HELMET_BELT_WORN==6] <-'With Safety Equipments'
person_data1$HELMET_BELT_WORN[person_data1$HELMET_BELT_WORN==2] <-'Without Safety Equipments'
person_data1$HELMET_BELT_WORN[person_data1$HELMET_BELT_WORN==4] <-'Without Safety Equipments'
person_data1$HELMET_BELT_WORN[person_data1$HELMET_BELT_WORN==5] <-'Without Safety Equipments'
person_data1$HELMET_BELT_WORN[person_data1$HELMET_BELT_WORN==7] <-'Without Safety Equipments'

person_data2$INJ_LEVEL[person_data2$INJ_LEVEL == 4] <- 3

person_data2 <- sqldf("select INJ_LEVEL, HELMET_BELT_WORN, SUM(1) as COUNT1 from person_data1
			group by INJ_LEVEL, HELMET_BELT_WORN")

person_data2$INJ_LEVEL <- as.factor(person_data2$INJ_LEVEL)

person_data2$COUNT1 <-  as.numeric(person_data2$COUNT1)
person_data3$COUNT1 <-  as.numeric(person_data3$COUNT1)

person_data3 <- sqldf("select HELMET_BELT_WORN, INJ_LEVEL, SUM(COUNT1*23.9214) AS COUNT1,
	ROUND(SUM(COUNT1/12366)* 100, 2) as PERC from person_data2
											where HELMET_BELT_WORN = 'Without Safety Equipments'
											GROUP BY  HELMET_BELT_WORN, INJ_LEVEL")

person_data4 <- sqldf("select HELMET_BELT_WORN, INJ_LEVEL, SUM(COUNT1) AS COUNT1,
	ROUND(SUM(COUNT1/295812)* 100, 2) as PERC from person_data2
											where HELMET_BELT_WORN = 'With Safety Equipments'
											GROUP BY  HELMET_BELT_WORN, INJ_LEVEL")
	

person_data5 <- union(person_data3, person_data4)
person_data5$PERC <- as.character(person_data5$PERC)

p1<- ggplot(person_data5, aes(x = "", y = COUNT1, fill = INJ_LEVEL)) +
	geom_bar(width = 1, stat = "identity") +
	geom_text(aes(label = PERC), color = "white",
						position= position_stack(vjust = 0.5),
						size = 3)+
	facet_grid(. ~ HELMET_BELT_WORN)+
	coord_polar("y", start=0)+
	theme_classic()+
	labs(fill = "Injuiry Level", size = 4) +
	ggtitle("Chances of Injuiry with and without Seatbelts/Helmets")

p1 + theme(axis.line = element_blank(),
										axis.text = element_blank(),
										axis.ticks = element_blank(),
					 					axis.title = element_blank(),
										plot.title = element_text(hjust = 0.5))


