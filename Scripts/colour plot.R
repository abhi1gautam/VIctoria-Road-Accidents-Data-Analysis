library(readr)
library(tidyverse)


crash <- read_csv("Datasets/Road Crashes/ACCIDENT.csv")
NODE <- read_csv("Datasets/Road Crashes/NODE.csv")

test <- crash
factor(test$SEVERITY)


test$SEVERITY[test$SEVERITY==1] <- "Fatal Accident"
test$SEVERITY[test$SEVERITY==2] <- "Serious Injury Accident"
test$SEVERITY[test$SEVERITY==3] <- "Minor Injury Accident"
test$SEVERITY[test$SEVERITY==4] <- "No Injury"



tog <- merge(NODE , test ,by="ACCIDENT_NO" )

ggplot(tog, aes(Long, Lat, colour = SEVERITY)) + 
	geom_point()+
	scale_colour_manual(values = c("Red", "blue", "Green", "Orange"))

	