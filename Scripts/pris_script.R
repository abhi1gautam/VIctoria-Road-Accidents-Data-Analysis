# Reading Libraries ----
library(tidyverse)
library(lubridate)
library(plyr)
library(AMR)
library(data.table)
library(gganimate)
library(reshape2)

# Reading files ----
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

glimpse(total_accident)
glimpse(accident)
glimpse(accident_chainage)
glimpse(accident_event)
glimpse(accident_location)
glimpse(accident_node)
glimpse(atmospheric_cond)
glimpse(node_id_complex_int_id)
glimpse(person)
glimpse(road_surface_cond)
glimpse(subdca)
glimpse(vehicle)

for (i in 1:length(colnames(accident))) {
	colnames(accident)[i] = toupper(colnames(accident)[i])
}
for (i in 1:length(colnames(accident_node))) {
	colnames(accident_node)[i] = toupper(colnames(accident_node)[i])
}
for (i in 1:length(colnames(accident_chainage))) {
	colnames(accident_chainage)[i] = toupper(colnames(accident_chainage)[i])
}
for (i in 1:length(colnames(accident_event))) {
	colnames(accident_event)[i] = toupper(colnames(accident_event)[i])
}
for (i in 1:length(colnames(accident_location))) {
	colnames(accident_location)[i] = toupper(colnames(accident_location)[i])
}
for (i in 1:length(colnames(atmospheric_cond))) {
	colnames(atmospheric_cond)[i] = toupper(colnames(atmospheric_cond)[i])
}
for (i in 1:length(colnames(node_id_complex_int_id))) {
	colnames(node_id_complex_int_id)[i] = toupper(colnames(node_id_complex_int_id)[i])
}
for (i in 1:length(colnames(person))) {
	colnames(person)[i] = toupper(colnames(person)[i])
}
for (i in 1:length(colnames(road_surface_cond))) {
	colnames(road_surface_cond)[i] = toupper(colnames(road_surface_cond)[i])
}
for (i in 1:length(colnames(subdca))) {
	colnames(subdca)[i] = toupper(colnames(subdca)[i])
}
for (i in 1:length(colnames(vehicle))) {
	colnames(vehicle)[i] = toupper(colnames(vehicle)[i])
}

#Separate day, month and year
accident <- accident %>% mutate(CLEANDATE = as.Date(accident$ACCIDENTDATE,"%d/%m/%y"))
accident <- separate(accident,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)

#Combine Data Sets------
#PERSON
accident_by_person <- merge(person,accident,by="ACCIDENT_NO")
accident_by_person <- merge(accident_by_person, atmospheric_cond, by="ACCIDENT_NO")

glimpse(accident_by_person)

#EVENT
accident_by_event <- merge(accident,accident_location,by="ACCIDENT_NO")
glimpse(accident_by_event)

#Find Number of Unique Values
atmospheric_cond %>%
	summarize(n_distinct(ACCIDENT_NO))
accident_by_event %>% distinct(ROAD_TYPE_INT)

#Create a summary table by year
accident_summary <- accident %>%
	group_by(YEAR) %>%
	tally(name = "NO_ACCIDENTS")

no_vehicles_involved_by_year <- ddply(accident, .(YEAR), summarise, no_vehicles_involved_by_year = sum(NO_OF_VEHICLES))
no_persons_involved_by_year <- ddply(accident, .(YEAR), summarise, no_people_involved_by_year = sum(NO_PERSONS), no_vehicles_involved_by_year = sum(NO_OF_VEHICLES))
no_person_involved_gender <- ddply(accident_by_person, .(YEAR, SEX, ROAD.USER.TYPE.DESC), summarise, gender = length(ACCIDENT_NO))


no_person_involved_gender <- accident_by_person %>%
	group_by(YEAR, SEX, ROAD.USER.TYPE.DESC) %>%
	summarise(sum1)

#merge by year
accident_summary <- merge(accident_summary,no_vehicles_involved_by_year, by ="YEAR")

?ddply

#Crash by year and sex
accident_by_person %>%
	filter(YEAR > "2008")%>%
	filter(YEAR < "2019")%>%
	ggplot(aes(x=YEAR, fill = SEX))+
	geom_bar()+
	labs(title = "Sex of Person involved in Accident")

#Crash by day of week and SEX
accident_by_person %>%
	filter(YEAR > "2008")%>%
	filter(YEAR < "2019")%>%
	ggplot(aes(x=DAY_OF_WEEK, fill = SEX))+
	geom_bar(position = "dodge")+
	facet_wrap(~YEAR)+
	labs(title = "Sex of Person involved by Weekday")

#Crash by year and severity
accident_by_person$SEVERITY <- as.factor(accident_by_person$SEVERITY)
accident_by_event$SEVERITY <- as.factor(accident_by_event$SEVERITY)

accident_by_person %>%
	filter(YEAR > "2008")%>%
	filter(YEAR < "2019")%>%
	ggplot(aes(x=YEAR, fill = SEVERITY))+
	geom_bar(position = "fill")+
	labs(title = "% Severity of Injured Person involved in Accident")

#crashes in wet weather
accident_by_person$ATMOSPH_COND <- as.factor(accident_by_person$ATMOSPH_COND)

accident_by_person %>%
	group_by(YEAR) %>%
	filter(ATMOSPH_COND == 2)%>%
	ggplot(aes(x=YEAR, fill = SEVERITY))+
	geom_bar()+
	labs(title = "Severity of Injured Person involved in Accident when Raining")

accident_by_person %>%
	group_by(YEAR) %>%
	filter(ATMOSPH_COND == 2)%>%
	filter(ROAD_USER_TYPE == 2)%>%
	ggplot(aes(x=YEAR, fill = SEX))+
	geom_bar(position = "fill")+
	labs(title = "When Raining, Driver by SEx")

accident_by_person %>%
	group_by(YEAR) %>%
	filter(ATMOSPH_COND == 2)%>%
	filter(ROAD_USER_TYPE == 2)%>%
	ggplot(aes(x=YEAR, fill = SEX))+
	geom_bar()+
	labs(title = "When Raining, Driver by SEx")

accident_by_person %>%
	group_by(YEAR) %>%
	filter(ATMOSPH_COND == 2)%>%
	filter(ROAD_USER_TYPE == 2)%>%
	ggplot(aes(x=SEX, fill = SEVERITY))+
	geom_bar(position = "fill")+
	labs(title = "When Raining, Driver by Sex and Injury Severity")+
	facet_wrap(~YEAR)

accident_by_person %>%
	group_by(YEAR) %>%
	filter(ATMOSPH_COND == 2)%>%
	filter(SEX == "M")%>%
	ggplot(aes(x=YEAR, fill = SEVERITY))+
	geom_bar(position = "fill")+
	labs(title = "When Raining, Male Injury Severity")+
	facet_wrap(~ROAD.USER.TYPE.DESC)

accident_by_person %>%
	group_by(YEAR) %>%
	filter(ATMOSPH_COND == 2)%>%
	filter(SEX == "F")%>%
	filter(ROAD.USER.TYPE.DESC != "Unknown") %>%
	ggplot(aes(x=YEAR, fill = SEVERITY))+
	geom_bar()+
	labs(title = "When Raining, Female Injury Severity")+
	facet_wrap(~ROAD.USER.TYPE.DESC)

accident_by_person %>%
	group_by(YEAR) %>%
	filter(ATMOSPH_COND == 2)%>%
	filter(ROAD.USER.TYPE.DESC == "Pedestrians")%>%
	filter(SEX != "U") %>%
	ggplot(aes(x=YEAR, fill = SEVERITY))+
	geom_bar(position = "fill")+
	labs(title = "When Raining, Pedestrian Injury Severity")+
	facet_wrap(~SEX)

accident_by_person %>%
	group_by(YEAR) %>%
	filter(ATMOSPH_COND == 2)%>%
	filter(ROAD.USER.TYPE.DESC == "Pedestrians")%>%
	filter(SEX != "U") %>%
	ggplot(aes(x=YEAR, fill = SEVERITY))+
	geom_bar(position = "fill")+
	labs(title = "When Raining, Pedestrian Injury Severity")+
	facet_wrap(~SEX)

accident_by_event %>%
	
	group_by(YEAR) %>%
	ggplot(aes(x=ROAD_TYPE))+
	geom_bar()+
	coord_flip()

accident_by_event %>%
	filter(SPEED_ZONE < 150)%>%
	ggplot(aes(x=SPEED_ZONE))+
	geom_bar()+
	facet_wrap(~YEAR)+
	labs(title = "Severity by Speed Zone")

?renderers

#heatmaps
heatmap_test_m %>%
	ggplot(aes(SPEED_ZONE,AGE))+
	geom_tile(aes(fill = sev.count),
						colour = "white", 
						scale_fill_gradient(low = "white", high = "steelblue"))
heatmap_test_m$SEVERITY <- as.integer(heatmap_test_m$SEVERITY)

no_person_involved_gender <- ddply(accident_by_person, .(YEAR, SEX, ROAD.USER.TYPE.DESC), summarise, gender = length(ACCIDENT_NO))

heatmap_test <- accident_by_person %>%
	select(SPEED_ZONE,AGE,SEVERITY)
heatmap_test_m <- ddply(heatmap_test, .(SPEED_ZONE,AGE), summarise, sev.count = length(SEVERITY))
heatmap_test_m <- as.data.frame(heatmap_test_m)
heatmap_test_m$sev.count <- rescale(heatmap_test_m$sev.count)

?rescale
glimpse(heatmap_test_m)

#Just Drivers
drivers <- accident_by_person %>%
	filter(ROAD_USER_TYPE == 2)

drivers %>%
	summarize(n_distinct(ACCIDENT_NO))

severity_year <- data.table(total_accident$YEAR, total_accident$SEVERITY)

severity_year %>%
	ggplot(aes(x=V1, colour=V2))+
	geom_bar()

total_accident %>%
	group_by(YEAR) %>%
	freq("SEVERITY")

severity_by_year <- total_accident [, list(Count=.N), by=.(YEAR, SEVERITY)]

?data.frame
