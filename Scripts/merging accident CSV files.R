library(tidyverse)

##### Reading files ----
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

##### TRANSFORM TO UPPERCASE ALL COLUMNS ----
for (i in 1:length(colnames(accident))) {
	colnames(accident)[i] = toupper(colnames(accident)[i])
	# return()     ... you don't need this
}
for (i in 1:length(colnames(accident_node))) {
	colnames(accident_node)[i] = toupper(colnames(accident_node)[i])
	# return()     ... you don't need this
}
for (i in 1:length(colnames(accident_chainage))) {
	colnames(accident_chainage)[i] = toupper(colnames(accident_chainage)[i])
	# return()     ... you don't need this
}
for (i in 1:length(colnames(accident_event))) {
	colnames(accident_event)[i] = toupper(colnames(accident_event)[i])
	# return()     ... you don't need this
}
for (i in 1:length(colnames(accident_location))) {
	colnames(accident_location)[i] = toupper(colnames(accident_location)[i])
	# return()     ... you don't need this
}
for (i in 1:length(colnames(atmospheric_cond))) {
	colnames(atmospheric_cond)[i] = toupper(colnames(atmospheric_cond)[i])
	# return()     ... you don't need this
}
for (i in 1:length(colnames(node_id_complex_int_id))) {
	colnames(node_id_complex_int_id)[i] = toupper(colnames(node_id_complex_int_id)[i])
	# return()     ... you don't need this
}
for (i in 1:length(colnames(person))) {
	colnames(person)[i] = toupper(colnames(person)[i])
	# return()     ... you don't need this
}
for (i in 1:length(colnames(road_surface_cond))) {
	colnames(road_surface_cond)[i] = toupper(colnames(road_surface_cond)[i])
	# return()     ... you don't need this
}
for (i in 1:length(colnames(subdca))) {
	colnames(subdca)[i] = toupper(colnames(subdca)[i])
	# return()     ... you don't need this
}
for (i in 1:length(colnames(vehicle))) {
	colnames(vehicle)[i] = toupper(colnames(vehicle)[i])
	# return()     ... you don't need this
}

accident_chainage <- accident_chainage %>% rename (NODE_ID = NODE.ID)
mergedFiles <- accident
#### Merging Data ----
#Creating mergedFiles object, which contains all accidents CSV
mergedFiles <- merge(accident,accident_event,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, accident_node,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, accident_location,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, atmospheric_cond,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, node_id_complex_int_id,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, person,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, road_surface_cond,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, subdca,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, vehicle, by="ACCIDENT_NO")


#The “person_id” and the “vehicle_id” fields are also joining keys between the vehicle and person tables (eg for finding which person was in which vehicle in the accident).
#For locational data the accident_no field is the critical joining key and then the ”node_id” field enables joining to the “accident_chainage” table
