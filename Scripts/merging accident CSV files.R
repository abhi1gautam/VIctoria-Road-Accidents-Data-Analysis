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

accident_chainage <- accident_chainage %>% rename (NODE_ID = NODE.ID)

#### Merging Data ----
#Creating mergedFiles object, which contains all accidents CSV
mergedFiles <- merge(person, accident,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, vehicle, by=c("ACCIDENT_NO","VEHICLE_ID"))
mergedFiles <- merge(mergedFiles,accident_event,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, accident_node,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, accident_location,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, atmospheric_cond,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, node_id_complex_int_id,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, road_surface_cond,by="ACCIDENT_NO")
mergedFiles <- merge(mergedFiles, subdca,by="ACCIDENT_NO")

#Creating MergedFiltesFiltered for STDS assignment, with fewer columns.
#Attention! When joining with accident_node and accident_location, some information may be lost as we are excluding duplicate rows!
MergedFilesFiltered <- merge(person, accident,by="ACCIDENT_NO")
MergedFilesFiltered <- merge(MergedFilesFiltered, vehicle[ , c("ACCIDENT_NO","VEHICLE_ID","VEHICLE_YEAR_MANUF","VEHICLE_TYPE")], by=c("ACCIDENT_NO", "VEHICLE_ID"), all.x = TRUE)
MergedFilesFiltered <- merge(MergedFilesFiltered, accident_node [!duplicated(accident_node$ACCIDENT_NO), c("ACCIDENT_NO","LGA_NAME")],by="ACCIDENT_NO", all.x = TRUE)
MergedFilesFiltered <- merge(MergedFilesFiltered, accident_location[, c("ACCIDENT_NO","ROAD_TYPE")],by="ACCIDENT_NO",all.x = TRUE)
#MergedFilesFiltered <- merge(MergedFilesFiltered, atmospheric_cond,by="ACCIDENT_NO", all.x = TRUE)
#MergedFilesFiltered <- merge(MergedFilesFiltered, node_id_complex_int_id,by="ACCIDENT_NO", all.x = TRUE)
#MergedFilesFiltered <- merge(MergedFilesFiltered, road_surface_cond,by="ACCIDENT_NO", all.x = TRUE)
#MergedFilesFiltered <- merge(MergedFilesFiltered, subdca,by="ACCIDENT_NO", all.x = TRUE)
#MergedFilesFiltered <- merge(MergedFilesFiltered,accident_event[!duplicated(accident_event$ACCIDENT_NO), ],by="ACCIDENT_NO", all.x = TRUE)


#The “person_id” and the “vehicle_id” fields are also joining keys between the vehicle and person tables (eg for finding which person was in which vehicle in the accident).
#For locational data the accident_no field is the critical joining key and then the ”node_id” field enables joining to the “accident_chainage” table

#remove duplicate column names
mergedFiles <- mergedFiles[,!duplicated(colnames(mergedFiles))]
