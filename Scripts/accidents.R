##### Reading files ----
accident_rain <- read.csv("Datasets/Road Crashes/ACCIDENTS WITH RAINFALL.csv")
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

for (i in 1:length(colnames(accident_rain))) {
	colnames(accident_rain)[i] = toupper(colnames(accident_rain)[i])
}
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


accident_rain$DATE <- as.Date(accident_rain$DATE, "%d/%m/%Y")
accident_rain$YEAR <- year(ymd(accident_rain$DATE))

accident <- accident %>% dplyr::rename (DATE = ACCIDENTDATE)
accident_chainage <- accident_chainage %>% dplyr::rename (NODE_ID = NODE.ID)
colnames(accident_node)[colnames(accident_node)=="LGA_NAME"] <- "LGA"
accident$DATE <- as.Date(accident$DATE, "%d/%m/%Y")
accident$YEAR <- year(ymd(accident$DATE))
### Rounding latitude and longitude to 2 decimal places in order to merge with rainfall dataset
accident_node$LAT_RAIN <-  round(accident_node$LAT, 2)
accident_node$LAT_RAIN <-  0.05*(round(accident_node$LAT_RAIN/0.05))
accident_node$LONG_RAIN <-  round(accident_node$LONG, 2)
accident_node$LONG_RAIN <-  0.05*(round(accident_node$LONG_RAIN/0.05))