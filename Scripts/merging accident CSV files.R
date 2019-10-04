#### Merging Data ----
#Creating mergedFiles object, which contains all accidents CSV
#mergedFiles <- merge(person, accident,by="ACCIDENT_NO")
#mergedFiles <- merge(mergedFiles, vehicle, by=c("ACCIDENT_NO","VEHICLE_ID"))
#mergedFiles <- merge(mergedFiles,accident_event,by="ACCIDENT_NO")
#mergedFiles <- merge(mergedFiles, accident_node,by="ACCIDENT_NO")
#mergedFiles <- merge(mergedFiles, accident_location,by="ACCIDENT_NO")
#mergedFiles <- merge(mergedFiles, atmospheric_cond,by="ACCIDENT_NO")
#mergedFiles <- merge(mergedFiles, node_id_complex_int_id,by="ACCIDENT_NO")
#mergedFiles <- merge(mergedFiles, road_surface_cond,by="ACCIDENT_NO")
#mergedFiles <- merge(mergedFiles, subdca,by="ACCIDENT_NO")


#Creating MergedFiltesFiltered for STDS assignment, with fewer columns.
#Attention! When joining with accident_node and accident_location, some information may be lost as we are excluding duplicate rows!
MergedFilesFiltered <- merge(vehicle, accident_rain,by="ACCIDENT_NO")
MergedFilesFiltered <- merge(accident, person,by="ACCIDENT_NO",all=FALSE)
MergedFilesFiltered <- merge(MergedFilesFiltered, vehicle[ , c("ACCIDENT_NO","VEHICLE_ID","VEHICLE_YEAR_MANUF","VEHICLE_TYPE")], by=c("ACCIDENT_NO", "VEHICLE_ID"), all.x = TRUE)
MergedFilesFiltered <- merge(MergedFilesFiltered, accident_location[, c("ACCIDENT_NO","ROAD_TYPE")],by="ACCIDENT_NO",all.x = TRUE)
#MergedFilesFiltered <- merge(MergedFilesFiltered, atmospheric_cond,by="ACCIDENT_NO", all.x = TRUE)
#MergedFilesFiltered <- merge(MergedFilesFiltered, node_id_complex_int_id,by="ACCIDENT_NO", all.x = TRUE)
#MergedFilesFiltered <- merge(MergedFilesFiltered, road_surface_cond,by="ACCIDENT_NO", all.x = TRUE)
#MergedFilesFiltered <- merge(MergedFilesFiltered, subdca,by="ACCIDENT_NO", all.x = TRUE)
#MergedFilesFiltered <- merge(MergedFilesFiltered,accident_event[!duplicated(accident_event$ACCIDENT_NO), ],by="ACCIDENT_NO", all.x = TRUE)
MergedFilesFiltered <- merge(accident, accident_node [!duplicated(accident_node$ACCIDENT_NO), c("ACCIDENT_NO","LGA","LAT","LONG","LAT_RAIN","LONG_RAIN")],by="ACCIDENT_NO", all.x = TRUE)
MergedFilesFiltered <- merge(MergedFilesFiltered, population[ , c("YEAR","LGA","TOTAL_POPULATION","PERSONS/KM2")],by=c("YEAR","LGA"), all.x = TRUE)

csvfileexport <- "Vehicle Data Cleaned"
write.table(accident_veh, csvfileexport, row.names = FALSE, sep = ",")



MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2006, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2006 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2007, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2007 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2008, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2008 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2009, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2009 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2010, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2010 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2011, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2011 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2012, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2012 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2013, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2013 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2014, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2014 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2015, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2015 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2016, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2016 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2017, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2017 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2018, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2018 = RAINFALL)
MergedFilesFiltered <- merge(MergedFilesFiltered, daily_rain.df2019, by.x=c("LAT_RAIN","LONG_RAIN","DATE"), by.y=c("LAT","LONG","DATE"), all.x = TRUE)
MergedFilesFiltered <- MergedFilesFiltered %>% dplyr::rename (RAINFALL_2019 = RAINFALL)


MergedFilesFiltered$Rainfall <- coalesce(MergedFilesFiltered$RAINFALL_2006,MergedFilesFiltered$RAINFALL_2007,MergedFilesFiltered$RAINFALL_2008, MergedFilesFiltered$RAINFALL_2009, MergedFilesFiltered$RAINFALL_2010, MergedFilesFiltered$RAINFALL_2011, MergedFilesFiltered$RAINFALL_2012, MergedFilesFiltered$RAINFALL_2013, MergedFilesFiltered$RAINFALL_2014, MergedFilesFiltered$RAINFALL_2015, MergedFilesFiltered$RAINFALL_2016, MergedFilesFiltered$RAINFALL_2017, MergedFilesFiltered$RAINFALL_2018, MergedFilesFiltered$RAINFALL_2019)
MergedFilesFiltered <- subset(MergedFilesFiltered, select = -c(RAINFALL_2006,RAINFALL_2007,RAINFALL_2008,RAINFALL_2009,RAINFALL_2010,RAINFALL_2011,RAINFALL_2012,RAINFALL_2013,RAINFALL_2014,RAINFALL_2015,RAINFALL_2016,RAINFALL_2017,RAINFALL_2018,RAINFALL_2019 ))


#The “person_id” and the “vehicle_id” fields are also joining keys between the vehicle and person tables (eg for finding which person was in which vehicle in the accident).
#For locational data the accident_no field is the critical joining key and then the ”node_id” field enables joining to the “accident_chainage” table

#remove duplicate column names
#mergedFiles <- mergedFiles[,!duplicated(colnames(mergedFiles))]

