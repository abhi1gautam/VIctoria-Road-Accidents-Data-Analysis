
#### Merging Data ----
#Creating mergedFiles object, which contains accident and accident location
mergedFiles <- merge(accident,accident_node[!duplicated(accident_node$ACCIDENT_NO), ],by="ACCIDENT_NO")


#Merging the Accident tables with the rainfall data


# Combining all rainfall data in one column

# Export to CSV
acccsvfile <- "Accident_with_Rainfall"
write.table(mergedFiles, acccsvfile, row.names = FALSE, sep = ",")


 