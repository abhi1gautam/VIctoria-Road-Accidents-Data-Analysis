library(stringr)
file_list = list.files(path="Datasets/Climate/", pattern="*.csv")

i <- 0
climate_data <- c(StationName=character(),
									Longitude=double(),
									Latitude=double())

for (val in file_list){
	climate_file <- read.csv(paste("Datasets/Climate/", val, sep=""))
	station_vector <- 
		c(str_extract(val, regex('[:digit:]+')),
		str_extract(climate_file$metadata[2], regex('[-]?[:digit:]+[.]+[:digit:]*')),
		str_extract(climate_file$metadata[3], regex('[-]?[:digit:]+[.]+[:digit:]*'))
	)
	
	climate_data <- rbind(climate_data, station_vector)
}

colnames(climate_data) <- c("StationName", "Latitude", "Longitude")

write.csv(climate_data, "Datasets/Computed/climate_location_data.csv")