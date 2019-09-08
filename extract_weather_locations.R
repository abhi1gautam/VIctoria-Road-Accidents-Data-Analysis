######################################################################
##																																	##
## Extract_weather_location- 																				##
## This script checks the folder "Datasets/Climate" for all`				##
## weather files and extracts latitude and longitude data from			##
## each of these files and populates them to												##
## Computed/climate_location_data.csv																##
##																																	##
##		Script Name: insert_station_info.r														##
##		Created by: Abhisek																						##
##																																	##
##																																	##
######################################################################

library(stringr)

# Get the list of files in the directory Climate/
file_list = list.files(path="Datasets/Climate/", pattern="*.csv")

# Initialize variables
i <- 0
climate_data <- c(StationName=character(),
									Longitude=double(),
									Latitude=double())

# Loop between file names
for (val in file_list){
	climate_file <- read.csv(paste("Datasets/Climate/", val, sep="")) #merge strings
	station_vector <- 
		c(str_extract(val, regex('[:digit:]+')), #extracting station number using the filename with regex
		str_extract(climate_file$metadata[2], regex('[-]?[:digit:]+[.]+[:digit:]*')), #extracting latitude via regex
		str_extract(climate_file$metadata[3], regex('[-]?[:digit:]+[.]+[:digit:]*'))  #extracting longitude via regex
	)
	
	climate_data <- rbind(climate_data, station_vector)
}

# Name the column headers of the matrix
colnames(climate_data) <- c("StationName", "Latitude", "Longitude")

# Write a CSV file in Computed directory
write.csv(climate_data, "Datasets/Computed/climate_location_data.csv")