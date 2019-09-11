
library(sqldf)
library(stringr)

# Load CSVs
accident_station_data <-  read.csv("Datasets/Computed/accident_station_data")
accident_data <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")
climate_file <- read.csv("Datasets/Climate/87006.csv")


#Converting fct to date
accident_data$ACCIDENTDATE <- as.Date(accident_data$ACCIDENTDATE, "%d/%m/%Y")

# Get the list of files in the directory Climate/
file_list = list.files(path="Datasets/Climate/", pattern="*.csv")

# Initialize the dataframe accident_with_rainfall
accident_with_rainfall <- sqldf("select accident_data.*,
						accident_station_data.StationName,
						accident_station_data.min_dist_station_crash,
						climate_file.daily_rain
						from
							accident_data inner join accident_station_data 
							on 
								accident_data.accident_no = accident_station_data.accident_no
							left join climate_file 
							on
								accident_data.ACCIDENTDATE = climate_file.\"YYYY.MM.DD\"
				where 1=0"
	)


# Loop between file names
for (val in file_list){
	climate_file <- read.csv(paste("Datasets/Climate/", val, sep="")) #merge strings
	
 	climate_file$YYYY.MM.DD  <- as.Date (climate_file$YYYY.MM.DD, "%Y-%m-%d")
	
 	accident_with_rainfall <- rbind(accident_with_rainfall,
			sqldf(
			 	paste("select accident_data.*,
									accident_station_data.StationName,
									accident_station_data.min_dist_station_crash,
									climate_file.daily_rain
									from
										accident_data inner join accident_station_data 
										on 
											accident_data.accident_no = accident_station_data.accident_no
										left join climate_file 
										on
											accident_data.ACCIDENTDATE = climate_file.\"YYYY.MM.DD\"
							where accident_station_data.StationName=", str_extract(val, regex('[:digit:]+'))
			 	)
			 )
	)
}


# Write a CSV file in Computed directory
write.csv(accident_with_rainfall, "Datasets/Computed/accident_with_rainfall.csv")






