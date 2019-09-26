# This script reads the rainfall.nc data and reduce it to the shape of Victoria
# Improvements should be done: Loop to read all files at once/read dynamic variables

#### Libraries
library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)
library (tidyverse)
library(sp)
library(rgdal)


# Rename your daily_rainfall.nc file to year.nc (i.e 2017.nc 2018.nc ...)
# Make sure to set working directory to read above files (Session > Set Working Directory ...)
	#Reading file
	i<- 2018 
	daily_rain <- i
	ncfname <- paste(daily_rain, ".nc", sep = "")
	dname <- "daily_rain"
	ncin <- nc_open(ncfname)

	# Getting the longitude indexes which longitude values are between 140.9 and 149.8 (Victoria State)
	lonIdx <- which( ncin$dim$lon$vals > 140.9 & ncin$dim$lon$vals < 149.8) ##
	max(lonIdx)
	# Reading longitude values. As the LonIdx varies from 1 (lon 140) to 199 (lon 150), we want 
	#to read the 199 next values, starting at index 562 (which is lon 140)
	lon <- ncvar_get(ncin, "lon", start=c(min(lonIdx)), count=c((max(lonIdx)-min(lonIdx)+1)))
	nlon <- dim(lon)

	# Getting the latitude indexes which latitude values are between -39.10 and -34 (Victoria State)
	latIdx  <- which( ncin$dim$lat$vals > -39.10 & ncin$dim$lat$vals < -34)
	
	# Reading latitude values.
	lat <- ncvar_get(ncin, "lat", verbose = F, start=c(min(latIdx)), count=c((max(latIdx)-min(latIdx)+1)))
	nlat <- dim(lat)

	# Reading time values (dates)
	t <- ncvar_get(ncin, "time")
	tunits <- ncatt_get(ncin, "time", "units")
	nt <- dim(t)
	
	# Reading daily_rain data, filtering the longitude and latidude values (Otherwise all daily_rain data from Australia will be returned)
	daily_rain.array <- ncvar_get( ncin, dname, start=c(min(lonIdx), min(latIdx),min(t)+1), count=c((max(lonIdx)-min(lonIdx)+1),(max(latIdx)-min(latIdx)+1),(max(t)-min(t)+1)))
	dlname <- ncatt_get(ncin, dname, "long_name")
	dunits <- ncatt_get(ncin, dname, "units")
	fillvalue <- ncatt_get(ncin, dname, "_FillValue")

	# Getting global attributes
	#	title <- ncatt_get(ncin, 0, "title")
	#	institution <- ncatt_get(ncin, 0, "institution")
	#	references <- ncatt_get(ncin, 0, "references")
	
	# Close the NetCDF file using the nc_close() function.
	nc_close(ncin)
	
	# split the time units string into fields
	tustr <- strsplit(tunits$value, " ")
	tdstr <- strsplit(unlist(tustr)[3], "-")
	tmonth = as.integer(unlist(tdstr)[2])
	tday = as.integer(unlist(tdstr)[3])
	tyear = as.integer(unlist(tdstr)[1])
	chron(t, origin = c(tmonth, tday, tyear))
	
	# Replace NetCDF fillvalues with R NAs
	daily_rain.array[daily_rain.array == fillvalue$value] <- NA

	# Getting a single time slice of the data (only one day), create an R data frame, and write a .csv file
  #m <- 1
	#daily_rain.slice2 <- daily_rain.df2018[, , m]
	
	# Visualization
	#grid <- expand.grid(lon = lon, lat = lat)
	#cutpts <- c(-50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50)
	#levelplot(daily_rain.slice ~ lon * lat, data = grid, at = cutpts, cuts = 11, pretty = T, 
	#					col.regions = (rev(brewer.pal(10, "RdBu"))))
	
	# Expand.grid() function is used to create pairs of values of longtiude and latitude
	# As.vector() function is used to “unstack” the slice of data into a long vector,
 #	lonlat <- expand.grid(lon, lat)
 #	daily_rain.vec <- as.vector(daily_rain.slice)
 #	length(daily_rain.vec)
	
	
	# The data.frame() and cbind() functions are used to assemble the columns of the data frame, which are assigned appropriate names using the names() function (on the left-hand side of assignment operator). The head() function, applied on top of the na.omit() function lists the first rows of values without NAs:
	#daily_rain.df01 <- data.frame(cbind(lonlat, daily_rain.vec))
	#names(daily_rain.df01) <- c("lon", "lat", paste(dname, as.character(m), sep = "_"))
	#head(na.omit(daily_rain.df01), 20)

	# Converting the whole array to a data frame
	#Convert the nlon by nlat by nt array into a nlon by nlat by nt matrix
	daily_rain.vec.long <- as.vector(daily_rain.array)
	
	# Reshape vector into  a matrix using the matrix() function
	daily_rain.mat <- matrix(daily_rain.vec.long, nrow = nlon * nlat, ncol = nt)
	
	# Create the second data frame from the daily_rain.mat matrix.
	lonlat <- expand.grid(lon, lat)
	nam <- paste("daily_rain.df", i, sep = "")
	assign(nam, data.frame(cbind(lonlat, daily_rain.mat)))
	options(width = 110)
  
	#Rename Var1 and Var2 columns to Long and Lat
  daily_rain.df2018 <- daily_rain.df2018 %>% rename (Lat = Var2)
  daily_rain.df2018 <- daily_rain.df2018 %>% rename (Long = Var1)
  
  #Reading Victoria shapefile
  wa.map <- readOGR(dsn = "Standard", layer = "VIC_STATE_POLYGON_shp")
  #Transforming the dataframe into a SpatialPointsDataFrame 
  coordinates(daily_rain.df2018) <- ~ Long + Lat
  proj4string(daily_rain.df2018) <- proj4string(wa.map)
  
  # Shaping the SpatialPointsDataFrame to Victoria shape
    daily_rain.df2018 <- daily_rain.df2018[!is.na(over(daily_rain.df2018, as(wa.map, "SpatialPolygons"))), ]
#  plot(daily_rain.df2018)
  
   # Transforming the SpatialPointsDataFrame to Data Frame
   daily_rain.df2018 <- as.data.frame(daily_rain.df2018)
   # Renaming the columns to Date format
   names(daily_rain.df2018)[3:(max(t)+3)] <- format(seq(as.Date("2018/01/01"), by = "day", length.out = (max(t)+1)))
   
   #Tidying data, transforming date columns into a single column
  daily_rain.df2018 <- as.data.frame(daily_rain.df2018) %>%
	tidyr::gather(key = DATE, value = Rainfall, -Long, -Lat)
  
  # Formating Date field as Date
  daily_rain.df2018$DATE  <- as.Date(daily_rain.df2018$DATE, "%Y-%m-%d")
  
  # Export to CSV
 csvfile2 <- "cru_daily_rain_2018.csv"
 write.table(na.omit(daily_rain.df2018), csvfile2, row.names = FALSE, sep = ",")
 