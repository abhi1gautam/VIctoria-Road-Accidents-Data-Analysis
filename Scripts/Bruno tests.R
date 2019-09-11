library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)

daily_rain <- "daily_rain"
ncfname <- paste(daily_rain, ".nc", sep = "")
dname <- "daily_rain"  # note: tmp means temperature (not temporary)
ncin <- nc_open(ncfname)
print(ncin)
lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
head(lon)
lat <- ncvar_get(ncin, "lat", verbose = F)
nlat <- dim(lat)
head(lat)
print(c(nlon, nlat))
t <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(t)
daily_rain.array <- ncvar_get(ncin, dname)
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin, dname, "_FillValue")
dim(daily_rain.array)
title <- ncatt_get(ncin, 0, "title")
institution <- ncatt_get(ncin, 0, "institution")
datasource <- ncatt_get(ncin, 0, "source")
references <- ncatt_get(ncin, 0, "references")
history <- ncatt_get(ncin, 0, "history")
Conventions <- ncatt_get(ncin, 0, "Conventions")
nc_close(ncin)
# split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
chron(t, origin = c(tmonth, tday, tyear))
daily_rain.array[daily_rain.array == fillvalue$value] <- NA
length(na.omit(as.vector(daily_rain.array[, , 1])))
m <- 1
daily_rain.slice <- daily_rain.array[, , m]
image(lon, lat, daily_rain.slice, col = rev(brewer.pal(10, "RdBu")))


grid <- expand.grid(lon = lon, lat = lat)
cutpts <- c(-50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50)
levelplot(daily_rain.slice ~ lon * lat, data = grid, at = cutpts, cuts = 11, pretty = T, 
					col.regions = (rev(brewer.pal(10, "RdBu"))))

lonlat <- expand.grid(lon, lat)
daily_rain.vec <- as.vector(daily_rain.slice)
length(daily_rain.vec)
daily_rain.df01 <- data.frame(cbind(lonlat, daily_rain.vec))
names(daily_rain.df01) <- c("lon", "lat", paste(dname, as.character(m), sep = "_"))
head(na.omit(daily_rain.df01), 20)
csvfile <- "cru_daily_rain_1.csv"
write.table(na.omit(daily_rain.df01), csvfile, row.names = FALSE, sep = ",")
