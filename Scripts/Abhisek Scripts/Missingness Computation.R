library(tidyverse)
library(lmtest)
library(Amelia)
library(mlbench)
library(sqldf)
library(dplyr)

dat <- read.csv("Datasets/Computed/Vehicle Data Uncleaned.csv")

#dat$CASUALTIES <- with(dat, NO_PERSONS_KILLED + NO_PERSONS_INJ_2)
#dat$NO_BELT_HELMET <- with(dat, HELMET_BELT_WORN1 + HELMET_BELT_WORN3)

dat$DATE <- as.Date(dat$DATE, '%d/%m/%Y')

#toChange <-!(dat$ROAD_TYPE %in% c("ROAD", "STREET", "FREEWAY", "HIGHWAY"))
#tmp <- as.character(dat$ROAD_TYPE)
#tmp[toChange] <- "OTHER"
#dat$ROAD_TYPE <- factor(tmp)

dat$CAR_AGE <- with(dat, dat$YEAR - dat$VEHICLE_YEAR_MANUF)

dat$CAR_AGE[dat$CAR_AGE >= 100] <- NA
dat$RAINFALL[dat$RAINFALL < 0] <- NA
dat$SPEED_ZONE[dat$SPEED_ZONE >= 700] <- NA
dat$SEVERITY[dat$SEVERITY >= 4] <- NA
dat$LIGHT_CONDITION[dat$LIGHT_CONDITION ==9] <- NA
dat$ROAD_SURFACE_TYPE[dat$ROAD_SURFACE_TYPE ==9] <- NA
dat$DRIVER.AGE[dat$DRIVER.AGE == 'unknown'] <-NA
dat$DRIVER.SEX[dat$DRIVER.SEX == 'U'] <-NA


final_dat <- sqldf("select TOTAL_CASUALTIES, 
						TOTAL_NO_OCCUPANTS AS NO_OCCUPANTS,
						\"ACCIDENT.TYPE.DESC\" as ACCIDENT_TYPE,
             LIGHT_CONDITION,
             SPEED_ZONE,
             RAINFALL,
             NO_OF_VEHICLES,
             CAR_AGE,
             \"Vehicle.Type.Desc\",
             ROAD_SURFACE_TYPE,
             VEHICLE_WEIGHT,
						SEVERITY,
						SPEED_ZONE,
						\"DRIVER.AGE\",
						\"DRIVER.SEX\",
						ROAD_TYPE,
						\"SURFACE.DESC\"
						from dat")

missmap(final_dat, col=c("red", "blue"),
				legend=TRUE, x.cex=0.8, margins=c(8, 3))


#
#
#
#
#
#Cleaned starting
#
#
#
#
#
#






dat <- read.csv("Datasets/Computed/Vehicle Data Cleaned.csv")

#dat$CASUALTIES <- with(dat, NO_PERSONS_KILLED + NO_PERSONS_INJ_2)
#dat$NO_BELT_HELMET <- with(dat, HELMET_BELT_WORN1 + HELMET_BELT_WORN3)
dat$LIGHT_CONDITION <- as.factor(dat$LIGHT_CONDITION)
dat$LIGHT_CONDITION[dat$LIGHT_CONDITION=="4"] <- "5"
dat$LIGHT_CONDITION[dat$LIGHT_CONDITION=="6"] <- "5"
dat$PERSONS.KM2 <- as.numeric(dat$PERSONS.KM2)
dat$VEHICLE.TYPE.DESC <- as.factor(dat$VEHICLE.TYPE.DESC)
dat$Vehicle.Type.Desc <- as.factor(dat$Vehicle.Type.Desc)
dat$ROAD_SURFACE_TYPE <- as.factor(dat$ROAD_SURFACE_TYPE)

dat$WET <- dat$RAINFALL
dat$WET[dat$RAINFALL>=1.5] <- 1
dat$WET[dat$RAINFALL<1.5] <- 0

dat$RAINFALL[dat$RAINFALL < 0] <- 0
dat <- filter(dat, SPEED_ZONE < 700, SEVERITY < 4, LIGHT_CONDITION != "9", ROAD_SURFACE_TYPE != "9")
dat <- filter(dat, !Vehicle.Type.Desc %in% c('Train', 'Tram', 'Quad Bike', 'Prime Mover (No of Trailers Unknown)', 'Horse (ridden or drawn)'))

glimpse(dat)

mean(dat$TOTAL_CASUALTIES)
var(dat$TOTAL_CASUALTIES)

dat$DATE <- as.Date(dat$DATE, '%d/%m/%Y')

#toChange <-!(dat$ROAD_TYPE %in% c("ROAD", "STREET", "FREEWAY", "HIGHWAY"))
#tmp <- as.character(dat$ROAD_TYPE)
#tmp[toChange] <- "OTHER"
#dat$ROAD_TYPE <- factor(tmp)

dat$CAR_AGE <- with(dat, dat$YEAR - dat$VEHICLE_YEAR_MANUF)
dat$CAR_AGE[dat$CAR_AGE > 100] <- 0 

dat2014 <- dat[format(dat$DATE, '%Y') == "2014", ]
dat2015 <- dat[format(dat$DATE, '%Y') == "2015", ]
dat2016 <- dat[format(dat$DATE, '%Y') == "2016", ]
dat2017 <- dat[format(dat$DATE, '%Y') == "2017", ]
dat2018 <- dat[format(dat$DATE, '%Y') == "2018", ]
dat201318 <- dat[format(dat$DATE, '%Y') %in% c("2013", "2014", "2015", "2016", "2017","2018"), ]
dat201618 <- dat[format(dat$DATE, '%Y') %in% c("2016", "2017","2018"), ]
dat201518 <- dat[format(dat$DATE, '%Y') %in% c("2015", "2016", "2017","2018"), ]



final_dat201518 <- select(dat201518, -c(Vehicle.Type.Desc))

final_dat201518 <- sqldf("select TOTAL_CASUALTIES, TOTAL_NO_OCCUPANTS, \"ACCIDENT.TYPE.DESC\",
             LIGHT_CONDITION,
             SPEED_ZONE,
             RAINFALL,
             NO_OF_VEHICLES,
             SECURITY_EQUIPS_NOT_WORN,
             ROAD_TYPE_GROUP,
             DRIVER_AGE,
             DRIVER_SEX,
             CAR_AGE,
             \"Vehicle.Type.Desc\"
             ROAD_SURFACE_TYPE,
             WET,
             OLD_COUNT from final_dat201518")

missmap(final_dat201518, col=c("red", "blue"),
				legend=TRUE, x.cex=0.5, margins=c(8, 3))