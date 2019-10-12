library(tidyverse)
library(lmtest)
library(car)

dat <- read.csv("Final Dataset.csv")

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

dat$DATE <- as.Date(dat$DATE, '%d/%m/%Y')

dat$CAR_AGE <- with(dat, dat$YEAR - dat$VEHICLE_YEAR_MANUF)
dat$CAR_AGE[dat$CAR_AGE > 100] <- 0 

dat2017 <- dat[format(dat$DATE, '%Y') == "2017", ]
dat2018 <- dat[format(dat$DATE, '%Y') == "2018", ]
dat2019 <- dat[format(dat$DATE, '%Y') == "2019", ]
dat201318 <- dat[format(dat$DATE, '%Y') %in% c("2013", "2014", "2015", "2016", "2017","2018"), ]

mdlMakerRain <- function(dataset){
  result <- glm(TOTAL_CASUALTIES/TOTAL_NO_OCCUPANTS ~ ACCIDENT.TYPE.DESC +
             LIGHT_CONDITION + 
             SPEED_ZONE + 
             RAINFALL + 
             NO_OF_VEHICLES + 
             SECURITY_EQUIPS_NOT_WORN + 
             ROAD_TYPE_GROUP + 
             DRIVER_AGE + 
             DRIVER_SEX + 
             CAR_AGE + 
             Vehicle.Type.Desc +
             ROAD_SURFACE_TYPE,
           family = binomial, data = dataset, weights = TOTAL_NO_OCCUPANTS)
  return(result)
}

mdlMakerWet <- function(dataset){
  result <- glm(TOTAL_CASUALTIES/TOTAL_NO_OCCUPANTS ~ ACCIDENT.TYPE.DESC +
                  LIGHT_CONDITION + 
                  SPEED_ZONE + 
                  WET + 
                  NO_OF_VEHICLES + 
                  SECURITY_EQUIPS_NOT_WORN + 
                  ROAD_TYPE_GROUP + 
                  CAR_AGE + 
                  Vehicle.Type.Desc + 
                  OLD_COUNT + 
                  ROAD_SURFACE_TYPE,
                family = binomial, data = dataset, weights = TOTAL_NO_OCCUPANTS)
  return(result)
}

mdlRain2018 <- mdlMakerRain(dat2018)

summary(mdlRain2018)

## Then do a comparison of rainfall, sex and 70s removed

mdlWet2018 <- mdlMakerWet(dat2018)

summary(mdlWet2018)

#anova(mdlRain2018, mdlWet201318, test = "Chisq")

# Testing on 2019
#sampleSize <- floor(0.1 * nrow(dat))
#set.seed(123)
#trainingIndex <- sample(seq_len(nrow(dat)), size = sampleSize)
#train <- dat[trainingIndex, ]
#test <- dat[-trainingIndex, ]


# Prediction
tester <- data.frame(ACCIDENT.TYPE.DESC  = "Collision with vehicle",
                     LIGHT_CONDITION = "1", 
                     SPEED_ZONE = 80,
                     WET = 1,
                     NO_OF_VEHICLES = 2,
                     SECURITY_EQUIPS_NOT_WORN = 1,
                     ROAD_TYPE_GROUP = "HIGHWAY",
                     CAR_AGE = 2,
                     Vehicle.Type.Desc = "Car",
                     OLD_COUNT = 1,
                     ROAD_SURFACE_TYPE = "3")

prd <-predict(mdlWet2018, tester, type = "response")
prd

prd2 <- predict(mdlWet2018, type = "response")

mydata <- dat2018 %>% dplyr::select_if(is.numeric)
predictors <- colnames(mydata)

# Multicollinearity Check
car::vif(mdlWet2018)

# Linearity plots
mydata <- mydata %>%
  mutate(logit = log(prd2/(1-prd2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
