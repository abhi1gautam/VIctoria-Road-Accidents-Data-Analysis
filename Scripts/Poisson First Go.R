library(tidyverse)

dat <- read.csv("dummySet.csv")

dat$CASUALTIES <- with(dat, NO_PERSONS_KILLED + NO_PERSONS_INJ_2)
dat$LIGHT_CONDITION <- as.factor(dat$LIGHT_CONDITION)

glimpse(dat)

mean(dat$CASUALTIES)
var(dat$CASUALTIES)

dat$DATE <- as.Date(dat$DATE, '%d/%m/%Y')
dat2018 <- dat[format(dat$DATE, '%Y') == "2018", ]

popn <- log(dat2018$LGA_POPULATION)

dat2018 <- cbind(dat2018,popn)

mdl <- glm(CASUALTIES ~ Accident.Type.Desc + Day.Week.Description + LIGHT_CONDITION + SPEED_ZONE + WET + 
             offset(popn), family = poisson(link="log"), data = dat2018)

#fitted(mdl)
summary(mdl)

test <- data.frame(Accident.Type.Desc = "Collision with vehicle", Day.Week.Description = "Saturday", LIGHT_CONDITION = "5", 
                   SPEED_ZONE = 100, WET = 1, popn = log(100000))

prd <-predict(mdl, test, type = "response")
