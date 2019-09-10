library(tidyverse)
library(MASS)
library(ggplot2)
library(effects)
library(splines)
library(car)

dat <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")

weather <- read.csv("Datasets/Road Crashes/ROAD_SURFACE_COND.csv")
wthr <- weather %>% mutate(yesno = 1) %>% distinct %>% spread(Surface.Cond.Desc, yesno, fill = 0)
wthr <- dplyr::select(wthr,-c(SURFACE_COND,SURFACE_COND_SEQ))

dat1 <- filter(dat, SPEED_ZONE < 700, SEVERITY < 4)
dat1 <- merge(dat1, wthr, by = "ACCIDENT_NO")

dat1$SEVERITY <- as.factor(dat1$SEVERITY)

mdl <- polr(SEVERITY ~ Wet + SPEED_ZONE + NO_OF_VEHICLES + NO_PERSONS, data = dat1, Hess = TRUE)

summary(mdl)

summary_table <- coef(summary(mdl))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

Anova(mdl)

Effect(focal.predictors = c("Wet","SPEED_ZONE"), mdl)

plot(Effect(focal.predictors = c("Wet","SPEED_ZONE"), mdl), rug = FALSE)
