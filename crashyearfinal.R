library(readr)
library(tidyverse)
library(esquisse)
library(ggplot2)

ACCIDENT <- read_csv("Datasets/Road Crashes/ACCIDENT.csv")



acc <- ACCIDENT


test1 <- acc %>% mutate(cleandate = as.Date(acc$ACCIDENTDATE, "%d/%m/%Y"))


compactTable <- test1 %>% separate(cleandate,c("year","month","day"),remove=FALSE)



PERSON <- read_csv("Datasets/Road Crashes/PERSON.csv")



tog <- merge(compactTable , PERSON ,by="ACCIDENT_NO" )


tog$INJ_LEVEL[tog$INJ_LEVEL==1] <- "Fatal Injury"
tog$INJ_LEVEL[tog$INJ_LEVEL==2] <- "Serious Injury"
tog$INJ_LEVEL[tog$INJ_LEVEL==3] <- "Minor Injury"
tog$INJ_LEVEL[tog$INJ_LEVEL==4] <- "No Injury"




library(plyr)
counts <- ddply(tog, .(tog$year, tog$INJ_LEVEL), nrow)
names(counts) <- c("Year", "INJURY_LEVEL", "counts")



counts$INJURY_LEVEL <- factor(counts$INJURY_LEVEL , ordered=TRUE , levels = c("No Injury","Minor Injury","Serious Injury", "Fatal Injury"))


factor(counts$INJURY_LEVEL)



p4 <- ggplot() + geom_bar(aes(y = counts, x = Year, fill = INJURY_LEVEL ), data =counts,
													stat="identity")


p5 <- p4 + geom_text(data = counts, aes(x = Year , y =counts, label = paste0(counts)) , size=5, position=position_stack(0.15)) + scale_fill_manual(values = c("Green","Yellow", "Orange", "Red"))



p5




