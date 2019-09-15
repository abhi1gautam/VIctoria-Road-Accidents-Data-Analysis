##### Reading Libraries ----
library(tidyverse)
library(lubridate)
glimpse(mergedFiles)

##### Reading files ----
#Separate month and year
accident <- accident %>% mutate(CLEANDATE = as.Date(accident$ACCIDENTDATE,"%d/%m/%y"))
accident <- separate(accident,"ACCIDENTDATE",c("Day", "Month", "Year"))
glimpse(accident)

#Missing Values
library(Amelia)
missmap(mergedFiles, main = "Missing values vs observed")

#Crash by year
mergedFiles %>%
	group_by(Year)%>%
	summarise
	ggplot(aes(x=Year, y=))+
	geom_col