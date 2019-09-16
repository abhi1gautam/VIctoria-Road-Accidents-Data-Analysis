##### Reading Libraries ----
library(tidyverse)
library(lubridate)
library(plyr)
library(AMR)
library(data.table)
glimpse(total_accident)

##### Reading files ----
#Separate month and year
accident <- accident %>% mutate(CLEANDATE = as.Date(accident$ACCIDENTDATE,"%d/%m/%y"))
accident <- separate(accident,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)
glimpse(accident)

#Combine all
total_accident <- merge(person,accident,by="ACCIDENT_NO")
total_accident <- merge(total_accident, atmospheric_cond, by="ACCIDENT_NO")

#Missing Values
library(Amelia)
missmap(mergedFiles, main = "Missing values vs observed")

#Crash by year and severity
total_accident %>%
	group_by(YEAR)%>%
	filter(YEAR > "2008")%>%
	filter(YEAR < "2019")%>%
	ggplot(aes(x=YEAR,))+
	geom_bar()

#crashes in wet weather
total_accident %>%
	group_by(YEAR) %>%
	summarise(count_severity = count("SEVERITY"))

severity_year <- data.table(total_accident$YEAR, total_accident$SEVERITY)

severity_year %>%
	ggplot(aes(x=V1))+
	geom_bar()

total_accident %>%
	group_by(YEAR) %>%
	freq("SEVERITY")

severity_by_year <- total_accident [, list(Count=.N), by=.(YEAR, SEVERITY)]
