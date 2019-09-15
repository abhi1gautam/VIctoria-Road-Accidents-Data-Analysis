##### Reading Libraries ----
library(tidyverse)
library(lubridate)
glimpse(mergedFiles)

##### Reading files ----
#Separate month and year
accident <- accident %>% mutate(CLEANDATE = as.Date(accident$ACCIDENTDATE,"%d/%m/%y"))
accident <- separate(accident,"ACCIDENTDATE",c("DAY", "MONTH", "YEAR"), remove = FALSE)
glimpse(accident)

#Combine all
total_accident <- merge(person,accident,by="ACCIDENT_NO")

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