library(tidyverse)
library(rgdal)
library(readxl)
library(raster)
library(maptools)
library(janitor)
library(tmap)
library(data.table)

population <- read_xls("Datasets/Population/victoria_population_by_LGA_2011-18.xls" , skip = 8)
population <- population[,c(1,2,3,69,78)]
colnames(population)[colnames(population)=="no....69"] <- "TOTAL_POPULATION"
colnames(population)[colnames(population)=="LABEL"] <- "LGA"
colnames(accident)[colnames(accident)=="LGA_NAME"] <- "LGA"
for (i in 1:length(colnames(population))) {
	colnames(population)[i] = toupper(colnames(population)[i])
}


population[population=="Greater Dandenong (C)"] <- "DANDENONG"
population[population=="Casey (C)"] <- "CASEY"
population[population=="Mornington Peninsula (S)"] <- "MORNINGTON PENINSULA"
population[population=="Greater Dandenong (C)"] <- "DANDENONG"
population[population=="Baw Baw (S)"] <- "BAW BAW"
population[population=="Frankston (C)"] <- "FRANKSTON"
population[population=="Wyndham (C)"] <- "WYNDHAM"
population[population=="East Gippsland (S)"] <- "EAST GIPPSLAND"
population[population=="Latrobe (C) (Vic.)"] <- "LATROBE"
population[population=="Bass Coast (S)"] <- "BASS COAST"
population[population=="Greater Geelong (C)"] <- "GEELONG"
population[population=="Stonnington (C)"] <- "STONNINGTON"
population[population=="Gannawarra (S)"] <- "GANNAWARRA"
population[population=="Cardinia (S)"] <- "CARDINIA"
population[population=="Glen Eira (C)"] <- "GLEN EIRA"
population[population=="South Gippsland (S)"] <- "SOUTH GIPPSLAND"
population[population=="Manningham (C)"] <- "MANNINGHAM"
population[population=="Moreland (C)"] <- "MORELAND"
population[population=="Wellington (S)"] <- "WELLINGTON"
population[population=="Buloke (S)"] <- "BULOKE"
population[population=="Melton (C)"] <- "MELTON"
population[population=="Hobsons Bay (C)"] <- "HOBSONS BAY"
population[population=="Melbourne (C)"] <- "MELBOURNE"
population[population=="Strathbogie (S)"] <- "STRATHBOGIE"
population[population=="Maribyrnong (C)"] <- "MARIBYRNONG"
population[population=="Macedon Ranges (S)"] <- "MACEDON RANGES"
population[population=="Brimbank (C)"] <- "BRIMBANK"
population[population=="Surf Coast (S)"] <- "SURF COAST"
population[population=="Nillumbik (S)"] <- "NILLUMBIK"
population[population=="Ballarat (C)"] <- "BALLARAT"
population[population=="Yarra (C)"] <- "YARRA"
population[population=="Port Phillip (C)"] <- "PORT PHILLIP"
population[population=="Golden Plains (S)"] <- "GOLDEN PLAINS"
population[population=="Moira (S)"] <- "MOIRA"
population[population=="Mitchell (S)"] <- "MITCHELL"
population[population=="Yarra Ranges (S)"] <- "YARRA RANGES"
population[population=="Greater Hume Shire (A)"] <- "HUME"
population[population=="Bayside (A)"] <- "BAYSIDE"
population[population=="Central Goldfields (S)"] <- "CENTRAL GOLDFIELDS"
population[population=="Greater Shepparton (C)"] <- "SHEPPARTON"
population[population=="Monash (C)"] <- "MONASH"
population[population=="Kingston (C) (Vic.)"] <- "KINGSTON"
population[population=="Moonee Valley (C)"] <- "MOONEE VALLEY"
population[population=="Greater Bendigo (C)"] <- "BENDIGO"
population[population=="Murrindindi (S)"] <- "MURRINDINDI"
population[population=="Horsham (RC)"] <- "HORSHAM"
population[population=="Southern Grampians (S)"] <- "SOUTHERN GRAMPIANS"
population[population=="Banyule (C)"] <- "BANYULE"
population[population=="Knox (C)"] <- "KNOX"
population[population=="Moorabool (S)"] <- "MOORABOOL"
population[population=="Whitehorse (C)"] <- "WHITEHORSE"
population[population=="Hindmarsh (S)"] <- "HINDMARSH"
population[population=="Boroondara (C)"] <- "BOROONDARA"
population[population=="Darebin (C)"] <- "DAREBIN"
population[population=="Moyne (S)"] <- "MOYNE"
population[population=="Ararat (RC)"] <- "ARARAT"
population[population=="Whittlesea (C)"] <- "WHITTLESEA"
population[population=="Maroondah (C)"] <- "MAROONDAH"
population[population=="Hepburn (S)"] <- "HEPBURN"
population[population=="Warrnambool (C)"] <- "WARRNAMBOOL"
population[population=="Mount Alexander (S)"] <- "MOUNT ALEXANDER"
population[population=="Swan Hill (RC)"] <- "SWAN HILL"
population[population=="Indigo (S)"] <- "INDIGO"
population[population=="Wangaratta (RC)"] <- "WANGARATTA"
population[population=="Mansfield (S)"] <- "MANSFIELD"
population[population=="Pyrenees (S)"] <- "PYRENEES"
population[population=="Benalla (RC)"] <- "BENALLA"
population[population=="Mildura (RC)"] <- "MILDURA"
population[population=="Campaspe (S)"] <- "CAMPASPE"
population[population=="Towong (S)"] <- "TOWONG"
population[population=="Loddon (S)"] <- "LODDON"
population[population=="West Wimmera (S)"] <- "WEST WIMMERA"
population[population=="Corangamite (S)"] <- "CORANGAMITE"
population[population=="Northern Grampians (S)"] <- "NORTHERN GRAMPIANS"
population[population=="Alpine (S)"] <- "ALPINE"
population[population=="Glenelg (S)"] <- "GLENELG"
population[population=="Yarriambiack (S)"] <- "YARRIAMBIACK"
population[population=="Wodonga (C)"] <- "WODONGA"
population[population=="Queenscliffe (B)"] <- "QUEENSCLIFFE"
population[population=="Colac-Otway (S)"] <- "COLAC OTWAY"

population_1 <- population %>% 
	dplyr::select(-TOTAL_POPULATION) %>% 
	filter(YEAR != "NA") %>% 
	filter(YEAR != "2011")
glimpse(population_1)


population_1$`PERSONS/KM2`<- as.numeric(population_1$`PERSONS/KM2`)

population_1<- population_1 %>% 
	spread(key = "YEAR",value = "PERSONS/KM2")


#Using GGPLOT---------
LGA_map <- readOGR(dsn = ("Datasets/Shapefiles/VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))

LGA_map_1 <- vic_lga_map
LGA_map_1@data<- left_join(LGA_map_1@data, population_1, by = c("VIC_LGA__3"="LGA"))

LGA_map_1@data[["2013"]][is.na(LGA_map_1@data[["2013"]])]<- 0
LGA_map_1@data[["2014"]][is.na(LGA_map_1@data[["2014"]])]<- 0
LGA_map_1@data[["2015"]][is.na(LGA_map_1@data[["2015"]])]<- 0
LGA_map_1@data[["2016"]][is.na(LGA_map_1@data[["2016"]])]<- 0
LGA_map_1@data[["2017"]][is.na(LGA_map_1@data[["2017"]])]<- 0
LGA_map_1@data[["2018"]][is.na(LGA_map_1@data[["2018"]])]<- 0

LGA_map_1_df <- as.data.frame(LGA_map_1)

ggplot()

glimpse(LGA_map_1_df)

glimpse()

#create quantiles for each year -----
quintiles_2018 <- function(x) {
	cut(x, breaks = c(quantile(LGA_map_1$`2018`, probs = seq (0,1, by = 0.2))),
			labels = c(1,2,3,4,5), include.lowest = TRUE)
}

quintiles_2017 <- function(x) {
	cut(x, breaks = c(quantile(LGA_map_1$`2017`, probs = seq (0,1, by = 0.2))),
			labels = c(1,2,3,4,5), include.lowest = TRUE, na.rm = TRUE)
}

quintiles_2016 <- function(x) {
	cut(x, breaks = c(quantile(LGA_map_1$`2016`, probs = seq (0,1, by = 0.2))),
			labels = c(1,2,3,4,5), include.lowest = TRUE, na.rm = TRUE)
}

#add in new row for the quantiles
LGA_map_1@data$bucket_2018<- sapply(LGA_map_1@data$`2018`, quintiles_2018)
LGA_map_1@data$bucket_2017<- sapply(LGA_map_1@data$`2017`, quintiles_2017)
LGA_map_1@data$bucket_2016<- sapply(LGA_map_1@data$`2016`, quintiles_2016)


cut_number(LGA_map_1_df$`2017`,n = 5)

