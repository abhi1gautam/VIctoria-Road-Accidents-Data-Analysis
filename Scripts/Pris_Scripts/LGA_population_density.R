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

colnames(population)[colnames(population)=="LGA"]<- "VIC_LGA__3"

#On Population Density --------
population_1 <- population %>% 
	dplyr::select(-TOTAL_POPULATION) %>% 
	filter(YEAR != "NA") %>% 
	filter(YEAR == "2018")
glimpse(population_1)


population_1$`PERSONS/KM2`<- as.numeric(population_1$`PERSONS/KM2`)

population_1<- population_1 %>% 
	spread(key = "YEAR",value = "PERSONS/KM2")

LGA_map <- readOGR(dsn = ("Datasets/Shapefiles/VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))
LGA_df <- as.data.frame(LGA_map)

LGA_map_1 <- LGA_map
LGA_map_1@data <- left_join(LGA_map_1@data,population_1, by = "VIC_LGA__3")
LGA_map_1@data[["2013"]][is.na(LGA_map_1@data[["2013"]])]<- 0
LGA_map_1@data[["2014"]][is.na(LGA_map_1@data[["2014"]])]<- 0
LGA_map_1@data[["2015"]][is.na(LGA_map_1@data[["2015"]])]<- 0
LGA_map_1@data[["2016"]][is.na(LGA_map_1@data[["2016"]])]<- 0
LGA_map_1@data[["2017"]][is.na(LGA_map_1@data[["2017"]])]<- 0
LGA_map_1@data[["2018"]][is.na(LGA_map_1@data[["2018"]])]<- 0
glimpse(LGA_map_1)

LGA_map_1_df<- as.data.frame(LGA_map_1)

tmap_mode("plot")
tm_shape(LGA_map_1)+
	tm_polygons(palette = viridisLite::viridis(10),
							title = "Population Density")+
	tm_layout(main.title = "Victoria LGA Population Density")

tm_shape(LGA_map_1)+
	tm_polygons("2018",
							title = "Population Density")+
	tm_layout(main.title = "LGA Population Density 2018")

#On Total Population --------
population_2 <- population %>% 
	dplyr::select(-`PERSONS/KM2`) %>% 
	filter(YEAR != "NA") %>% 
	filter(YEAR != "2011")
glimpse(population_2)
population_2$TOTAL_POPULATION<- as.numeric(population_2$TOTAL_POPULATION)

population_2<- population_2 %>% 
	spread(key = "YEAR",value = "TOTAL_POPULATION")

LGA_map_2 <- LGA_2013
LGA_map_2@data <- left_join(LGA_map_2@data, population_2, by = "VIC_LGA__3")
LGA_map_2@data[["2013"]][is.na(LGA_map_2@data[["2013"]])]<- 0
LGA_map_2@data[["2014"]][is.na(LGA_map_2@data[["2014"]])]<- 0
LGA_map_2@data[["2015"]][is.na(LGA_map_2@data[["2015"]])]<- 0
LGA_map_2@data[["2016"]][is.na(LGA_map_2@data[["2016"]])]<- 0
LGA_map_2@data[["2017"]][is.na(LGA_map_2@data[["2017"]])]<- 0
LGA_map_2@data[["2018"]][is.na(LGA_map_2@data[["2018"]])]<- 0

tm_shape(LGA_map_2)+
	tm_polygons(c("2013","2014","2015","2016","2017","2018"),
							title = "Population")+
	tm_layout(main.title = "LGA Population 2013-18",
						panel.show = TRUE, 
						panel.labels = c("2013","2014","2015","2016","2017","2018"),
						panel.label.color = "white",             
						panel.label.bg.color = "midnightblue")+
	tm_layout (legend.outside = TRUE)

tm_shape(LGA_map_2)+
	tm_polygons("2018",
							palette = viridisLite::viridis(5),
							title = "Population")+
	tm_layout(main.title = "LGA Population 2018")

#Using GGPLOT---------
LGA_map_1 <- readOGR(dsn = ("Datasets/Shapefiles/VIC_LGA_POLYGON_shp/VIC_LOCALITY_POLYGON_shp.shp"))
summary(LGA_map_1)
LGA_map_2 <- fortify(LGA_map_1, region = "VIC_LGA__3")

LGA_map_3 <- left_join(LGA_map_2, population, by = c("id"="VIC_LGA__3"))
LGA_map_3 <- clean_names(LGA_map_3)
LGA_map_3$persons_km2 <- as.numeric(LGA_map_3$persons_km2)
LGA_map_3 %>% 
	mutate(log_density = log(persons_km2))

LGA_map_2018 <- LGA_map_3 %>% 
	filter(year == "2018")

summary(LGA_map_2018)

ggplot()+
	geom_polygon(data = LGA_map_3, aes(fill = persons_km2, x = long, y = lat, group = group))+
	geom_path(data = LGA_map_3, aes (x = long, y = lat, group = group))+
	coord_equal()+
	theme_classic()+
	theme(legend.position = "right")+
	ggtitle("Fatalities Across Victoria - 2018")

ggplot()+
	geom_polygon(data = LGA_map_2018, aes(x = long, y = lat, group = group, fill = persons_km2))+
	geom_path(data = LGA_map_2018, aes(x = long, y = lat, group = group), colour = "black")+
	coord_equal()+
	theme_classic()

bins.quantiles()

	scale_fill_gradient(colours = rainbow(n_breaks),
											breaks = split_interval(br,n_breaks))

	scale_fill_manual(values = viridisLite::viridis(5),
										breaks = brks_scale_2018,
										name = "Population Density",
										labels = labels_scale_2018)
	
	#######
	
breaks_2018 <- c(3.1,8.1,83.5) 
min_2018 <- min (LGA_map_2018$persons_km2, na.rm = T)
max_2018 <- max (LGA_map_2018$persons_km2, na.rm = T)	
	
labels_2018 <- c()
brks_2018 <- c(min_2018,breaks_2018,max_2018)
	
for(idx in 1:length(breaks_2018)){
		labels_2018 <- c(labels_2018,round(brks_2018[idx +1], 2))
	}
	
labels_2018 <- brks_2018
	
LGA_map_2018$brks <- cut(LGA_map_2018$persons_km2, 
													 breaks = brks_2018, 
													 include.lowest = TRUE,
													 labels = labels_2018)

brks_scale_2018 <- levels(LGA_map_2018$brks)
labels_scale_2018 <- rev(brks_scale_2018)

### Quantiles in Dataset-------
quantile (LGA_map_1_df$`2018`, na.rm = TRUE)

quantiles_2018 <- cut_number(LGA_map_1_df$`2018`, n = 5)
table(quantiles_2018)

LGA_2018<- LGA_map_1_df
quintiles <- function(x) {
	cut(x, breaks = c(quantile(LGA_2018$`2018`, probs = seq (0,1, by = 0.2))),
			labels = c(1,2,3,4,5), include.lowest = TRUE)
}
LGA_2018$bucket_2018<- sapply(LGA_2018$`2018`, quintiles)
glimpse(LGA_2018)

LGA_2018$bucket_2018 <-	cut_number(quant_2018$`2018`,n = 5)

LGA_2018_quant <- LGA_map_1
quantiles_2018 <- function(x) {
	cut(x, breaks = c(quantile(LGA_2018_quant$`2018`, probs = seq (0,1, by = 0.2))),
			labels = c(1,2,3,4,5), include.lowest = TRUE)
}
LGA_2018_quant@data$quant_2018 <- sapply(LGA_2018_quant@data$`2018`, quantiles_2018)
LGA_2018_quant@data$quant_2018[LGA_2018_quant@data$quant_2018== 1] <- 1
LGA_2018_quant@data$quant_2018[LGA_2018_quant@data$quant_2018== 2] <- 4
LGA_2018_quant@data$quant_2018[LGA_2018_quant@data$quant_2018== 3] <- 30
LGA_2018_quant@data$quant_2018[LGA_2018_quant@data$quant_2018== 4] <- 1200
LGA_2018_quant@data$quant_2018[LGA_2018_quant@data$quant_2018== 5] <- 5500


LGA_2018_quant@data$quant_2018<- cut_number(LGA_2018_quant@data$`2018`, n = 5)
LGA_2018_quant@data$quant_2018<- as.numeric(LGA_2018_quant@data$quant_2018)

LGA_2018_quant_df <- as.data.frame(LGA_2018_quant)

tm_shape(LGA_2018_quant)+
	tm_polygons(col = "quant_2018",
							title = "Population Density (persons/km2)")+
	tm_layout(main.title = "LGA Population Density")
	

?tm_polygons

?cut_number


#accident count by LGA

LGA_count <- left_join(accident, NODE, by = "ACCIDENT_NO")
LGA_count<- clean_names(LGA_count)
LGA_summary <- ddply(LGA_count, .(lga_name_all), summarise, count = length(accident_no))

LGA_summary[1:49,"VIC_LGA__3"]<- "FALLS CREEK ALPINE RESORT (UNINC)"
summary_by_LGA2[50:55,"VIC_LGA__3"]<- "FRENCH-ELIZABETH-SANDSTONE ISLANDS (UNINC)"
summary_by_LGA2[56:79,"VIC_LGA__3"]<- "LAKE MOUNTAIN ALPINE RESORT (UNINC)"
summary_by_LGA2[80:100,"VIC_LGA__3"]<- "MOUNT BAW BAW ALPINE RESORT (UNINC)"
summary_by_LGA2[101:141,"VIC_LGA__3"]<- "MOUNT BULLER ALPINE RESORT (UNINC)"
summary_by_LGA2[142:213,"VIC_LGA__3"]<- "MOUNT HOTHAM ALPINE RESORT (UNINC)"
summary_by_LGA2[214:217,"VIC_LGA__3"]<- "MOUNT STIRLING ALPINE RESORT (UNINC)"
