library(ggplot2)

d <-  c(30, 40, 50,  60, 70, 75, 80, 90, 100, 110)
e <- c(1,2,3,5)
d<- as.numeric(d)
e<- as.factor(e)

a <- 1
b <- 1
my_matrix <- matrix(nrow=10, ncol=4)

#Varied Speed Zone and Light Condition
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



for(row in 1:nrow(my_matrix)) {
	for(col in 1:ncol(my_matrix)) {
		tester$SPEED_ZONE <- d[row]
		tester$LIGHT_CONDITION <- e[col]
		my_matrix[row, col]  <- predict(mdlWet2018, tester, type = "response")
	}
}

colnames(my_matrix) <- c("Day", "Dusk/Dawn", "Lights ON", "No Lights")
rownames(my_matrix) <- t(d)

my_matrix <- my_matrix *100

ggplot(my_matrix, aes(X, Y, fill= Z)) + 
	geom_tile()


aa <- data.frame(rows=rownames(m1)[row(m1)], vars=colnames(m1)[col(m1)],
								 values=c(m1))


aa <- data.frame(Speed_Zone = i,
								 Light_Condition = j,
								 PredictedValue=value)
aa <- aa[0,]

for(i in d){
	for(j in e){
		paste(i,j)
		tester$SPEED_ZONE <- i
		tester$LIGHT_CONDITION <- as.factor(j)
		value<- predict(mdlWet2018, tester, type = "response") *100
		aa  <- rbind(aa, c(i, j, value))
	}
}


colnames(aa) <- c("Speed_Limit", "Light_Conditions", "Probability")

aa$Speed_Limit <- as.factor(aa$Speed_Limit)
aa$Light_Conditions <- as.factor(aa$Light_Conditions)

ggplot(data = aa, aes(x = Light_Conditions, y = Speed_Limit)) +
	geom_tile(aes(fill = Probability))+
	theme_classic()+
	labs(x= "Light  Conditions", y="Speed Limit",
			 title = "Probability- Speed Limit VS Light Conditions")+
			theme(plot.title = element_text(hjust = 0.5))+
	scale_fill_gradient(low = "#56B1F7", high = "#132B43")


