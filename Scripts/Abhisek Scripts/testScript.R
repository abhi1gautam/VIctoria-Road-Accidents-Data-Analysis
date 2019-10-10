
accident_data <- read.csv("Datasets/Road Crashes/ACCIDENTS WITH RAINFALL.csv")

person_data %>% select(ACCIDENT_NO, SEX, AGE, INJ_LEVEL, SEATING_POSITION, HELMET_BELT_WORN, ROAD_USER_TYPE)


accident_data1 <- accident_data %>%
	select(Rainfall, ROAD_GEOMETRY, LIGHT_CONDITION, DAY_OF_WEEK, NO_PERSONS_INJ_2, NO_PERSONS_KILLED)

accident_data1$SEV <- ifelse(accident_data1$NO_PERSONS_INJ_2 + accident_data1$NO_PERSONS_KILLED != 0, 1, 0)
#accident_data1$Rainfall <- ifelse(accident_data1$Rainfall >10, 1, 0)


accident_data1$SEV <- ifelse(accident_data1$NO_PERSONS_INJ_2 + accident_data1$NO_PERSONS_KILLED != 0, 1, 0)


accident_data1 <- accident_data1 %>% 	select(Rainfall, ROAD_GEOMETRY, LIGHT_CONDITION, DAY_OF_WEEK, SEV)


missmap(accident_data1, main = "Missing values vs observed")

accident_data1$ROAD_GEOMETRY <- as.factor(accident_data1$ROAD_GEOMETRY)
accident_data1$LIGHT_CONDITION <- as.factor(accident_data1$LIGHT_CONDITION)
accident_data1$DAY_OF_WEEK <- as.factor(accident_data1$DAY_OF_WEEK)
accident_data1$SEV <- as.factor(accident_data1$SEV)



#a training data set
train <- accident_data1[1:100000,]

#a dataset to test our model
test <- accident_data1[100001:120000,]

train$SEV <- as.numeric(as.character(train$SEV))

model <- glm(SEV ~.,family=binomial(link='logit'),data=train)

anova(model, test="Chisq")

fitted.results <- predict(model,newdata=subset(test,select=c(1,2,3,4,5)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$SEV, na.rm = TRUE)

print(paste('Accuracy',1-misClasificError))



