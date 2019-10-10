# This script is to draw a logistic regression model of two variables
# Here, two assumptions are taken. Injuiry and Non Injuiry are calculated
# based on the accidents

person_data <- read.csv("Datasets/Road Crashes/PERSON.csv")

names(person_data)


head(person_data)


summary(person_data)


par(mfrow=c(1,8))
for(i in 1:8) {
	hist(person_data[,i], main=names(person_data)[i])
}


library(Amelia)
library(mlbench)
library(ggplot2)
missmap(person_data, col=c("blue", "red"), legend=FALSE)



p2 <- df[person_data$SEX, person_data$AGE, person_data$Age.Group,
						person_data$INJ_LEVEL, person_data$HELMET_HELT_WORN]




colnames(p2) <- c("Sex", "Age", "Age_Group", "Inj_Lvl", "Helmet")

p2 <- sqldf("select Sex, AGE, \"Age.Group\" as Age_Group, INJ_LEVEL as Inj_Level, HELMET_BELT_WORN as Helmet
						from person_data")

p3 <- sqldf("select AGE, 2 as INJ_LEVEL from p2 where inj_level = 1")

p3 <- rbind(p3, sqldf("select AGE, 2 as INJ_LEVEL from p2 where inj_level = 2"))

p3 <- rbind(p3, sqldf("select AGE, 1 as INJ_LEVEL from p2 where inj_level = 3"))

p3 <- rbind(p3, sqldf("select AGE, 1 as INJ_LEVEL from p2 where inj_level = 4"))








library(corrplot)

correlations <- cor(p2[3 : 4])
corrplot(correlations, method="circle")

plot(p3$AGE, p3$INJ_LEVEL)


library(Amelia)
missmap(dat2018, main = "Missing values vs observed")


person_data <- read.csv("Datasets/Road Crashes/PERSON.csv")

person_data %>% select(ACCIDENT_NO, SEX, AGE, INJ_LEVEL, SEATING_POSITION, HELMET_BELT_WORN, ROAD_USER_TYPE)


person_data1 <- person_data %>% filter(SEATING_POSITION == 'D ') %>% 
		select(SEX, AGE, INJ_LEVEL, HELMET_BELT_WORN, ROAD_USER_TYPE)

person_data1 <- person_data %>% filter(SEATING_POSITION == 'D ') %>% 
	select(SEX, AGE, INJ_LEVEL, HELMET_BELT_WORN, ROAD_USER_TYPE)

missmap(person_data1, main = "Missing values vs observed")

#setting age as average age for NA values
person_data1$AGE[is.na(person_data1$AGE)] <- mean(person_data1$AGE,na.rm=T)

#setting seating position as U for NA values
#person_data1$SEATING_POSITION <- factor(ifelse( is.na(person_data1$SEATING_POSITION), "U",
#																								person_data1$SEATING_POSITION))


#a training data set
train <- person_data1[1:100000,]

#a dataset to test our model
test <- person_data1[100001:100200,]

train$INJ_LEVEL[train$INJ_LEVEL == "2"] <- "1"
train$INJ_LEVEL[train$INJ_LEVEL == "3"] <- "2"
train$INJ_LEVEL[train$INJ_LEVEL == "4"] <- "2"


train$INJ_LEVEL[train$INJ_LEVEL == "1"] <- "0"
train$INJ_LEVEL[train$INJ_LEVEL == "2"] <- "1"

train$INJ_LEVEL[train$INJ_LEVEL == "0"] <- 0
train$INJ_LEVEL[train$INJ_LEVEL == "1"] <- 1

train$INJ_LEVEL <- as.numeric(as.character(train$INJ_LEVEL))

model <- glm(INJ_LEVEL ~.,family=binomial(link='logit'),data=train)

data <- person_data1



fitted.results <- predict(model,newdata=subset(test,select=c(1,2,3,4,5)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$INJ_LEVEL)
print(paste('Accuracy',1-misClasificError))


