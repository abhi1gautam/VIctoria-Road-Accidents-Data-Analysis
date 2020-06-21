# Analysis of Victorian Road Accident data

## Introduction
As at 30 September 2019, the number of lives lost on Victorian roads in 2019 reached 205 (TAC 2019). This figure is above the targeted 200 and working its way towards the 2018 fatality level of 213. From an investigation of historical data since 2006, road accidents in Victoria are most common amongst young adults.

The causes of the increasing number of accidents may be multifactorial, it is however important to treat and eliminate some of these factors so the accident wouldn’t occur to begin with. As the number of vehicles increases, the number of accidents go up as well. This calls for effective measures to be taken to reduce the mortality rate.

## Models Used
### Logistic Regression
```
mdlMakerRain <- function(dataset){
	result <- glm(TOTAL_CASUALTIES/TOTAL_NO_OCCUPANTS ~ ACCIDENT.TYPE.DESC +
									LIGHT_CONDITION + 
									SPEED_ZONE + 
									RAINFALL + 
									NO_OF_VEHICLES + 
									SECURITY_EQUIPS_NOT_WORN + 
									ROAD_TYPE_GROUP + 
									DRIVER_AGE + 
									DRIVER_SEX + 
									CAR_AGE + 
									Vehicle.Type.Desc +
									ROAD_SURFACE_TYPE,
								family = binomial, data = dataset, weights = TOTAL_NO_OCCUPANTS)
	return(result)
}

mdlMakerWet <- function(dataset){
	result <- glm(TOTAL_CASUALTIES/TOTAL_NO_OCCUPANTS ~ ACCIDENT.TYPE.DESC +
									LIGHT_CONDITION + 
									SPEED_ZONE + 
									WET + 
									NO_OF_VEHICLES + 
									SECURITY_EQUIPS_NOT_WORN + 
									ROAD_TYPE_GROUP + 
									CAR_AGE + 
									Vehicle.Type.Desc + 
									OLD_COUNT + 
									ROAD_SURFACE_TYPE,
								family = binomial, data = dataset, weights = TOTAL_NO_OCCUPANTS)
	return(result)
}
```

### Model Output
Please view the report, "AT2_Crash_Test_Dummies.pdf".


## Conclusion
Given the nature of the datasets employed in this investigation and the question sought to be answered, a binomial logistic regression model was employed to ascertain the probability of death of severe injury, given the host of variables modelled.
While the exploratory data analysis looks at the total population of crashes, the regression model itself focuses on 2018, uncovering highly significant results in the majority of variables included.

Most notably it finds wet weather has a weak and negative relationship with the injury outcome of individuals in road crashes.

Moreover, it reaffirms the significance and weight variables associated with current Towards Zero initiatives have, with the speed zone, seat belt usage and road quality aspects all strong indicators.
