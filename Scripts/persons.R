accident_data <- read.csv("Datasets/Road Crashes/ACCIDENT.csv")
person_data <- read.csv("Datasets/Road Crashes/PERSON.csv")


#Converting fct to date
accident_data$ACCIDENTDATE <- as.Date(accident_data$ACCIDENTDATE, "%d/%m/%Y")

A4 <- sqldf("select p.SEX, strftime('%m', a.ACCIDENTDATE) AS Month, *
							from accident_data a inner join person_data p on a.accident_no = p.accident_no
						and p.\"Road.User.Type.Desc\" like '%Drivers%' ")

A4 <- sqldf("select p.SEX, strftime('%m', a.ACCIDENTDATE) AS Month, *
							from accident_data a inner join person_data p on a.accident_no = p.accident_no")

A4 <- sqldf("select \"Road.Geometry.Desc\" as Road_Geometry, case when daily_rain > 0 then 'Rain'
	else 'No Rain' end rain, strftime('%m', DATE) AS Month
							from accident_data ")

A5 <- sqldf("select ACCIDENTDATE, SEX, count(*) as No_of_accidents
						from A4 group by ACCIDENTDATE, SEX")

A5 <- sqldf("select \"Inj.Level.Desc\", AGE, count(*) as No_of_accidents
						from A4 group by \"Inj.Level.Desc\", AGE")

counts <- table( A4$SEX,  A4$Month)

barplot(counts, main="Accident Frequency according to Months",
				xlab="Months", col=c("red","darkblue", "Yellow"),
				legend = rownames(counts), beside=TRUE)

ggplot(data = A4) + 
	geom_bar(mapping = aes(x = Inj.Level.Desc, fill = Age.Group), position = "fill")

ggplot(data = A4) + 
	geom_bar(mapping = aes(x = Inj.Level.Desc, fill = Day.Week.Description), position = "fill")


# Multiple line plot
ggplot(A5, aes(x = Age.Group, y = No_of_accidents)) + 
	geom_line(aes(color = Inj.Level.Desc), size = 1) +
	theme_minimal()


# Area plot
ggplot(A5, aes(x = ACCIDENTDATE, y = No_of_accidents)) + 
	geom_area(aes(color = SEX, fill = SEX), 
						alpha = 0.5, position = position_dodge(0.8)) +
	scale_color_manual(values = c("#00AFBB", "#E7B800", "Red")) +
	scale_fill_manual(values = c("#00AFBB", "#E7B800", "Red"))



# Area plot
ggplot(A5, aes(x = AGE, y = No_of_accidents)) + 
	geom_area(aes(color = Inj.Level.Desc, fill = Inj.Level.Desc), 
						alpha = 0.3, position = position_dodge(0.8)) +
	scale_color_manual(values = c("Red", "Green", "Yellow", "Orange")) +
	scale_fill_manual(values = c("Red", "Green", "Yellow", "Orange"))


ggplot(A5, aes(AGE, No_of_accidents)) +
	geom_area(aes(fill = Inj.Level.Desc))


# Multiple line plot
ggplot(A5, aes(x = AGE, y = No_of_accidents)) + 
	theme_minimal()