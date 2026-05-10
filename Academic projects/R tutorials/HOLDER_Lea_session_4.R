#TD R 
#Léa Holder
#Seance 4

rm(list= ls())
library(readxl)
mydatas3 <- read_excel("US_counties_COVID19_health_weather_data.xlsx")

data_unique <- mydatas3[!duplicated(mydatas3), ]
sum(duplicated(data_unique))

#Exercise 11 -----
sum(is.na(data_unique$segregation_index & data_unique$income_ratio & data_unique$max_wind_speed))

#2100 missing

#Create a new database data_dropped in which you drop the missing values for these three variables

data_dropped <- subset(data_unique, !is.na(segregation_index) & !is.na(income_ratio) & !is.na(max_wind_speed))

nrow(data_dropped)

#Create a database data_inputted in which you replace the missing values of these three variables by their mean

#max speed wind 
data_inputted <- data_unique

miss_wind <- is.na(data_inputted$max_wind_speed)
data_inputted$max_wind_speed_filled <- data_inputted$max_wind_speed
data_inputted[miss_wind, "max_wind_speed_filled"] <- mean(data_inputted$max_wind_speed, na.rm = TRUE)

#segregation
miss_segregation <- is.na(data_inputted$segregation_index)
data_inputted$segregation_index_filled <- data_inputted$segregation_index
data_inputted[miss_segregation, "segregation_index_filled"] <- mean(data_inputted$segregation_index, na.rm = TRUE)

#income_ratio
miss_income <- is.na(data_inputted$income_ratio)
data_inputted$income_ratio_filled <- data_inputted$income_ratio
data_inputted[miss_income, "income_ratio_filled"] <- mean(data_inputted$income_ratio, na.rm = TRUE)

sum(is.na(data_inputted$segregation_index_filled))
sum(is.na(data_inputted$income_ratio_filled))
sum(is.na(data_inputted$max_wind_speed_filled))

#plus de missing

#Exercise 12

library(haven)
mydata <- read_dta("student_test_data.dta")
mydata <- as.data.frame(mydata)

summary(mydata$agetest)
summary(mydata$girl)
summary(mydata$std_mark)
summary(mydata$etpteacher)

