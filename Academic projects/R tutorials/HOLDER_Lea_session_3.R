#TD session 3
#Léa Holder

#Exercise 9 -----
rm(list= ls())
US_county = read.csv("US_counties_COVID19_health_weather_data.csv", sep = ";")

typeof(US_county$date)
#character

class(US_county$date)
#character 

typeof(US_county$percent_physically_inactive)
#character

class(US_county$percent_physically_inactive)
#character 

US_county$date <- as.Date(US_county$date, format = "%Y-%m-%d")

typeof(US_county$date)
#double 
class(US_county$date)
#date 

#Exercise 10 -----

sum(is.na(US_county$date))
#0 missing values 

sum(is.na(US_county$percent_physically_inactive))
#pas de missing values 

