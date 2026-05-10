#Session 1
#Léa Holder M2APP

#Exercice 0 -----
getwd()

#Exercice 1 -----
days_year = c(1:365)
m = matrix(1:10, nrow = 2, byrow = TRUE)
urbanpop <- read.csv("urbanpop_1960_1966.csv", sep = ";")

#Exercice 2 -----
#install.packages("readxl")
library(readxl)
help(read_excel)
emigration_climate = read_excel("merrieks.xls")
