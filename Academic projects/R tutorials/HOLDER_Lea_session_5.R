#SESSION 4 & 5 TD R 
#Léa Holder

#SESSION 4 
rm(list= ls())
library(haven)
mydata = read_dta("C:/Users/leaaa/OneDrive/Documents/MAGISTERE/MAGEVAL 3/TD R/student_test_data.dta")

#Exerise 13 ------

lm(agetest ~ tracking, data = mydata)
#difference: 0.1815

lm(girl ~ tracking, data = mydata)
#difference: 0.0067

lm(etpteacher ~ tracking, data = mydata)
#difference: -0.013

lm(attrition ~ tracking, data = mydata)
#difference: 0.0004

#Exercise 14 ----

#1
X_mean <- mean(mydata$mathscoreraw, na.rm = TRUE)
X_sd <- sd(mydata$mathscoreraw, na.rm = TRUE)
mydata$mathscoreraw_standardized <- (mydata$attrition- X_mean) / X_sd

#2
X_mean <- mean(mydata$litscore, na.rm = TRUE)
X_sd <- sd(mydata$litscore, na.rm = TRUE)
mydata$litscore_standardized <- (mydata$attrition- X_mean) / X_sd

lm(mathscoreraw_standardized ~ tracking, data = mydata)
lm(litscore_standardized ~ tracking, data = mydata)

lm(mathscoreraw_standardized ~ tracking + agetest + girl + etpteacher + tophalf + percentile, data = mydata)
lm(litscore_standardized ~ tracking + agetest + girl + etpteacher + tophalf + percentile, data = mydata)

#Exercise 15 -----

#1
X_mean <- mean(mydata$totalscore, na.rm = TRUE)
X_sd <- sd(mydata$totalscore, na.rm = TRUE)
mydata$totalscore_standardized <- (mydata$totalscore- X_mean) / X_sd

#2
lm(totalscore_standardized ~ tracking * (bottomquarter + secondquarter + topquarter),data = mydata)

#Exercice 16 -----
#install.packages("gtsummary")

library(gtsummary)

tbl <- tbl_summary(data = mydata, by = "tracking", include = c("std_mark", "litscore", "mathscoreraw"), statistic = everything() ~ "{mean} ({sd}) {min} {max}", missing_stat = "{N_miss} ({p_miss})")

#Exercise 17 -----

#install.packages("modelsummary")
library(modelsummary)

models <- list()
models[[1]] <- lm(litscore ~ tracking, data = mydata)
models[[2]] <- lm(litscore ~ tracking + girl + agetest, data = mydata)
models[[3]] <- lm(litscore ~ tracking + girl + agetest + factor(division), data = mydata)

modelsummary(models, estimate = "{estimate}{stars}", gof_map = c("nobs", "r.squared"), output = "models_17.tex")

#the issue with the table is that there is written "factor(divison)" in front of the corresponding variables. we can rename the levels of the factor so the table is more interpretable for the division fixed effects. we can also add a title and notes, data source...


#SESSION 5

#Exercise 12 -----

rm(list= ls())
library(haven)
mydata = read_dta("C:/Users/leaaa/OneDrive/Documents/MAGISTERE/MAGEVAL 3/TD R/TAJ_2007_LSMS.dta")

#install.packages("ggplot2")
library(ggplot2)

mydata$bsfood <- mydata$pcfood / mydata$pcc
mydata$bseduc <- mydata$pceduc / mydata$pcc

ggplot(mydata, aes(x = pcc, y = bsfood)) + geom_point()

basescatter <- ggplot(mydata, aes(x = log(pcc), y = bsfood, size = HHsize)) + geom_point(color = "red", alpha = 0.1) + labs(x = "Log of per capita consumption", y = "Food budget share")

basescatter

#Exercise 13 -----
ggplot(mydata, aes(x = pcc)) + geom_density() + xlim(0, 800)

ggplot(mydata, aes(x = pcc)) + geom_histogram(bins = 50)

ggplot(mydata, aes(x = factor(Decilec))) + geom_bar() + labs(x = "Consumption deciles")

ggplot(mydata, aes(x = log(pcc), y = bsfood)) + geom_smooth(aes(y = bsfood), color = "pink") + geom_smooth(aes(y = bseduc), color = "purple") + labs(x = "Log of per capita consumption", y = "Budget share")

#Exercise 14 -----

scatplot <- ggplot(mydata, aes(x = pcc, y = bsfood, color = factor(location))) + geom_point() + scale_x_continuous(name = "Per capita consumption") + scale_y_continuous(name = "Food budget share") + scale_color_discrete(name = "Location", labels = c("Rural", "Urban"))

scatplot

#with labs:
scatplot <- ggplot(mydata, aes(x = pcc, y = bsfood, color = factor(location))) + geom_point() + labs(x = "Per capita consumption", y = "Food budget share", color = "Location") + scale_color_discrete(labels = c("Rural", "Urban"))

scatplot

scatplot + theme_minimal()
scatplot + theme_classic()
scatplot + theme_light()
scatplot + theme_void()
scatplot + theme_dark()
scatplot + theme_linedraw()
scatplot + theme_bw() 
scatplot + theme_gray()

