#EXAMEN R LEA HOLDER 

#Set up ----

install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("gtsummary")
install.packages("ggplot2")
install.packages("modelsummary")

setwd("C:/Users/leaaa/OneDrive/Documents/MAGISTERE/MAGEVAL 3/TD R/EXAM")

#Preparing the data ----

#1
library(readxl)
mydata <- read_excel("data_Rtest.xlsx")

#2
head(mydata)

#3
nrow(mydata)
#6991 observations. 

#4
ncol(mydata)
#8 colonnes

#5
sum(duplicated(mydata))
#21 duplicates 

#6
mydata_uniq <- unique(mydata)

sum(duplicated(mydata_uniq))

#No more duplicates 

#7
mydata_noomit <- na.omit(mydata_uniq)

#8a

mydata_noomit$turnout_rate_total <- mydata_noomit$turnout_total/mydata_noomit$registered_total
mydata_noomit$turnout_rate_male <- mydata_noomit$turnout_male/mydata_noomit$registered_male
mydata_noomit$turnout_rate_female <- mydata_noomit$turnout_female/mydata_noomit$registered_female

#8b
mydata_noomit$share_registered_female <- mydata_noomit$registered_female/mydata_noomit$registered_total

#8c
mydata_noomit$share_turnout_female <- mydata_noomit$turnout_female/mydata_noomit$turnout_total

#8d
median_reg <- median(mydata_noomit$registered_total)
mydata_noomit$large_pooling_booth <- as.numeric(mydata_noomit$registered_total>median_reg)

#8e
mydata_noomit$treated <- factor(mydata_noomit$treatment, levels = c(0,1), labels = c("Control","Treated"))

#8f

library(dplyr)

mydata_noomit <- mydata_noomit %>% group_by(town_id) %>% mutate(large_town = ifelse(sum(registered_total) > 250000, 1, 0)) %>% ungroup() 

#Descriptive statistics ----

library(gtsummary)

#1
table_balance <- mydata_noomit |>
  select(treated, registered_total, turnout_total, share_registered_female, share_turnout_female, turnout_rate_total, turnout_rate_male, turnout_rate_female) |>
  tbl_summary(by = treated, missing = "no", statistic = all_continuous() ~ "{mean} ({sd})", digits = all_continuous() ~ 2)

table_balance

#2
table_balance <- mydata_noomit |>
  select(treated, registered_total, turnout_total, share_registered_female, share_turnout_female, turnout_rate_total, turnout_rate_male, turnout_rate_female) |>
  tbl_summary(by = treated, missing = "no", statistic = all_continuous() ~ "{mean} ({sd})", digits = all_continuous() ~ 2) |>
  add_overall() |> add_difference() |> add_significance_stars() |> modify_caption("**Balance table**")

table_balance

#3
library(gt)

table_balance <- mydata_noomit |>
  select(treated, registered_total, turnout_total, share_registered_female, share_turnout_female, turnout_rate_total, turnout_rate_male, turnout_rate_female) |>
  tbl_summary(by = treated, missing = "no", statistic = all_continuous() ~ "{mean} ({sd})", digits = all_continuous() ~ 2, #then we change the variable names
      label = list(registered_total ~ "Total registered voters",
      turnout_total ~ "Total turnout",
      share_registered_female ~ "Share of female registered voters",
      share_turnout_female ~ "Share of female turnout",
      turnout_rate_total ~ "Total turnout rate",
      turnout_rate_male ~ "Turnout rate (male)",
      turnout_rate_female ~ "Turnout rate (female)")) |> #then we remove the p value column to add the stars in the difference column, and we remove the automatic footnotes, to add a note: 
      add_overall() |> add_difference() |> add_significance_stars(hide_p = TRUE, pattern = "{estimate}{stars}") |> modify_caption("**Balance table**") |> modify_footnote(everything() ~NA) |> as_gt() |> gt::tab_source_note(source_note= "Note: This table reports the variables' mean and standard deviation in parentheses for different samples.
                                                                                                                                                                                                                                                                                 Column (1) corresponds to the overall population. Column (2) and (3) correspond respectively
                                                                                                                                                                                                                                                                                 to the control and treated groups. Column (4) presents the difference in means between the two groups.
                                                                                                                                                                                                                                                                                 Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05.")

table_balance

#Visualize the main results ----

#1 bar graph 
library(ggplot2)

#i. mean total turnout rate
ggplot(mydata_noomit, aes(x = treated, y = turnout_rate_total)) + stat_summary(fun = "mean", geom = "col") + scale_x_discrete(labels = c("Control group", "Treatment group")) + labs(x = NULL, y = "Average total turnout rate")

#ii. mean female turnout rate 
ggplot(mydata_noomit, aes(x = treated, y = turnout_rate_female)) + stat_summary(fun = "mean", geom = "col", fill="pink") + scale_x_discrete(labels = c("Control group", "Treatment group")) + labs(x = NULL, y = "Average turnout rate (female)")

#iii. mean male turnout rate 
ggplot(mydata_noomit, aes(x = treated, y = turnout_rate_male)) + stat_summary(fun = "mean", geom = "col", fill="darkgreen") + scale_x_discrete(labels = c("Control group", "Treatment group")) + labs(x = NULL, y = "Average turnout rate (male)")

#2 scatter plot

#a
ggplot(mydata_noomit, aes(x = registered_total, y = turnout_rate_total)) + geom_point(color= "pink") 

#b
ggplot(mydata_noomit, aes(x = registered_total, y = turnout_rate_total)) + geom_point(color= "pink")+ geom_smooth(aes(color = treated))

#Regression analysis and presenting results ----

#1
base_lm <- lm(turnout_rate_total ~ treated, data = mydata_noomit)
summary(base_lm)

#2
#female turnout rate: 
female_lm <- lm(turnout_rate_female ~ treated, data = mydata_noomit)

#male turnout rate: 
male_lm <- lm(turnout_rate_male ~ treated, data= mydata_noomit)

summary(female_lm)
summary(male_lm)

#3
size_lm <- lm(turnout_rate_total ~ treated*large_town, data = mydata_noomit)
summary(size_lm)

#4
library(modelsummary)

#a. table with baseline, female, and male 
modelsummary(list(base_lm, female_lm, male_lm), stars = TRUE, title = "Effect of the treatment on the turnout rate by gender")

#b. with interaction term by town size
modelsummary(list(base_lm, size_lm), starts = TRUE, title = "Effect of the treatment on the turnout rate by town size")

#5. improving the table
coefmap <- c("(Intercept)" = "Intercept", "treatedTreated" = "Treated", "large_town" = "Large Town", "treatedTreated:large_town" = "Treated * Large Town")
modelsummary(list(base_lm, size_lm), stars = TRUE, gof_map = c("nobs", "r.squared"), coef_map = coefmap, title = "Effect of the treatment on the turnout rate", notes = list("Note:This table reports the estimated effect of the treatment on the poll booth turnout rate. The observations are at
                                                                                                                                                                                                           the poll booth level. Column (1) corresponds to the baseline ATE. Column (2) corresponds to the
                                                                                                                                                                                                           heterogeneous effect by town size, where a large town is a town with more that 250,000 registered voters."))

#6. Interpretations ----

#***Regression with interaction term:
#column (1):
#intercept = 0.569 --> Ceteris paribus, in the non-treated pooling booths, the average turnout rate among registered voters is 56.9%. This coefficient is statistically significant (p<0.01). 
#treated = 0.01 --> ceteris paribus, the treatment is associated with a 1 percentage point increase in total turnout compared to the control group. However, this effect is not statistically significant.

#column (2)
#the intercept didn't change (0.569)
#treated coefficient: 0.034 --> Ceteris paribus, treatment in small towns is associated with a 3.4 percentage point increase of the total turnout rate. This effect is statistically significant (p<0.01).
#the coefficient associated to large town is not statistically significant, meaning that in the absence of treatment, there is no evidence of difference in baseline turnout.
#interaction term: ceteris paribus, the treatment effect on the average total turnout is smaller in large towns than in smaller towns, by about 0.44 percentage points. This effect is statistically significant (p<0.05)

#***Regression without interaction term (by gender):
#Column (1)
#intercept: in untreated pooling booths, the average turnout rate is 56.9%. This result is statistically significant (p<0.001). 
#treated coefficient: ceteris paribus, the treatment increases the total turnout rate by 1 percentage point, but this effect is not statistically significant. 

#Column (2): female turnout
#Intercept is the same
#treatment: ceteris paribus, the treatment led to a 1.4 percentage point increase in the female turnout rate. This effect is statistically significant (p<0.05). 

#Column (3): male turnout rate 
#intercept is the same
#Ceteris paribus, the treatment increased the male turnout rate by about 0.6 percentage point. However, this effect is not statistically significant.

