#####################
# Ariana Antunes - PS04 
#####################


#packages
install.packages("eha")
install.packages("survival")
install.packages("ggfortify")
library(survival)
library(eha)
library(stargazer)
library(ggfortify)


# set wd for current folder
setwd("/Users/arianaantunes/Documents/GitHub/StatsII_Spring2023/problemSets/PS04/my_answer")

#####################
#     Problem 1     #
#####################

# download the dataset 

data("infants")

# Cox Proportional Hazard model

# Surv object 
child_surv <- with(infants, Surv(enter, exit, event))

# Model 
cox <- coxph(child_surv ~ sex + age, data = infants)
summary(cox)
drop1(cox, test = "Chisq")
stargazer(cox, type = "text")
stargazer(cox, type = "latex")

# Plotting 
cox_fit <- survfit(cox)
autoplot(cox_fit)

#Exp(coef)


exp <- exp(cbind(Odds_and_OR=coef(cox), confint(cox)))
stargazer(exp)
# There is a 0.485 decrease in the expected log of the hazard for male babies compared to 
# female, holding mother's age as a constant. There is a 0.04 increase in the expected log of the hazard
# for infants of with mother's age. The hazard ratio of male babies is 0.61 that of female babies


