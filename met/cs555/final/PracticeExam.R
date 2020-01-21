hospitals <- read.table("https://bit.ly/2qZwdMn", header=T)
hospitals <- hospitals[,-1]
hospitals$Region <- as.factor(hospitals$Region)
hospitals$Med.school <- as.factor(hospitals$Med.school)

# 1.  Which single variable out of the 10 variables would you use to predict the infection risk value?
#     Describe why you select that variables.
# Infection.risk

# 2.  Consider your answer for previous question, consider only one single independent variable and
#     fit a simple linear regeression (SLR) model to the data.
#     Provide the equation of your SLR.
xbar <- mean(hospitals$Culture)
sx <- sd(hospitals$Culture)
ybar <- mean(hospitals$Infection.risk)
sy <- sd(hospitals$Infection.risk)
r <- cor(hospitals$Culture, hospitals$Infection.risk)
beta1 <- r*sy/sx
beta0 <- ybar - beta1*xbar

#     How well your model can explain the variability of the response data?
# 

# 3.  Which other variables are significant predictors to be used in Multiple Linear Regression for
#     prediciting infection risk?

# 4.  Which other variables would you use to predicit infection risk using Multiple Linear Regression?

# 5.  Provide  a  multiple  linear  regression  equation.   How  well  can  your  MLR  model  predict  the
#     infection risk?

# 6.  Are the infection risks in us Regions different?  Write your Hypothesis and provide significant
#     tests.

# 7.  Is  number  of  Nurses  in  each  hospital  a  significan  covariate?   Are  the  differences  in  different
#     region driven by the number of Nureses?

# 8.  Considering the case that Hospitals are affiliated medical school and they are in different reigons,
#     are all of these hospitals different in terms of infection risks?

# 9.  Which tuples of variables (x, y) can be used to predict the other variables?  Check all possible
#     combination and which pair has the highest model fittness?  Provide reasons.









creditAppData <- read.csv("https://bit.ly/2HRjkxj")
head(creditAppData)
summary(creditAppData)
attach(creditAppData)
# 1.  Is “Employed” a good predictor of getting credit approval?  What is the c-statistic?

# 2.  What is the risk difference between Employed vs. Unemployed for getting credit approval?

# 3.  Are the varaibles Employed and Debt together good predictor for credit approval?

# 4.  What is the risk difference for 1 unit increase in debt?

# 5.  What is the ODD ration confidence interval for 1 unit increase in debt?

# 6.  Is there a correlation between Income, Credit Score, and YearsEmployed and the credit approval status?

