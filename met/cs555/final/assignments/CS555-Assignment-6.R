# install.packages("aod")
# install.packages("pROC")

library(aod)
library(stats)
library(pROC)
# setwd("~/Dropbox/Teaching/BU-Teaching/2018-Spring/BU-CS555-Data-Analytics-Class-Spring-2018/Assignment-Solutions")

# Load the smoker data set. 
data<- read.csv("dataAssignment6.csv")
attach(data)
# print a small part of the data 
head(data)



# The data in this document consists of body temperature measurements and heart rate measurements for 65 men and 65 women.  
# Save the data to excel and read the data into R.  Use this data to address the following questions.


# (1) We are interested in whether the proportion of men and women with body temperatures greater than or equal to 98.6 degrees Fahrenheit are equal.
# Therefore, we need to dichotomize the body temperature variable. 
# Create a new variable, called “temp_level” in which temp_level = 1 if body temperature >= 98.6 and temp_level=0 if body temperature < 98.6. (1 point)

data$temp_level=ifelse(data$temp>=98.6, 1, 0)

# have look at your data
head(data)


# (2) Summarize the data relating to body temperature level by sex. (2 points)

table(data$temp_level , data$sex)

femalProp<-(35/65)*100
maleProp<-(14/65)*100

# (3) Calculate the risk difference. Formally test (at the α=.05 level) whether the proportion of people with higher
# body temperatures (greater than or equal to 98.6) is the same across men and women, based on this effect measure. 
# Do females have higher body temperatures than males? (4.5 points)

femalProp - maleProp  

# > maleProp - femalProp
# [1] -32.30769
# Female group had a higher proportion 

prop.test( c(14,35) , c(65,65) , alternative = "two.sided", conf.level = 0.95, correct = FALSE )

# We reject null hyphotesis because the p-value  p-value = 0.0001444 is smaller than 0.05 

# X-squared = 14.444
z<-(14.444)^(1/2) 
z
# z-statistics is 3.800526


# (4) Perform a logistic regression with sex as the only explanatory variable.  
# Formally test (at the α=.05 level) if the odds of having a temperature greater than or equal to 98.6 is the same between males and females.  
# Include the odds ratio for sex and the associated 95% confidence interval based on the model in your summary and interpret this value.  
# What is the c-statistic for this model? (5.5 points)

data$girl<-ifelse(data$sex==2, 1, 0)
m<-glm(data$temp_level ~ data$girl , family = binomial )
summary(m)


# your reject the null hyphothsis because of significant p-values

# or you can also to it with boys as reference group 

data$boy <-ifelse(data$sex==1, 1, 0)
m2<-glm(data$temp_level ~ data$boy  , family = binomial )
summary(m2)





# calculating the ODDs ratio 
exp(cbind(OR = coef(m), confint.default(m)))

# or you can do it with the other group boys and inverse the interpretation of results 
exp(cbind(OR = coef(m2) , confint.default(m2) ))


data$prob<-predict(m,  type=c("response") )
g <-roc(data$temp_level ~ data$prob) 

print(g)

plot(g)



# (5) Perform a multiple logistic regression predicting body temperature level from sex and heart rate. 
# Summarize briefly the output from this model.  Give the odds ratio for sex and heart rate (for a 10 beat increase).  
# What is the c-statistic of this model?  (5 points)

m3 <- glm(data$temp_level ~ data$girl + data$heartRate, family = binomial)
summary(m3)

wald.test( b= coef(m3), Sigma = vcov(m3), Terms = 2:3 )

# calculating the ODDs ration 
exp(cbind(OR=coef(m3) , confint.default(m3)))
exp(m3$coefficients[3]*10)

# if we wanted cofindence interval for 10 unit increase in hear rate, we can do ...
exp(m3$coefficients[3] - qnorm(0.975) * summary(m3)$coefficients[3,2]*10)

exp(m3$coefficients[3] + qnorm(0.975) * summary(m3)$coefficients[3,2]*10)


data$prob<-predict(m3, type=c("response") )
print(data)

g<-roc(data$temp_level~ data$prob)
g
plot(g)

# (6) Which model fit the data better?  Support your response with evidence from your output. 
# Present the ROC curve for the model you choose. (2 points)

# Compare z statistics from both approaches. 
# multiple regression is better because of higher z 

plot( 1-g$specificities, g$sensitivities,    xlab = "1 - Specificity", ylab="Sensitivity", main="ROC Curve")
abline(a=0, b=1)

