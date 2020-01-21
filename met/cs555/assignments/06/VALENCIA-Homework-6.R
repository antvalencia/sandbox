# The data in this document consists of body temperature measurements and heart rate measurements for 65 men and 65
# women.  Save the data to excel and read the data into R.  Use this data to address the following questions.

library(plyr)
setwd("/Users/anthony.valencia/met/cs555/assignments/06/")
data <- read.csv("data.csv")


# (1) We are interested in whether the proportion of men and women with body temperatures greater than or equal to
# 98.6 degrees Fahrenheit are equal. Therefore, we need to dichotomize the body temperature variable. Create a new
# variable, called “temp_level” in which temp_level = 1 if body temperature >= 98.6 and temp_level=0 if body temperature
# < 98.6. (1 point)
data$temp_level <- ifelse(data$temp>=98.6, 1, 0)


# (2) Summarize the data relating to body temperature level by sex. (2 points)
dat1 <- data.frame(
  sex = factor(c("female", "female", "male", "male")),
  temp_level = factor(c(0, 1, 0, 1), levels=c(0, 1)),
  percentage = c(
    nrow(data[data$sex==2 & data$temp_level==0,])*100/65,
    nrow(data[data$sex==2 & data$temp_level==1,])*100/65,
    nrow(data[data$sex==1 & data$temp_level==0,])*100/65,
    nrow(data[data$sex==1 & data$temp_level==1,])*100/65
  )
)
library(ggplot2)
ggplot(data=dat1, aes(x=sex, y=percentage, fill=temp_level)) + geom_bar(stat="identity")
# females run slightly hotter with a little over 50% of females with a temp level above 98.6F.
# males run mostly colder with around 22% running below 98.6F.

# (3) Calculate the risk difference.  Formally test (at the α=.05 level) whether the proportion of people with higher
# body temperatures (greater than or equal to 98.6) is the same across men and women, based on this effect measure.
# Do females have higher body temperatures than males? (4.5 points)
female_0 <- nrow(data[data$sex==2 & data$temp_level==0,])
female_1 <- nrow(data[data$sex==2 & data$temp_level==1,])
male_0 <- nrow(data[data$sex==1 & data$temp_level==0,])
male_1 <- nrow(data[data$sex==1 & data$temp_level==1,])
p_hat_1 <- female_0/(female_0+female_1)
p_hat_2 <- male_0/(male_0+male_1)
p_hat_1-p_hat_2
# -0.3230769.  Meaning about 32% more males have a colder temp level.  Here 0 (<98.6) was taken as the success event.
# 1) Set up the hypotheses and select the alpha level
# h0: p1 = p2
# h1: p1 != p2
# alpha=0.05
#
# 2) Select the appropriate test statistic
# z = (p_hat_1 - p_hat_2)/(sqrt(p_hat*(1-p_hat)*((1/n1)+(1/n2))))
#
# 3) State the decision rule
# Critical value from the standard normal distribution for a right hand tail probability of alpha/2 = 0.05/2 = 0.025
# Decision Rule: Reject h0 if |z| >= 1.960
# Otherwise, do not reject h0
# 4) Compute the test statistic and the associated p-value
n1 <- nrow(data[data$sex==2,])
n2 <- nrow(data[data$sex==1,])
p_hat <- (female_0+male_0)/(n1+n2)
z <- (p_hat_1 - p_hat_2)/(sqrt(p_hat * (1-p_hat) * (1/(n1)+(1/n2)) ))
abs(z)
# 3.800585
# 5) reject h0 since 3.800585>=1.960.
# females do have higher body temperatures than men as shown from p_hat_1-p_hat_2 value.


# (4) Perform a logistic regression with sex as the only explanatory variable.  Formally test (at the α=.05 level) if
# the odds of having a temperature greater than or equal to 98.6 is the same between males and females.   Include the
# odds ratio for sex and the associated 95% confidence interval based on the model in your summary and interpret this
# value.  What is the c-statistic for this model? (5.5 points)
m <- glm(data$temp_level ~ data$sex, family=binomial)
summary(m)
beta_1 <- 1.4469
se_beta_1 <- 0.3911
# 1) Set up the hypothesis and select the alpha level
# h0: beta_1=0 or =1 (there is no association between sex and temp level)
# h1: beta_1!=0 or !=1 (there is an association between sex and temp level)
# alpha=0.05
#
# 2) select the appropriate test statistic
# z = beta_1/se_beta_1
#
# 3) state the decision rule
# determine the appropriate value from the standard normal distribution associated with the right hand tail
# probability of alpha/2=0.025
# using the table, this is 1.960
# decision rule: reject h0 if |z|>=1.960 or reject h0 if p<=a.  otherwise, do not reject h0.
#
# 4) compute the test statistic
z <- beta_1/se_beta_1
# z=3.699565
#
# 5) conclusion
# reject h0 since 3.699565>=1.960.  or since <=alpha.  we have significant evidence at the alpha=0.05 level
# that beta_1!=0.  That is, there is evidence of an association between sex and temp level.
#
# c-statistic
library(pROC)
data$prob <- predict(m, type=c("response"))
g <- roc(data$temp_level ~ data$prob)
# Area under the curve (c-statistic): 0.672
plot(g)

# (5) Perform a multiple logistic regression predicting body temperature level from sex and heart rate.  Summarize
# briefly the output from this model.  Give the odds ratio for sex and heart rate (for a 10 beat increase).  What is
# the c-statistic of this model?  (5 points)
m <- glm(data$temp_level ~ data$sex + data$Heart.rate, family=binomial)
summary(m)
beta_1 <- 1.38919     # sex
se_beta_1 <- 0.39868  # sex
beta_2 <- 0.06337     # heart_rate
se_beta_2 <- 0.02850  # heart_rate
# odds ratio
# per 10 unit increase:
exp(m$coefficients[2]*10)
# sex: 1079345
exp(m$coefficients[3]*10)
# heart rate: 1.884521
# c-statistic
data$prob <-predict(m, type=c("response"))
g <- roc(data$temp_level ~ data$prob)
# Area under the curve (c-statistic): 0.7289
plot(g)

# (6) Which model fit the data better?  Support your response with evidence from your output.  Present the ROC curve for
# the model you choose. (2 points)
