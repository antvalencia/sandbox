
# Question 1) 
# (1) Summarize the data by whether children participated in the meal preparation or not.  
# Use an appropriately labelled table to show the results.  Also include a graphical presentation that shows the distribution of calories for participants vs. non-participants.  Describe the shape of each distribution and comment on the similarity (or lack thereof) 
# between the distributions in each population.


# setwd("/home/kia/Desktop/")

data  <- read.csv("kid_calories.csv")

# data in the CSV file is like this

# calorie,parti
# 435.16,1
# 338.99,1
# 488.73,1


part<- data$calorie[data$parti==1]
non_part<- data$calorie[data$parti==0]


aggregate(data$calorie, by=list(data$parti), FUN=summary )
aggregate(data$calorie, by=list(data$parti), FUN=sd )

length(part)
length(non_part)

# make plots 
par(mfrow=c(2,2))
boxplot(data$calorie~data$parti)

# attach the data so that we can work with variable names directry 
attach(data)

means <-tapply(calorie, parti, mean )
# lower bound 
lower <-tapply(calorie, parti, function(v) t.test(v)$conf.int[1])
# upper bound 
upper <-tapply(calorie, parti, function(v) t.test(v)$conf.int[2])

library("gplots")

barplot2(means, plot.ci = TRUE, ci.l=lower, ci.u = upper, 
         names.arg = c("Participants", "Non-Participants"), 
         xlab="Paritipants Status",
         main="Mean Calories by Paricipants Status",
         ylab="Mean Calories Consumed",
         col="green1", ylim=c(0,500))

hist(part)
hist(non_part)



# Question 2 
# Does the mean calorie consumption for those who participated in the meal preparation 
# differ from 425? Formally test at the level using the 5 steps outlined in the module.


# H0: μ = 425
# H1: μ ≠ 425 

mean(part)

t.test(data$calorie[data$parti==1], mu=425, alternative="two.sided")

# t = 0.30272, df = 24, p-value = 0.7647
# p value is grater than 0.5 
# We failed to reject H0 
# 

# Question 3)
# Calculate a 90% confidence interval for the mean calorie intake for participants 
# in the meal preparation.  Interpret the confidence interval.

t.test(data$calorie[data$parti==1], mu=425, alternative="two.sided", conf.level=0.90)

# Results
# 90 percent confidence interval:
# 395.2311 467.5681


# Question 4) 
# Formally test whether or not participants consumed more calories than non-participants 
# at the level using the 5 steps outlined in the module.

# Because we ask if the means are "more", it makes it a one sided test.   

# H0: μ1 = μ2 (the mean calorie consumption for participants in the meal preparation and non-participants are the same)
# H1: μ1 > μ2 (the mean calorie consumption for participants in the meal preparation is greater than the mean calorie consumption for non-participants)
# α = 0.05

# I list the non participants first, then alternative is "less"
t.test(data$calorie[data$parti==0], data$calorie[data$parti==1], alternative = "less")

# Decision Rule is if p is less than 0.5 we reject the H0, if not we fail to reject the H0

# Results: 
# t = 2.8248, df = 44.779, p-value = 0.9965

mean(data$calorie[data$parti==0]) - mean( data$calorie[data$parti==1])
# -84.60051

# Conclusion
# We reject the H0 in favor of H1



# (5) Are the assumptions of the test used in (4) met?  How do you know?
# Assuptions are met because the distributions are approximately normally distributed (you can see on histograms). 
# And the distributions are approximately symmetric. 

# In this case, as long as the underlying distributions have similar shapes and 
# the sample sizes are both≥5, the approximation using the t.test is quite good.
# Because the two distributions have similar shapes and the sample sizes are both ≥ 5,
# the approximation is quite good. 















         
