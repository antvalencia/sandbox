
setwd("/home/kia/Dropbox/Teaching/BU-Teaching/BU-CS555-Data-Analytics-Class/New-Assignments/Assignment-Solutions/")


# (1) Save the data to excel or CSV file or Excel file and read into R for analysis. (Q1 -  2 points)

# Load the smoker data set. 
data<- read.csv("dataAssignment3.csv")
attach(data)


# (2) To get a sense of the data, generate a scatterplot (using an appropriate window, label the axes, and title the graph).  
# Consciously decide which variable should be on the -axis and which should be on the y-axis.  
# Using the scatterplot, describe the form, direction, and strength of the association between the variables. (Q2 - 3 points)

plot(data$NumFishMeals, data$TotalMercury, main="Scatterplot of Fish consumed vs. mercury levels", 
     xlab="Number of meals containing fish (per week)", ylab="mercury levels mg/g")


# (3) Calculate the correlation coefficient.  What does the correlation tell us? (Q3 - 2 points)

cor(data$NumFishMeals, data$TotalMercury)

# 0.6991094
# It is a pretty strong correlation 
# It tels us that there is a strong correlation between number of fish consumed per week and the total mercury levels. 

# (4) Find the equation of the least squares regression equation, and write out the equation.  Add the regression line to the scatterplot you generated above. (Q4 - 4 points)

m <- lm(data$TotalMercury~data$NumFishMeals)
print(m)
# Equation is y=0.276 X + 1.688

abline(m)


# (5) What is the estimate for  beta_1 ?   How can we interpret this value?  What is the estimate for beta_0 ?  What is the interpretation of this value?  (Q5 - 4 points)


summary(m)

# data$NumFishMeals  0.27595    0.02851   9.679 6.01e-16 ***

# Average Mercury level if someone eats zero fish is 1.688 
# Average increase unit of mercury level is 0.276 units for each single number of fish. 


# (6) Calculate the ANOVA table and the table which gives the standard error of   (hat beta 1) . 
# Formally test the hypothesis that beta_1 = 0 using either the F-test or the t-test at the  alpha level a=0.10.  
# Either way, present your results using the 5 step procedure as in the course notes.  Within your conclusion, calculate the R2 (R squared) value and interpret this.  
# Also, calculate and interpret the 90% confidence interval for  beta_1 .   (Q6 -  5 points)


anova(m)

summary(m)


# use the 5 steps 
# desion rule is if p value is less than alpha level of 0.10 we would reject the null hypothesis 
# if you use the F test then you can use the anova(m) and get P value of 6.013e-16 ***

# if you are using the T test then you can use the summary(m), t value of 9.679  and p-value of 6.01e-16 ***

# we reject the null hypothesis. There is a significat associations. 

# Multiple R-squared:  0.4888 
# Interpretation is that 48.88% of the variablity of the total mercury levels  is explained by the number of meal

confint(m, level=0.90)

#                       5 %      95 %
#  (Intercept)       1.192253 2.1830324
# data$NumFishMeals  0.228609 0.3232916


