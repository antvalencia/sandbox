# (1) Save the data to excel or CSV file and read into R for analysis.  (1 point)

setwd("/home/kia/Dropbox/Teaching/BU-Teaching/BU-CS555-Data-Analytics-Class/New-Assignments/Assignment-Solutions/")

# Load the smoker data set. 
data<- read.csv("dataAssignment4.csv")
attach(data)
# print a small part of the data 
head(data)

# (2) To get a sense of the data, generate a scatterplot to examine the association between prestige score and years of education.  
# Briefly describe the form, direction, and strength of the association between the variables.  Calculate the correlation.  (3 points)

plot(Education, Score)
cor(Education, Score)

# (3) Perform a simple linear regression.  Generate a residual plot.  Assess whether the model assumptions are met.  Are there any outliers or influence points?  
# If so, identify them by ID and comment on the effect of each on the regression. (4 points)

m<-lm(Score~Education)
summary(m)

abline(m)

plot(Education, resid(m))
abline(h=0)
hist(resid(m))


# You can sort the residuals 

sort(resid(m))
data[53,]

# remove the outlier 
data2<-data[-53]
summary(lm(data2$Score~data2$Education))

# (4) Calculate the least squares regression equation that predicts prestige from education, income and percentage of women. 
# Formally test whether the set of these predictors are associated with prestige at the  = 0.05 level.  (4 points)

m<-lm(Score~Education+Income+WorkforceWomen)
summary(m)


# (5) If the overall model was significant, summarize the information about the contribution of
# each variable separately at the same significance level as used for the overall model 
# (no need to do a formal 5-step procedure for each one, just comment on the results of the tests).  
# Provide interpretations for any estimates that were significant. 
# Calculate 95% confidence intervals where appropriate. (4 points)


confint(m)


# (6) Generate a residual plot showing the fitted values from the regression against the residuals. 
# Is the fit of the model reasonable?  Are there any outliers or influence points?   (4 points)

plot(fitted(m), resid(m))
abline(h=0)
hist(resid(m))



