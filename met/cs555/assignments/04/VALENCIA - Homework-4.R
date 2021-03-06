library(plyr)
setwd("/Users/anthony.valencia/met/cs555/assignments/04/")

# (1) Save the data to excel or CSV file and read into R for analysis.  (1 point)
occupations <- read.csv("occupations.csv")
occupations <- rename(
  occupations,
  c(
    "Occupational.Title"="occupation",
    "Education.Level..years."="education",
    "Income...."="income",
    "Percent.of.Workforce.that.are.Women"="fraction.females",
    "Prestige.Score"="prestige"
    )
  )
occupations



# (2) To get a sense of the data, generate a scatterplot to examine the association between
#     prestige score and years of education.  Briefly describe the form, direction, and
#     strength of the association between the variables.  Calculate the correlation.  (3 points)
plot(
  occupations$education,
  occupations$prestige,
  main="Prestige vs Education",
  xlab="Education (years)",
  ylab="Prestige Score",
  cex=0.2
)
# form: linear.  points tend toward a straight line pattern
# direction: there is a positive association between education and prestige
# strength of association: points tend to follow a clear pattern, and are thus strongly associated
cor(
  occupations$education,
  occupations$prestige
  )
# 0.8501769; education is very strongly correlated to prestige score



# (3) Perform a simple linear regression.  Generate a residual plot.  Assess whether the model
#     assumptions are met.  Are there any outliers or influence points?  If so, identify them
#     by ID and comment on the effect of each on the regression. (4 points)
# simple linear regression line
education.bar <- mean(occupations$education)
education.sd <- sd(occupations$education)
prestige.bar <- mean(occupations$prestige)
prestige.sd <- sd(occupations$prestige)
r <- cor(occupations$education, occupations$prestige)
beta1 <- r*prestige.sd/education.sd
beta1
# 5.360878
beta0 <- prestige.bar - beta1*education.bar
beta0
# -10.73198
# y = -10.73198 + 5.360878*x

# plot
m <- lm(occupations$prestige~occupations$education)
plot(
  occupations$education,
  occupations$prestige,
  axes=TRUE,
  frame.plot=TRUE,
  xlab='education',
  ylab='prestige'
  )
abline(m, lty=3, col="blue")

# residual plot
plot(
  fitted(m),
  resid(m),
  axes=TRUE,
  frame.plot=TRUE,
  xlab='fitted values',
  ylab='residue'
  )
plot(
  occupations$education,
  resid(m),
  axes=TRUE,
  frame.plot=TRUE,
  xlab='education',
  ylab='residue'
)
hist(resid(m))

# assumptions:
# relationship is linear
# observations are independent
# the variation of prestige around education is constant
# residuals are normally distributed

# outliers / influence points
boxplot.stats(occupations$prestige)$out
# there are no outliers in the data set as there is nothing exceptionally large in the y direction.
boxplot.stats(occupations$education)$out
# there are no influencing points with regard to education as there is nothing outlying in x.



# (4) Calculate the least squares regression equation that predicts prestige from education,
#     income and percentage of women.  Formally test whether the set of these predictors are
#     associated with prestige at the alpha=0.05 level.  (4 points)
m <- lm(occupations$prestige~occupations$education+occupations$income+occupations$fraction.females)
lm(formula = occupations$prestige ~ occupations$education + occupations$income + occupations$fraction.females)
# Intercept: -6.794334
# education: 4.186637
# income: 0.001314  
# fraction.females: -0.008905 
# y.hat = -6.794334 + 4.186637*education + 0.001314*income - 0.008905*fraction.females

k <- 3
n <- nrow(occupations)
reg.df <- k
res.df <- n - k - 1

total.ss <- sum((occupations$prestige - mean(occupations$prestige))^2)
reg.ss <- sum((fitted(m) - mean(occupations$prestige))^2)
resi.ss <- sum((occupations$prestige-fitted(m))^2)

qf(0.95, df1=reg.df, df2=res.df)
# 2.697423; reject if H0 >= 2.697423

f.statistic <- (reg.ss/reg.df)/(resi.ss/res.df)
f.statistic
# 129.1917

p.value <- 1-pf(f.statistic, df1=reg.df, df2=res.df)
p.value
# 0
# 129.1917 >= 2.697423.  There is significant evidence at the alpha=0.05 level that
# beta.education, beta.income, beta.fraction.females all do not equal 0.  That is,
# there is evidence of a linear association between prestige and education and/or income
# and/or fraction.females (here p<0.001).

R2 <- reg.ss/total.ss
R2
# 0.7981775



# (5) If the overall model was significant, summarize the information about the contribution
#     of each variable separately at the same significance level as used for the overall model
#     (no need to do a formal 5-step procedure for each one, just comment on the results of the
#     tests).  Provide interpretations for any estimates that were significant.   Calculate 95%
#     confidence intervals where appropriate. (4 points)
# The model is significant.
summary(m)
#                                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                  -6.7943342  3.2390886  -2.098   0.0385 *  
#   occupations$education         4.1866373  0.3887013  10.771  < 2e-16 ***
#   occupations$income            0.0013136  0.0002778   4.729 7.58e-06 ***
#   occupations$fraction.females -0.0089052  0.0304071  -0.293   0.7702  
qt(0.95, res.df)
# 1.660551
# reject H0 if |t| >= 1.660551
# reject education since 10.771 >= 1.660551
# reject income since 4.729 >= 1.660551
# do not reject fraction of females since 0.293 < 1.660551

# (6) Generate a residual plot showing the fitted values from the regression against the residuals.
#     Is the fit of the model reasonable? (2 points)
m <- lm(occupations$prestige~occupations$education+occupations$income+occupations$fraction.females)
summary(m)
resid(m)
par(mfrow=c(3,2))
plot(fitted(m), resid(m), axes=TRUE, frame.plot=TRUE, xlab='fitted values', ylab='residue')
plot(occupations$education, resid(m), axes=TRUE, frame.plot=TRUE, xlab='education', ylab='residue')
plot(occupations$income, resid(m), axes=TRUE, frame.plot=TRUE, xlab='income', ylab='residue')
plot(occupations$fraction.females, resid(m), axes=TRUE, frame.plot=TRUE, xlab='fraction.females', ylab='residue')
hist(resid(m))
# from the distribution of points in each residual plot, we can conclude that the fit of the model is reasonable.



# (7) Are there any outliers or influence points?  (2 points)
# Outliers in the y-direction tend to have large residuals.
# Outliers in the x-direction may or may not have large residuals but have the potential to be influential.
par(mfrow=c(1,1))
plot(fitted(m), resid(m), axes=TRUE, frame.plot=TRUE, xlab='fitted values', ylab='residue')
boxplot.stats(occupations$prestige)$out
# there are no outliers in the data set as there is nothing exceptionally large in the y direction.

plot(occupations$education, resid(m), axes=TRUE, frame.plot=TRUE, xlab='education', ylab='residue')
boxplot.stats(occupations$education)$out
# there are no influencing points with regard to education as there is nothing outlying in x.

plot(occupations$income, resid(m), axes=TRUE, frame.plot=TRUE, xlab='income', ylab='residue')
boxplot.stats(occupations$income)$out
# there are influencing points with regard to income 25879 19263 25308 14558 17498

plot(occupations$fraction.females, resid(m), axes=TRUE, frame.plot=TRUE, xlab='fraction.females', ylab='residue')
boxplot.stats(occupations$fraction.females)$out
# there are no influencing points with regard to fraction of females as there is nothing outlying in x.

