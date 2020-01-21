# Quiz on Stats

# Q1: (5 points). Assign your First name Space Last name to an R object called "My.Name"
# filename: Anthony Valencia.R


# Q2: (10 points). Using "dbinom()" plot Binomial probability distribution with the following arguments:
#     number of trials = 150, and probability of success for a single trial = 0.33.
n.trials <- 150
success.p <- 0.33
x <- seq(from=0, to=n.trials, by=1)
y <- dbinom(x, size=n.trials, prob=success.p)
plot(x, y)
title("Binomial probability distribution")


# Q3: (10 points). Hypothesis testing. Note that the correlation is a value between (-1 and 1), 1 being
#     correlated, zero being not correlated, and -1 being anti-correlated. Use dataset “mtcars” and R’s
#     correlation function “cor()” to answer the question are Mpg (miles per gallon) & Hp (horse powers)
#     correlated? The null hypothesis is that they ARE NOT correlated, which is a reasonable assumption.
#     If needed, familiarize yourself with the dataset by using “?mtcars”.

#         a. Write a line of code to find the correlation of Mpg & Hp.
cor(mtcars$mpg, mtcars$hp)
# value: -0.776168 (anti-correlated)

#         b. Use “cor.test()” of Mpg & Hp to find the p-value.
cor.test(mtcars$mpg, mtcars$hp)
# p-value: 1.788e-07

#         c. Based on the p-value, write a clear comment in your code if the null hypothesis has to be kept or rejected.
# The p-value is less than the significance level (α = 0.05), Decision: Reject the null hypothesis



# Statistical inference. Analysis of variance (ANOVA) is a statistical tool used in several ways to
# develop and confirm an explanation for the observed data. ANOVA is useful for comparing (testing)
# three or more means (groups or variables) for statistical significance. For example: Study effects
# of tea (X) on weight loss (Y). Consider ANOVA’s Null Hypothesis that the mean accross the category
# Y and X are equal. If p.value < 0.05 then the Null Hypothesis is rejected.

# Q4: (15 points). Consider the dataset “mtcars” and R’s analysis of variance function “aov(Y~X)” to answer
#     the question:

#         a. Does Mpg depend on the number of Cylinders?
#            - take Y to be mtcars$mpg
Y <- mtcars$mpg
#            - take X to be factor(mtcars$cyl)
X <- factor(mtcars$cyl)

#         b. To get the p-value, you would need to execute the following code
#            - model <- aov(Y~X)
model <- aov(Y~X)
#            - summary(model)
summary(model)

#         c. Based on the p-value, write a clear comment in your code if the null hypothesis has to be kept or rejected.
# p-value (4.98e-09) is less than 0.05, Decision: Reject the null hypothesis
# to answer (a), it does appear that Mpg depends on the number of cylinders


# Q5: (10 points). Repeat the same as Q4, but for Gross horsepower (mtcars$hp) and Weight (1000 lbs)
#     a. Y <- mtcars$hp
Y <- mtcars$hp
#     b. X <- factor(mtcars$wt)
X <- factor(mtcars$wt)
#     c. Based on the p-value, write a clear comment in your code if the null hypothesis has to be kept or rejected.
#            - model <- aov(Y~X)
model <- aov(Y~X)
#            - summary(model)
summary(model)
# p-value (0.239) is not less than 0.05, Decision: The null hypothesis cannot be rejected.  More testing is needed
#     in order to draw a conclusion about significance of a comparison between the variables of horsepower and weight
#     in this car dataset.

