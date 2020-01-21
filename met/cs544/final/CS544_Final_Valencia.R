# CS544 Final Exam
# Anthony Valencia
# Success! Your submission appears on this page. The submission confirmation number is a356a461-0613-45bd-84cc-df3357de366d. Copy and save this number as proof of your submission.

# Q1
# Given a vector, show the R code for finding the element in the vector has the longest run (same value consecutively)
# Sample input:
# c(10,20,20,40,40,40,40,40,50,50,10,40)
# Output: 40
s <- c(10,20,20,40,40,40,40,40,50,50,10,40)
lens <- data.frame(unclass(rle(s)))
lens[lens$lengths == max(lens$lengths), ]$values


# Q2
# Consider a class of students with 6 boys and 4 girls.
q2.boys <- 6
q2.girls <- 4
# Setup the experiment for selecting 5 unique students for representing the class as one group at the science fair.
q2.l <- rep(c("B", "G"), times=c(q2.boys,q2.girls))
q2.students <- 5
q2.m <- urnsamples(q2.l, size = q2.students, ordered = TRUE)
# Add the probability space for this experiment.
q2.s <- probspace(q2.m)
# Add the random variable for the number of boys.
countBoys <- function(x) { return (sum(x=="B")) }
q2.b.s <- addrv(q2.s, FUN=countBoys, name="COUNT.B")
# Show the marginal distribution for this random variable.
marginal(q2.b.s, vars="COUNT.B")
# Add the random variable for the number of girls.
countGirls <- function(x) { return (sum(x=="G")) }
q2.g.s <- addrv(q2.s, FUN=countGirls, name="COUNT.G")
# Show the marginal distribution for this random variable.
marginal(q2.g.s, vars="COUNT.G")


# Q3
# A random variable has the normal distribution mean 250 and standard deviation 50.  Show
# how you would do the following with R code.  Write the solutions and interpretation in the box below.
q3.mu <- 250
q3.sigma <- 50
# a) Determine and interpret the quartiles of this variable.
q3.quartiles <- c(0.25,0.5,0.75)
qnorm(q3.quartiles, mean = q3.mu, sd = q3.sigma)
# Interpretation: 50% of all values distributed about the mean reside between 216.3 and 283.7
# b) Find the value that 85% of all possible values of this variable exceed.
q3.percent <- 1-0.85
qnorm(q3.percent, mean = q3.mu, sd = q3.sigma)
# c) Find the two values that divide the area under the corresponding normal curve into a
#    middle area of 90% and two outside areas of 5% each.  Interpret your answer.
q3.area.under.curve.90 <- c(0.05,0.95)
qnorm(q3.area.under.curve.90, mean = q3.mu, sd = q3.sigma)
# Interpretation: 90% of all values distributed about the mean reside between 167.8 and 332.2.
# 5% reside below 167.8 and 5% reside above 332.2.


# Q4
# Consider teh votes.reput dataset from "cluster" library, initialized with the following
# code.  Show how you would do the following with R code.
library(cluster)
library(sampling)
data(votes.repub)
data <- votes.repub
# The rownames of the data frame are the names of the states
# a) Add the column named Letter to the data frame initialized with the first letter of the
# state.  Use substring(rownames(data), 0, 1) for this purpose.
data$Letter <- substring(rownames(data), 0, 1)
rownames(data)
# b) Show the contingency table for this Letter.
q4.t <- table(data$Letter)
# c) Using the stratified sampling with the Letter as the stratum, select a sample that
#    selects one state from each stratum.
q4.st <- strata(
  data,
  stratanames = c("Letter"),
  size = rep(1, length(q4.t)),
  method = "srswor",
  description = TRUE
  )
q4.st.sample <- getdata(data, q4.st)
# d) Compuare the means of the data for the year 1976 with the mean of the selected sample.
mean(q4.st.sample$X1976)


# Q5
# A random variable has normal distribution mean 250 and standard deviation 50.  Show how
# you would do the following with R code.  Set the seed to 123.
set.seed(123)
q5.mean <- 250
q5.sd <- 50
q5.data <- rnorm(10000, mean=q5.mean, sd=q5.sd)
# a) Draw a sample of size 50 from this data.  Show the 95.44% confidence interval using this
#    sample.
q5.size <- 50
q5.sd.sample.means <- q5.sd/sqrt(q5.size)
q5.sample.data <- sample(q5.data, size=q5.size)
q5.xbar <- mean(q5.sample.data)
cat("95.44% Conf Interval = ",
    q5.xbar - 2*q5.sd.sample.means, "-", 
    q5.xbar + 2*q5.sd.sample.means, "\n")
# b) Using the sample from a), compute the confidence intervals and precisions for the
#    confidence levels 75%, 85%, 95% and 99%, respectively.
q5.conf <- c(75, 85, 95, 99)
q5.alpha <- 1 - q5.conf/100   # alpha values for confidence
for (i in q5.alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), z: %.2f , %.2f",
                 100*(1-i), i, 
                 qnorm(i/2),
                 qnorm(1-i/2))
  cat(str,"\n")
}
for (i in q5.alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 q5.xbar - qnorm(1-i/2) * q5.sd.sample.means,
                 q5.xbar + qnorm(1-i/2) * q5.sd.sample.means)
  cat(str,"\n")
}
for (i in q5.alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qnorm(1-i/2) * q5.sd.sample.means)
  cat(str,"\n")
}
# c) For the 90% confidence level, what is the margin of error with the sample used in a)?
q5.conf <- 90
q5.alpha <- 1 - q5.conf/100   # alpha values for confidence
q5.error <- qnorm(1 - q5.alpha/2) * q5.sd.sample.means
q5.error
#    What sample sizes are needed to have the margin of error value of 5, 2, and 1, respectively?
for (q5.error in  c(5.0, 2.0, 1.0)) {
  required.size <- (qnorm(1 - q5.alpha/2) * q5.sd/q5.error)^2
  required.size <- round(required.size)
  str <- sprintf("Error = %.2f, Required sample size = %d", 
                 q5.error, required.size)
  cat(str, "\n")
}


# Q6
# Use the data froam Q5
# a) For the two-tailed test, what is the null hypothesis and alternative
#    hypothesis for the population mean?
# null hypothesis mu=250; alternative hypothesis mu!=250
q6.mu0 <- q5.mean
q6.sigma <- q5.sd
q6.n <- q5.size
q6.z <- (q5.xbar - q6.mu0) / (q6.sigma / sqrt(q6.n))
q6.z
# 0.3995
# b) Using the critical-value approach, and the sample used in Q6, show the
#    results for significance levels 0.1, 0.05 and 0.01 if the null hypothesis
#   should be rejected or not.
q6.alpha.10 <- 0.10
c(qnorm(q6.alpha.10/2), qnorm(1 - q6.alpha.10/2))
# -1.645  1.645  ==> null hypothesis is not rejected
q6.alpha.05 <- 0.05
c(qnorm(q6.alpha.05/2), qnorm(1 - q6.alpha.05/2))
# -1.96  1.96  ==> null hypothesis is not rejected
q6.alpha.01 <- 0.01
c(qnorm(q6.alpha.01/2), qnorm(1 - q6.alpha.01/2))
# -2.576  2.576  ==> null hypothesis is not rejected