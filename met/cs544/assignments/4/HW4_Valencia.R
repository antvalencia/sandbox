# Part1) Binomial distribution (20 points)
# Suppose a pitcher in Baseball has 50% chance of getting a strike-out. Using the binomial distribution,
# a) Compute and plot the probability distribution for striking out the next 6 batters.
n <- 6
p <- 1/2
choose(n, n) * p^n * (1 - p)^0
dbinom(n, size = n, prob = p)
# b) Plot the CDF for the above
pmf = dbinom(0:n, size = n, prob = p)
plot(0:n, pmf, type = "h", xaxt = "n",
     main = "", xlab = "x", ylab = "PMF")
points(0:n, pmf, pch = 16)   
axis(side = 1, at = 0:n, labels = TRUE)
abline(h = 0, col="red")

cdf = c(0, cumsum(pmf))
cdf

# c) Repeat a) and b) if the pitcher has 70% chance of getting a strike-out.
p <- 0.7
choose(n, n) * p^n * (1 - p)^0
dbinom(n, size = n, prob = p)
dbinom(0:n, size = n, prob = p)

pmf = dbinom(0:n, size = n, prob = p)
plot(0:n, pmf, type = "h", xaxt = "n",
     main = "", xlab = "x", ylab = "PMF")
points(0:n, pmf, pch = 16)   
axis(side = 1, at = 0:n, labels = TRUE)
abline(h = 0, col="red")

cdf = c(0, cumsum(pmf))
cdf
# d) Repeat a) and b) if the pitcher has 30% chance of getting a strike-out.
p <- 0.3
choose(n, n) * p^n * (1 - p)^0
dbinom(n, size = n, prob = p)
dbinom(0:n, size = n, prob = p)

pmf = dbinom(0:n, size = n, prob = p)
plot(0:n, pmf, type = "h", xaxt = "n",
     main = "", xlab = "x", ylab = "PMF")
points(0:n, pmf, pch = 16)   
axis(side = 1, at = 0:n, labels = TRUE)
abline(h = 0, col="red")

cdf = c(0, cumsum(pmf))
cdf
# e) Infer from the shape of the distributions.
# c easier to get more strikouts when probability is .7 of strikeout as more are distributed toward the right of the x axis, where x is the number of strikeouts.
# d harder to get more strikouts when probability is .3 of strikeout as more are distributed toward the left of the x axis, where x is the number of strikeouts.

# Part2) Binomial distribution (15 points)
# Suppose that 80% of the flights arrive on time. Using the binomial distribution,
# a) What is the probability that four flights will arrive on time in the next 10 flights?
n <- 10
i <- 4
p <- 0.8
choose(n, i) * p^i * (1 - p)^(n-i)
dbinom(i, size = n, prob = p)
# b) What is the probability that four or fewer flights will arrive on time in the next 10 flights?
1 - sum(dbinom((i+1):n, size = n, prob = p))
pbinom(i, size = n, prob = p)

# c) Compute the probability distribution for the next 10 flights.
pmf = dbinom(0:n, size = n, prob = p)
pmf

# d) Show the PMF and the CDF for the next 10 flights.
# pmf
plot(0:n, pmf, type = "h", xaxt = "n",
     main = "", xlab = "x", ylab = "PMF")
points(0:n, pmf, pch = 16)   
axis(side = 1, at = 0:n, labels = TRUE)
abline(h = 0, col="red")
# cdf
cdf = c(0, cumsum(pmf))
cdf
cdfplot = stepfun(0:n, cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
     main = "", xlab = "x", ylab = "CDF")


# Part3) Poisson distribution (15 points)
# Suppose that on average 10 cars drive up to the teller window at your bank between 3 PM and 4 PM
#         and the random variable has a Poisson distribution. During this time period,
# a) What is the probability of serving exactly 3 cars?
n <- 10
y <- 3
ppois(y, lambda=n)
# b) What is the probability of serving at least 3 cars?
x <- 0
sum(ppois(x:y, lambda=n))
# c) What is the probability of serving between 2 and 5 cars (inclusive)?
x <- 2
y <- 5
sum(ppois(x:y, lambda=n))
# d) Calculate and plot the PMF for the first 20 cars.
x <- 0
y <- 20
pmf <- dpois(x:y, lambda=n)
pmf
plot(x:y, pmf, type="h", xlab="x", ylab="PMF", ylim = c(0, 0.25))
abline(h=0, col="red")



# Part4) Uniform distribution (15 points)
# Suppose that your exams are graded using a uniform distribution between 60 and 100.
x <- 60
y <- 100
# a) What is the probability of scoring
#    i) 60?
dunif(60, min=x, max=y)
#    ii) 80?
dunif(80, min=x, max=y)
#    iii) 100?
dunif(100, min=x, max=y)
# b) What is the mean and standard deviation of this distribution?
# mean
(y-x)/2
# stdev
`^`((`^`((y-x),2)/12),.5)
# c) What is the probability of getting a score of at most 70?
punif(70, min=60, max=100)
# d) What is the probability of getting a score greater than 80 (use the lower.tail option)?
punif(80, min=60, max=100, lower.tail = FALSE)
# e) What is the probability of getting a score between 90 and 100 (inclusive)?
sum(dunif(90:100, min=60, max=100))

# Part5) Normal distribution (20 points)
# Suppose that visitors at a theme park spend an average of $100 on souvenirs. Assume that the money spent
#   is normally distributed with a standard deviation of $10.
# a) Show the PDF of this distribution covering the three standard deviations on either side of the mean.
mu <- 100
sigma <- 10
sigma_count <- 3
x <- mu - (sigma_count*sigma)
y <- mu + (sigma_count*sigma)
s <- seq(x, y, sigma)
pdf = dnorm(s, mean = mu, sd = sigma)
pdf
# plot(s, pdf, type="l", col="green", xlim=c(x, y))

# b) What is the probability that a randomly selected visitor will spend more than $120?
1 - pnorm(120, mean = mu, sd = sigma)

# c) What is the probability that a randomly selected visitor will spend between $80 and $90 (inclusive)?
pnorm(90, mean = mu, sd = sigma) - pnorm(80, mean = mu, sd = sigma)

# d) What are the probabilities of spending within
# one standard deviation,
sigma_count <- 1
pnorm(mu + sigma_count*sigma, mean = mu, sd = sigma) -
  pnorm(mu - sigma_count*sigma, mean = mu, sd = sigma)
# two standard deviations, and
sigma_count <- 2
pnorm(mu + sigma_count*sigma, mean = mu, sd = sigma) -
  pnorm(mu - sigma_count*sigma, mean = mu, sd = sigma)
# three standard deviations, respectively?
sigma_count <- 3
pnorm(mu + sigma_count*sigma, mean = mu, sd = sigma) -
  pnorm(mu - sigma_count*sigma, mean = mu, sd = sigma)

# e) Between what two values will the middle 90% of the money spent will fall?
v <- 16.449
pnorm(mu + v, mean = mu, sd = sigma) -
  pnorm(mu - v, mean = mu, sd = sigma)
d <- c(mu - v, mu + v)

# f) Show a plot for 10,000 visitors using the above distribution.
visitors <- 10000
x <- seq(mu - v, mu + v, 2*v/visitors)
pdf <- visitors*dnorm(x, mean = mu, sd = sigma)
plot(x, pdf, type="l", col="red", xlim=d)



# Part6) Exponential distribution (15 points)
# Suppose your cell phone provider’s customer support receives calls at the rate of 18 per hour.
# a) What is the probability that the next call will arrive within 2 minutes?
pexp(2/60, rate=18)

# b) What is the probability that the next call will arrive within 5 minutes?
pexp(5/60, rate=18)

# c) What is the probability that the next call will arrive between 2 minutes and 5 minutes (inclusive)?
pexp(5/60, rate=18) - pexp(2/60, rate=18)


# d) Show the CDF of this distribution.
x <- seq(2/60, 5/60, by=1/60)
pdf <- dexp(x, rate=18)
pdf
plot(x, pdf, type="l", col="red", xlim=c(1/60,6/60))
abline(h=0)
cdf <- pexp(x, rate=18)
plot(x, cdf, type="l", col="red", xlim=c(1/60,6/60))


# Submission:
# Create a folder, CS544_HW4_lastName and place the following files in this folder.
# Write all the solutions in a single Word document, HW4_lastName.doc.
# For the code portions, provide all R code in a single file, HW4_lastName.r
# Archive the folder (CS544_HW4_lastName.zip). Upload the zip file to the Assignments section of Blackboard.