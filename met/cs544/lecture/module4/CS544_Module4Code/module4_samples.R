#### 2. Discrete Distributions

#### 2.2. Random Variable Example (Number of Heads)

# HHH, HHT, HTH, HTT, THH, THT, TTH, TTT
# P(X), where X is event of heads

# number of occurrences of heads
x <- c(0, 1, 2, 3)
# same as x <- 0:3

# probability of heads for each of number of occurrences
f <- c(1/8, 3/8, 3/8, 1/8)

# mean (expected value) of the probability distribution of the discrete random variable
mu <- sum(x * f)
mu

# variance of the discrete distribution
sigmaSquare <- sum((x - mu)^2 * f)
sigmaSquare

# standard deviation
sigma <- sqrt(sigmaSquare)
sigma

# cumulative distribution function (CDF) of the random variable X (P(X <= x))
F <- cumsum(f)
F

#### 2.3. Random Variable Example (Age of Students)

ages <- c(21,25,27,23,21,21,25,25,21,27)
ctable <- table(ages)
ctable

# frequency of ages as dataframe
dframe <- as.data.frame(ctable)
dframe

# the distinct ages
x <- as.numeric(as.character(dframe$ages))
x

# the probability distribution
f <- dframe$Freq / (sum(dframe$Freq))
f

#### 2.4. Discrete Uniform Distribution

# Single Die
x <- 1:6

# probability mass function
f <- rep(1/6, 6)

# mean
mu <- sum(x * f)
mu

# variance
sigmaSquare <- sum((x - mu)^2 * f)
sigmaSquare

# PMF and CDF example
m <- 6
dunif(1, max = m)

pmf <- dunif(1:m, max = m)
pmf

cdf <- punif(1:m, max = m)
cdf

#Plot PMF
plot(1:m, pmf, type="h",
  xlab="x",ylab="PMF", ylim = c(0, 0.2))
abline(h=0, col="red")

# Quantile
qunif(0.5, max=6)

# Generate Uniform Data

sample(6, size = 20, replace = TRUE)

sample(10:20, size = 5, replace = TRUE)

sample(c("H", "T"), size = 10, replace = TRUE)



#### 2.7. Bernoulli Trials

p <- 1/4
sample(0:1, size = 10, replace = TRUE, 
  prob = c(1 - p, p))

p <- 3/4
sample(0:1, size = 10, replace = TRUE, 
  prob = c(1 - p, p))

#### 2.10. Example Using R—Tossing 5 Coins

n <- 5; p <- 1/2

choose(n,3) * p^3 * (1 - p)^2

dbinom(3, size = n, prob = p)

dbinom(0:n, size = n, prob = p)

dbinom(c(1,5), size = n, prob = p)

sum(dbinom(0:3, size = n, prob = p))

pbinom(3, size = n, prob = p)

sum(dbinom(4:n, size = n, prob = p))

1 - pbinom(3, size = n, prob = p)

pbinom(3, size = n, prob = p, lower.tail = FALSE)


# PMF & CDF Example
n = 3; p = 0.8;
pmf = dbinom(0:n, size = n, prob = p)
pmf

plot(0:n, pmf, type = "h", xaxt = "n",
   main = "", xlab = "x", ylab = "PMF")
points(0:n, pmf, pch = 16)   
axis(side = 1, at = 0:n, labels = TRUE)
abline(h = 0, col="red")

cdf = c(0, cumsum(pmf))
cdf

cdfplot = stepfun(0:n, cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
  main = "", xlab = "x", ylab = "CDF")
  

# Plot PMF
n = 5; p = 0.5;
heights <- dbinom(0:n, size = n, prob = p)
plot(0:n, heights, type = "h",
  main = "Spike plot of X", xlab = "x", ylab = "PMF")
points(0:n, heights, pch = 16)   

# Repeat plot with n = 50; p = 1/2
# Repeat plot with n = 50; p = 0.7
# Repeat plot with n = 50; p = 0.3

#### 2.11. Plotting the CDF

n <- 5; p <- 1/2;
pmf <- dbinom(0:n, size = n, prob = p)
cdf <- c(0, cumsum(pmf))
cdfplot <- stepfun(0:n, cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
  main = "Step plot", xlab = "x", ylab = "CDF")

# Repeat plot with n = 50; p = 1/2
# Repeat plot with n = 50; p = 0.7
# Repeat plot with n = 50; p = 0.3

# Quantile
qbinom(0.8125, size=5, prob=1/2)

# Random numbers

rbinom(10, size=5, prob=1/2)

y <- rbinom(1000, size=5, prob=1/2)
table(y)
plot(table(y), type="h", col="red")


#### 2.12. Hypergeometric Distribution

# PMF & CDF Example
M <- 5; N <- 3; K <- 2
pmf <- dhyper(0:K, m = M, n = N, k = K)
pmf

cdf <- phyper(0:K, m = M, n = N, k = K)
cdf
# same as  cdf <- cumsum(pmf)

#Quantile
qhyper(0.64, m = M, n = N, k = K)

# random numbers

rhyper(10,m = M, n = N, k = K )

#### 2.13. Example Using R—Faulty Chips

M <- 20; N <- 980; K <- 50
dhyper(2, m = M, n = N, k = K)

sum(dhyper(0:2, m = M, n = N, k = K))
phyper(2, m = M, n = N, k = K)

phyper(2, m = M, n = N, k = K, lower.tail = FALSE)


# Plot PMF
pmf <- dhyper(0:K, m = M, n = N, k = K)
plot(0:K,pmf,type="h",
  xlab="x",ylab="PMF",ylim=c(0,0.5))
abline(h=0)

# Plot CDF
cdf <- c(0, cumsum(pmf))
cdfplot <- stepfun(0:K, cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
  main = "", xlab = "x", ylab = "CDF")

#### 2.14. Geometric Distribution

p <- 0.5
dgeom(2, prob = p)

pmf <- dgeom(0:10, prob = p)
pmf

#Plot PMF
plot(0:10,pmf,type="h",
  xlab="x",ylab="PMF")
abline(h=0, col="red")

#### 2.15. Negative Binomial Distribution

r <- 3; p <- 0.5
dnbinom(5, size = r, prob = p)

pmf <- dnbinom(0:10, size = r, prob = p)
pmf

#Plot PMF
plot(0:10,pmf,type="h",
  xlab="x",ylab="PMF", ylim = c(0, 0.1))
abline(h=0, col="red")

pnbinom(5, size = r, prob = p)

cdf <- c(0, cumsum(pmf))
cdf

cdfplot <- stepfun(0:10, cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
  main = "", xlab = "x", ylab = "CDF")


#### 2.16. Poisson Distribution

dpois(6, lambda=8)

ppois(2, lambda=8)

ppois(10, lambda=8) - ppois(4, lambda=8)

diff(ppois(c(4,10), lambda=8))

pmf <- dpois(0:40, lambda=16)
pmf

#Plot PMF
plot(0:40,pmf,type="h",
  xlab="x",ylab="PMF", ylim = c(0, 0.25))
abline(h=0, col="red")

##############################
#### 3. Continuous Distributions


#### 3.1. Figure plot

par(mfrow=c(1,3))

x <- seq(-10,10,0.01)
pdf = dnorm(x,0,2)
plot(x, pdf, type="h", main="Normal Distribution",
  col="lightblue", xaxt="n", yaxt="n")

x <- seq(0,10,0.01)
pdf = dunif(x,0,10)
plot(x, pdf, type="h", main="Uniform Distribution",
 col="lightblue", xaxt="n", yaxt="n")

x <- seq(0,12,0.001)
pdf = dexp(x,0.4)
# Plot PDF
plot(x,pdf,type="h", main="Exponential Distribution",
  col="lightblue", xaxt="n", yaxt="n")
abline(h=0)

par(mfrow=c(1,1))

#### 3.3. Example 1—Uniform Distribution

# Plot

x <- seq(0,1, 0.001)
pdf = dunif(x,0,1)
plot(x, pdf, type="h", main="Uniform Distribution",
 col="lightblue", xaxt="n", yaxt="n", ylim=c(0,1.5))
axis(side = 1, at = seq(0,1,by=0.1), labels = TRUE)
axis(side = 2, at = c(0,1), labels = TRUE)

##

punif(0.4, min=0, max=1) - punif(0.2, min=0, max=1)

#### 3.4. Example 2—Uniform Distribution

1 - punif(5, min=2.5, max=6.5)
punif(5, min=2.5, max=6.5, lower.tail=FALSE)

#### 3.5. Normal Distribution

# Plots
x <- seq(-6,6,0.1)
pdf.1 = dnorm(x, mean = 0, sd = 0.5)
pdf.2 = dnorm(x, mean = 0, sd = 1)
pdf.3 = dnorm(x, mean = 0, sd = 2)

plot(x, pdf.1, type="l", col="green", xlim=c(-6,6))
lines(x, pdf.2, col="red")
lines(x, pdf.3, col="blue")


x1 <- seq(-7,1,0.1)
pdf.1 = dnorm(x1, mean = -3, sd = 1)

x2 <- seq(-4,4,0.1)
pdf.2 = dnorm(x2, mean = 0, sd = 1)

x3 <- seq(2,10,0.1)
pdf.3 = dnorm(x3, mean = 6, sd = 1)

plot(x1, pdf.1, type="l", col="green", xaxt="n", xlim=c(-8,12))
lines(x2, pdf.2, col="red")
lines(x3, pdf.3, col="blue")
axis(side = 1, at = seq(-8,12,by=2), labels = TRUE)

#### 3.6. Example—Normal Distribution
# Gestation example

x <- seq(212,320)
pdf <- dnorm(x, mean = 266, sd = 16)

plot(x, pdf, type="l", col="red", 
  xlim=c(212,320), ylim=c(0,0.03),
  xaxt="n", yaxt="n",
  main="Gestation Period", xlab="Days", ylab="PDF")
axis(side = 1, at = c(218,234,250,266,282,298,314), 
  labels = TRUE) 
axis(side = 2, at = c(0,0.01,0.02,0.03), 
  labels = TRUE) 

mu <- 266; sigma <- 16

pnorm(mu, mean = mu, sd = sigma)

pnorm(mu - 3*sigma, mean = mu, sd = sigma)

pnorm(mu + 3*sigma, mean = mu, sd = sigma) -
 pnorm(mu - 3*sigma, mean = mu, sd = sigma)
 
pnorm(mu + 2*sigma, mean = mu, sd = sigma) -
 pnorm(mu - 2*sigma, mean = mu, sd = sigma)

pnorm(mu + sigma, mean = mu, sd = sigma) -
 pnorm(mu - sigma, mean = mu, sd = sigma)
 

#CDF

x <- seq(212,320)
cdf <- pnorm(x, mean = 266, sd = 16)
plot(x, cdf, type="l", col="red", 
  xlim=c(212,320), ylim=c(0,1),
  xaxt="n",
  main="Gestation Period CDF", xlab="Days", ylab="CDF")
abline(h=0)
axis(side = 1, at = c(218,234,250,266,282,298,314), 
  labels = TRUE) 
  
  
#### 3.7. Standard Normal Distribution

x <- seq(-4,4,0.1)
pdf <- dnorm(x, mean = 0, sd = 1)

plot(x, pdf, type="l", col="red", xlim=c(-3,3))

cdf <- pnorm(x, mean = 0, sd = 1)
plot(x, cdf, type="l", col="red", xlim=c(-3,3))

100*(pnorm(c(1,2,3)) - pnorm(c(-1,-2,-3)))

#### 3.8. Normal Quantiles

qnorm(0.5, mean=0, sd=1)

qnorm(0.95, mean=80, sd=5)

qnorm(0.99, mean=80, sd=5)

### 3.9. Generating Random Numbers with Normal Distribution

y <- rnorm(20, mean = 80, sd = 5)

y <- rnorm(1000, mean = 80, sd = 5)
y <- round(y)
table(y)
plot(table(y), type="h")


###########################

#### 3.11. Example—Exponential Distribution

pexp(1/60, rate=20)


x <- seq(0,1, by=1/60)
pdf <- dexp(x, rate=20)

plot(x, pdf, type="l", col="red", 
   xlim=c(0,0.4))
abline(h=0)

cdf <- pexp(x, rate=20)
plot(x, cdf, type="l", col="red", 
   xlim=c(0,0.4))


