#### 2.1. Confidence Intervals with known Population Standard Deviation σ

set.seed(100)

plot.confidence <- function (conf = 95) {
 alpha <- 1 - conf/100
 z <- qnorm(1 - alpha/2)
 print(z, digits=4)
 
 f1 <- curve(dnorm(x), from=-3, to=3, 
         xaxt="n", yaxt="n",
         xlab = "z")
 
 title(paste("Confidence =", conf, "%"))
 
 axis(side=1, at=c(-z, 0, z), las=0,
       labels=formatC(c(-z, 0, z), digits=3))
       
 polygon(f1$x, f1$y, col="lightblue")

 x.1 <- seq(-3, -z, by = 0.05)
 y.1 <- dnorm(x.1)
 x.1 <- c(x.1, -z, -z)
 y.1 <- c(y.1, dnorm(-z), dnorm(-3))

 polygon(x.1, y.1, col="white")

 x.2 <- seq(3, z, by = -0.05)
 y.2 <- dnorm(x.2)

 x.2 <- c(x.2, z, z)
 y.2 <- c(y.2, dnorm(z), dnorm(3))

 polygon(x.2, y.2, col="white")

 # lines(c(0,0), c(dnorm(-3), dnorm(0)), lty=2)

 text(0, 0.2, 1-alpha)
 text(-2.6, 0.2, alpha/2)
 text(2.6, 0.2, alpha/2)

  return (z)
}

plot.confidence(95.44)

#####

## 2.2. Example—95.44% Confidence Interval

options(digits=4)

set.seed(150)

pop.mean <- 60
pop.sd <- 10

x <- rnorm(10000, mean = pop.mean, sd = pop.sd)
x <- as.integer(x)

sample.size <- 30

sd.sample.means <- pop.sd/sqrt(sample.size)
sd.sample.means

sample.data <- sample(x, size=sample.size)
sample.data

xbar <- mean(sample.data)
xbar

cat("95.44% Conf Interval = ",
      xbar - 2*sd.sample.means, "-", 
      xbar + 2*sd.sample.means, "\n")

samples <- 20

xbar2 <- numeric(samples)

for (i in 1: samples) {
	sample.data.1 <- sample(x, size=sample.size)
	xbar2[i] <- mean(sample.data.1)
	str <- sprintf("%2d: xbar = %.2f, CI = %.2f - %.2f",
	        i, xbar2[i], xbar2[i] - 2*sd.sample.means,
	        xbar2[i] + 2*sd.sample.means)
	cat(str,"\n")
}
xbar2
# number outside the range
sum(abs(xbar2-pop.mean) > 2*sd.sample.means)

matplot(rbind(xbar2 - 2*sd.sample.means, xbar2 + 2*sd.sample.means),
        rbind(1:samples, 1:samples), type="l", lty=1)
abline(v = pop.mean)

####
## 2.3. Confidence Interval—z-Interval Procedure

conf <- 90

alpha <- 1 - conf/100
alpha

c(qnorm(alpha/2), qnorm(1 - alpha/2))

#

conf <- c(75, 85, 95, 99)
conf

alpha <- 1 - conf/100
alpha

qnorm(alpha/2)

qnorm(1 - alpha/2)

for (i in alpha) {
  	str <- sprintf("%2d%% Conf Level (alpha = %.2f), z: %.2f , %.2f",
	        100*(1-i), i, 
	        qnorm(i/2),
	        qnorm(1-i/2))
	cat(str,"\n")
}


par(mfrow = c(4,1))
for (i in conf) plot.confidence(i)
par(mfrow = c(1,1))

sample.data

sd.sample.means <- pop.sd/sqrt(sample.size)
sd.sample.means

xbar <- mean(sample.data)
xbar

for (i in alpha) {
  	str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
	        100*(1-i), i, 
	        xbar - qnorm(1-i/2) * sd.sample.means,
	        xbar + qnorm(1-i/2) * sd.sample.means)
	cat(str,"\n")
}

for (i in alpha) {
  	str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
	        100*(1-i), i, 
	        2* qnorm(1-i/2) * sd.sample.means)
	cat(str,"\n")
}

#####

## 2.4. Margin of Error

conf <- 95

alpha <- 1 - conf/100
alpha

pop.sd
sample.size

sd.sample.means <- pop.sd/sqrt(sample.size)
sd.sample.means

qnorm(alpha/2)

qnorm(1 - alpha/2)

error <- qnorm(1 - alpha/2) * sd.sample.means
error

for (error in  c(2.0, 1.0, 0.5, 0.25)) {
  required.size <- (qnorm(1 - alpha/2) * pop.sd/error)^2
  required.size <- round(required.size)
  str <- sprintf("Error = %.2f, Required sample size = %d", 
            error, required.size)
  cat(str, "\n")
}

#####

### 2.5. Confidence Intervals for One Population Mean When Population Standard Deviation Is Not Known
### t curves

x <- seq(-3.2,3.2,0.1)
pdf.1 = dnorm(x)
pdf.2 = dt(x, df = 1)
pdf.3 = dt(x, df = 3)
pdf.4 = dt(x, df = 6)

plot(x, pdf.1, type="l", col="red", xlim=c(-3,3))
lines(x, pdf.2, col="green")
lines(x, pdf.3, col="blue")
lines(x, pdf.4, col="orange")

legend(1.5, 0.4, c("t, df=1", "t, df=3", "t, df=6", "Standard"),
   lty=1, lwd=2, col=c("green", "blue", "orange", "red"))

#

sample.data
n <- length(sample.data)

sd.sample.means <- sd(sample.data)/sqrt(n)
sd.sample.means

xbar <- mean(sample.data)
xbar

conf <- 90

alpha <- 1 - conf/100
alpha


c(qt(alpha/2, df = n-1), 
  qt(1 - alpha/2, df = n-1))

c(xbar - qt(1 - alpha/2, df = n-1) * sd.sample.means,
  xbar + qt(1 - alpha/2, df = n-1) * sd.sample.means)
  
conf <- c(75, 85, 95, 99)
conf

alpha <- 1 - conf/100
alpha

qt(alpha/2, df = n-1)

qt(1 - alpha/2, df = n-1)

for (i in alpha) {
  	str <- sprintf("%2d%% Conf Level (alpha = %.2f), t: %.2f , %.2f",
	        100*(1-i), i, 
	        qt(i/2, df = n-1),
	        qt(1 - i/2, df = n-1))
	cat(str,"\n")
}
  
for (i in alpha) {
  	str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
	        100*(1-i), i, 
	        xbar - qt(1 - i/2, df = n-1) * sd.sample.means,
	        xbar + qt(1 - i/2, df = n-1) * sd.sample.means)
	cat(str,"\n")
}

for (i in alpha) {
  	str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
	        100*(1-i), i, 
	        2* qt(1-i/2, df = n-1) * sd.sample.means)
	cat(str,"\n")
}

#####

## 2.6. Confidence Interval of Population Proportion

library(MASS)

n <- length(survey$Exer)
n

k <- sum(survey$Exer == "Freq")
k

pbar <- k/n
pbar

standard.error <- sqrt(pbar * (1 - pbar) / n)
standard.error

conf <- c(75, 85, 95, 99)
conf

alpha <- 1 - conf/100
alpha

qnorm(alpha/2)

qnorm(1 - alpha/2)

for (i in alpha) {
  	str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
	        100*(1-i), i, 
	        pbar - qnorm(1-i/2) * standard.error,
	        pbar + qnorm(1-i/2) * standard.error)
	cat(str,"\n")
}

## 2.7. Example—Population Proportion-Binomial Distribution

set.seed(120)

n <- 25
outcomes <- rbinom(100, size = n, prob = 0.5)
length(outcomes)
head(outcomes)

pbar <- outcomes/n
head(pbar)

standard.error <- sqrt(pbar * (1 - pbar) / n)
head(standard.error)

conf <- 90

alpha <- 1 - conf/100
alpha

c(qnorm(alpha/2), qnorm(1 - alpha/2))

ci.lower <- pbar - qnorm(1-alpha/2) * standard.error
ci.upper <- pbar + qnorm(1-alpha/2) * standard.error

head(ci.lower)
head(ci.upper)

matplot(rbind(ci.lower, ci.upper),
         rbind(1:100, 1:100),
         type="l", lty=1)
abline(v = 0.5)

#####

plot.confidence.symbolic.z <- function (conf = 85) {
 alpha <- 1 - conf/100
 z <- qnorm(1 - alpha/2)
 print(z, digits=4)
 
 f1 <- curve(dnorm(x), from=-3, to=3, 
         xaxt="n", yaxt="n",
         xlab = "z",
         main = "")
         
 title(expression(paste("Confidence = 100*(1-", alpha, ")%")))
 
 axis(side=1, at=c(-z, 0, z), las=0,
       labels=c(expression(-z[alpha/2], 0, z[alpha/2])))
       
 polygon(f1$x, f1$y, col="lightblue")

 x.1 <- seq(-3, -z, by = 0.05)
 y.1 <- dnorm(x.1)
 x.1 <- c(x.1, -z, -z)
 y.1 <- c(y.1, dnorm(-z), dnorm(-3))

 polygon(x.1, y.1, col="white")

 x.2 <- seq(3, z, by = -0.05)
 y.2 <- dnorm(x.2)

 x.2 <- c(x.2, z, z)
 y.2 <- c(y.2, dnorm(z), dnorm(3))

 polygon(x.2, y.2, col="white")

# lines(c(0,0), c(dnorm(-3), dnorm(0)), lty=2)

 text(0, 0.1, expression(1-alpha))
 text(-2.6, 0.1, expression(alpha/2))
 text(2.6, 0.1, expression(alpha/2))

  return (z)
}

plot.confidence.symbolic.z()

plot.confidence.symbolic.t <- function (conf = 85) {
 alpha <- 1 - conf/100
 t <- qt(1 - alpha/2, df=13)
 print(t, digits=4)
 
 f1 <- curve(dt(x, df=13), from=-3, to=3, 
         xaxt="n", yaxt="n",
         xlab = "t",
         main = "")
         
 title(expression(paste("Confidence = 100*(1-", alpha, ")%")))
 
 axis(side=1, at=c(-t, 0, t), las=0,
       labels=c(expression(-t[alpha/2], 0, t[alpha/2])))
       
 polygon(f1$x, f1$y, col="lightblue")

 x.1 <- seq(-3, -t, by = 0.05)
 y.1 <- dt(x.1, df=13)
 x.1 <- c(x.1, -t, -t)
 y.1 <- c(y.1, dt(-t, df=13), dt(-3, df=13))

 polygon(x.1, y.1, col="white")

 x.2 <- seq(3, t, by = -0.05)
 y.2 <- dt(x.2, df=13)

 x.2 <- c(x.2, t, t)
 y.2 <- c(y.2, dt(t, df=13), dt(3, df=13))

 polygon(x.2, y.2, col="white")

# lines(c(0,0), c(dnorm(-3), dnorm(0)), lty=2)

 text(0, 0.1, expression(1-alpha))
 text(-2.6, 0.1, expression(alpha/2))
 text(2.6, 0.1, expression(alpha/2))

  return (t)
}
plot.confidence.symbolic.t()

####

## 3.2. Critical-Value Approach

set.seed(150)

pop.mean <- 60
pop.sd <- 10

x <- rnorm(10000, mean = pop.mean, sd = pop.sd)
x <- as.integer(x)

sample.size <- 30

sample.data <- sample(x, size=sample.size)
sample.data

xbar <- mean(sample.data)
xbar

mu0 <- pop.mean
sigma <- pop.sd
n <- sample.size

z <- (xbar - mu0) / (sigma / sqrt(n))
z

alpha <- 0.10
c(qnorm(alpha/2), qnorm(1 - alpha/2))

alpha <- 0.05
c(qnorm(alpha/2), qnorm(1 - alpha/2))

alpha <- 0.01
c(qnorm(alpha/2), qnorm(1 - alpha/2))

## 3.3. P-Value Approach

xbar <- mean(sample.data)
xbar

mu0 <- pop.mean
sigma <- pop.sd
n <- sample.size

z <- (xbar - mu0) / (sigma / sqrt(n))
z

2*pnorm(-(abs(z)))

## 3.4. Power of a Hypothesis Test

xbar <- mean(sample.data)
xbar

mu0 <- pop.mean
sigma <- pop.sd
n <- sample.size

z <- (xbar - mu0) / (sigma / sqrt(n))
z

alpha <- 0.05
c(qnorm(alpha/2), qnorm(1 - alpha/2))



##############


plot.hyptest.2t.z <- function (conf = 85) {
  alpha <- 1 - conf/100
  z <- qnorm(1 - alpha/2)
  print(z, digits=4)
  
  f1 <- curve(dnorm(x), from=-3, to=3, 
              xaxt="n", yaxt="n", ylim=c(0, 0.6),
              xlab = "z",
              main = "")
  
  title(expression(paste("Confidence = (1-", alpha, ")")))
  
  axis(side=1, at=c(-z, 0, z), las=0,
       labels=c(expression(-z[alpha/2], 0, z[alpha/2])))
  
  # polygon(f1$x, f1$y, col="lightblue")
  
  x.1 <- seq(-3, -z, by = 0.05)
  y.1 <- dnorm(x.1)
  x.1 <- c(x.1, -z, -z)
  y.1 <- c(y.1, dnorm(-z), dnorm(-3))
  
  polygon(x.1, y.1, col="lightblue")
  
  x.2 <- seq(3, z, by = -0.05)
  y.2 <- dnorm(x.2)
  
  x.2 <- c(x.2, z, z)
  y.2 <- c(y.2, dnorm(z), dnorm(3))
  
  polygon(x.2, y.2, col="lightblue")
  
  lines(c(-z,-z), c(0, 0.55), lty=2)
  lines(c(z, z), c(0, 0.55), lty=2)
  
  text(0, 0.1, expression(1-alpha))
  text(-2.6, 0.1, expression(alpha/2))
  text(2.6, 0.1, expression(alpha/2))
  
  text(0, 0.5, expression(paste("Do not reject ", H[0])))
  text(-2.2, 0.5, expression(paste("Reject ", H[0])))
  text(2.2, 0.5, expression(paste("Reject ", H[0])))
  
  
  return (z)
}

plot.hyptest.2t.z()

plot.hyptest.lt.z <- function (conf = 85) {
  alpha <- 1 - conf/100
  z <- qnorm(1 - alpha)
  print(z, digits=4)
  
  f1 <- curve(dnorm(x), from=-3, to=3, 
              xaxt="n", yaxt="n", ylim=c(0, 0.6),
              xlab = "z",
              main = "")
  
  title(expression(paste("Confidence = (1-", alpha, ")")))
  
  axis(side=1, at=c(-z, 0), las=0,
       labels=c(expression(-z[alpha], 0)))
  
  # polygon(f1$x, f1$y, col="lightblue")
  
  x.1 <- seq(-3, -z, by = 0.05)
  y.1 <- dnorm(x.1)
  x.1 <- c(x.1, -z, -z)
  y.1 <- c(y.1, dnorm(-z), dnorm(-3))
  
  polygon(x.1, y.1, col="lightblue")
  
  
  lines(c(-z,-z), c(0, 0.55), lty=2)
  
  text(0, 0.1, expression(1-alpha))
  text(-2.4, 0.1, expression(alpha))
  
  text(0.2, 0.5, expression(paste("Do not reject ", H[0])))
  text(-2.2, 0.5, expression(paste("Reject ", H[0])))
  
  
  return (z)
}

plot.hyptest.lt.z()


plot.hyptest.rt.z <- function (conf = 85) {
  alpha <- 1 - conf/100
  z <- qnorm(1 - alpha)
  print(z, digits=4)
  
  f1 <- curve(dnorm(x), from=-3, to=3, 
              xaxt="n", yaxt="n", ylim=c(0, 0.6),
              xlab = "z",
              main = "")
  
  title(expression(paste("Confidence = (1-", alpha, ")")))
  
  axis(side=1, at=c(0, z), las=0,
       labels=c(0, expression(z[alpha])))
  
  # polygon(f1$x, f1$y, col="lightblue")
  
  x.2 <- seq(3, z, by = -0.05)
  y.2 <- dnorm(x.2)
  
  x.2 <- c(x.2, z, z)
  y.2 <- c(y.2, dnorm(z), dnorm(3))
  
  polygon(x.2, y.2, col="lightblue")
  
  lines(c(z,z), c(0, 0.55), lty=2)
  
  text(0, 0.1, expression(1-alpha))
  text(2.2, 0.15, expression(alpha))
  
  text(-1, 0.5, expression(paste("Do not reject ", H[0])))
  text(2.2, 0.5, expression(paste("Reject ", H[0])))
  
  
  return (z)
}

plot.hyptest.rt.z()

#####


plot.hyptest.2t.p <- function (conf = 85) {
  alpha <- 1 - conf/100
  z <- qnorm(1 - alpha/2)
  print(z, digits=4)
  
  f1 <- curve(dnorm(x), from=-3, to=3, 
              xaxt="n", yaxt="n", ylim=c(0, 0.6),
              xlab = "z",
              main = "")
  
  # title(expression(paste("Confidence = (1-", alpha, ")")))
  
  axis(side=1, at=c(-z, 0, z), las=0,
       labels=c(expression(-abs(z[0]), 0, abs(z[0]))))
  
  # polygon(f1$x, f1$y, col="lightblue")
  
  x.1 <- seq(-3, -z, by = 0.05)
  y.1 <- dnorm(x.1)
  x.1 <- c(x.1, -z, -z)
  y.1 <- c(y.1, dnorm(-z), dnorm(-3))
  
  polygon(x.1, y.1, col="lightblue")
  
  x.2 <- seq(3, z, by = -0.05)
  y.2 <- dnorm(x.2)
  
  x.2 <- c(x.2, z, z)
  y.2 <- c(y.2, dnorm(z), dnorm(3))
  
  polygon(x.2, y.2, col="lightblue")
  
  lines(c(-z,-z), c(0, dnorm(-z)), lty=2)
  lines(c(z, z), c(0, dnorm(z)), lty=2)
  
  text(0, 0.5, "P-value")
  
  
  return (z)
}
plot.hyptest.2t.p()


plot.hyptest.lt.p <- function (conf = 85) {
  alpha <- 1 - conf/100
  z <- qnorm(1 - alpha)
  print(z, digits=4)
  
  f1 <- curve(dnorm(x), from=-3, to=3, 
              xaxt="n", yaxt="n", ylim=c(0, 0.6),
              xlab = "z",
              main = "")
  
  # title(expression(paste("Confidence = (1-", alpha, ")")))
  
  axis(side=1, at=c(-z, 0), las=0,
       labels=c(expression(-z[0], 0)))
  
  # polygon(f1$x, f1$y, col="lightblue")
  
  x.1 <- seq(-3, -z, by = 0.05)
  y.1 <- dnorm(x.1)
  x.1 <- c(x.1, -z, -z)
  y.1 <- c(y.1, dnorm(-z), dnorm(-3))
  
  polygon(x.1, y.1, col="lightblue")
  
  
  lines(c(-z,-z), c(0, dnorm(-z)), lty=2)
  
  text(-2.2, 0.2, "P-value")
  
  
  return (z)
}

plot.hyptest.lt.p()

plot.hyptest.rt.p <- function (conf = 85) {
  alpha <- 1 - conf/100
  z <- qnorm(1 - alpha)
  print(z, digits=4)
  
  f1 <- curve(dnorm(x), from=-3, to=3, 
              xaxt="n", yaxt="n", ylim=c(0, 0.6),
              xlab = "z",
              main = "")
  
  # title(expression(paste("Confidence = (1-", alpha, ")")))
  
  axis(side=1, at=c(0, z), las=0,
       labels=c(0, expression(z[0])))
  
  # polygon(f1$x, f1$y, col="lightblue")
  
  x.2 <- seq(3, z, by = -0.05)
  y.2 <- dnorm(x.2)
  
  x.2 <- c(x.2, z, z)
  y.2 <- c(y.2, dnorm(z), dnorm(3))
  
  polygon(x.2, y.2, col="lightblue")
  
  lines(c(z,z), c(0, dnorm(z)), lty=2)
  
  text(2.3, 0.15, "P-value")
  
  
  return (z)
}
plot.hyptest.rt.p()


####

## 4.2. The replicate Function

set.seed(100)

x <- 1:5
sample(x, replace = TRUE)
sample(x, replace = TRUE)

replicate(10, sample(x, replace = TRUE))

y <- replicate(10, sample(x, replace = TRUE),
               simplify = FALSE)
y

sapply(y, mean, simplify = TRUE)


####

## 4.3. Bootstrap Distribution—Standard Error of the Mean

set.seed(120)

pop.mean <- 60
pop.sd <- 10

sample.size <- 30

x <- rnorm(sample.size, mean = pop.mean, sd = pop.sd)
x <- as.integer(x)
x

resamples.1 <- replicate(1000, 
                         sample(x, replace = TRUE),
                         simplify = FALSE)

head(resamples.1, n = 2)

xbar.star <- sapply(resamples.1, mean, simplify = TRUE)

length(xbar.star)

head(xbar.star, n = 6)

hist(xbar.star, breaks=40, prob = TRUE, 
     xlim=c(50,70), ylim=c(0, 0.3))

sd.sample.means <- pop.sd/sqrt(sample.size)
sd.sample.means

curve(dnorm(x, mean = pop.mean, sd = sd.sample.means), 
      from = 50, to = 70, add = TRUE)

mean(xbar.star)

mean(x)

mean(xbar.star) - mean(x)

sd(xbar.star)

sd.sample.means

#

library(boot)

set.seed(120)

samplemean <- function(x, indices) { 
  return (mean(x[indices]))
}
boot(data = x, statistic = samplemean, R = 1000)

#

## 4.4. Bootstrap Distribution—Standard Error of the Median

set.seed(120)

length(rivers)

hist(rivers, breaks = 20)

mean(rivers)

median(rivers)


resamples.2 <- replicate(1000, 
                         sample(rivers, replace = TRUE),
                         simplify = FALSE)

medians.star <- sapply(resamples.2, median, 
                       simplify = TRUE)

length(medians.star)

hist(medians.star, breaks=30, prob = TRUE)

median(rivers)

mean(medians.star)

mean(medians.star) - median(rivers)

sd(medians.star)

set.seed(120)

samplemedian <- function(x, indices) { 
  return (median(x[indices]))
}


boot(data = rivers, 
     statistic = samplemedian, R = 1000)


## 4.5. Bootstrap Confidence Intervals


quantile(medians.star, c(0.025, 0.975))

set.seed(120)

samplemedian <- function(x, indices) { 
  return (median(x[indices]))
}

boot.data <- boot(data = rivers, 
                  statistic = samplemedian, R = 1000)

boot.ci(boot.data, conf = 0.95, type="perc")




