#### 2. Central Limit Theorem

#### 2.1. Introduction

library(prob)
options(digits=4)

x <- c(69,70,72,75,79)
samples <- urnsamples(x, 2)
samples

# means of samples
xbar <- (samples$X1 + samples$X2)/2
xbar

hist(xbar, prob = TRUE)

# Alternative approach (no need for prob library)

x <- c(69,70,72,75,79)
samples <- combn(x,2)
samples

xbar <- apply(samples, 2, FUN = mean) 
xbar

hist(xbar, prob = TRUE)


#### 2.2. Data from Normal Distribution

set.seed(100)

x <- rnorm(1000, mean = 60, sd = 10)

hist(x, prob = TRUE, 
  xlim=c(30,90), ylim = c(0, 0.05))

curve(dnorm(x, mean = 60, sd = 10), add = TRUE, col = "red")

options(digits=2)
mean(x)
sd(x)


samples <- 10000
sample.size <- 5

xbar <- numeric(samples)

for (i in 1: samples) {
	xbar[i] <- mean(rnorm(sample.size, mean = 60, sd = 10))
}

hist(xbar, prob = TRUE, 
     breaks = 15, xlim=c(30,90), 
     ylim = c(0, 0.1))

mean(xbar)
sd(xbar)

par(mfrow = c(2,2))

for (size in c(10, 20, 30, 40)) {
	for (i in 1:samples) {
	  xbar[i] <- mean(rnorm(size, mean = 60, sd = 10))
  }

   hist(xbar, prob = TRUE, 
     breaks = 15, xlim=c(50,70), ylim = c(0, 0.3),
     main = paste("Sample Size =", size))
     
   cat("Sample Size = ", size, " Mean = ", mean(xbar),
        " SD = ", sd(xbar), "\n")
}

par(mfrow = c(1,1))

10 / sqrt(c(10,20,30,40))

#### 2.3. Data from Exponential Distribution

set.seed(100)

curve(dexp(x, rate = 2), 0, 5, col = "red")

x <- rexp(1000, rate = 2)

hist(x, prob = TRUE, 
  breaks = 15, ylim = c(0,2), 
  xlim = c(0,5))

options(digits=2)
mean(x)
sd(x)


samples <- 10000
sample.size <- 5

xbar <- numeric(samples)

for (i in 1:samples) {
	xbar[i] <- mean(rexp(sample.size, rate = 2))
}

hist(xbar, prob = TRUE, 
     breaks = 15, xlim=c(0,3), 
     ylim = c(0, 2),
     main = "Sample Size = 5")

mean(xbar)
sd(xbar)

par(mfrow = c(2,3))

 for (size in c(10, 20, 30, 40, 50, 60)) {
	for (i in 1:samples) {
	  xbar[i] <- mean(rexp(size, rate = 2))
    }

    hist(xbar, prob = TRUE, 
     breaks = 15, xlim=c(0,2), ylim = c(0, 5),
     main = paste("Sample Size =", size))
     
    cat("Sample Size = ", size, " Mean = ", mean(xbar),
        " SD = ", sd(xbar), "\n")
 }

 xpar(mfrow = c(1,1))

0.5 / sqrt(c(10,20,30,40,50,60))

#### 2.4. Data from Discrete Uniform Distribution

set.seed(150)

x <- 1:6

x.sample <- sample(x, size = 1000, 
              replace = TRUE)

table(x.sample)

prop.table(table(x.sample))

barplot(prop.table(table(x.sample)),
  xlab = "x", ylab = "Proportion")
  

mean(x.sample)
sd(x.sample)

samples <- 10000
sample.size <- 5

xbar <- numeric(samples)

for (i in 1:samples) {
	xbar[i] <- mean(sample(x, size = sample.size, 
              replace = TRUE))
}

hist(xbar, prob = TRUE, 
     breaks = 15, xlim=c(0,6), 
     ylim = c(0, 0.6),
     main = "Sample Size = 5")

mean(xbar)
sd(xbar)

par(mfrow = c(2,2))

 for (size in c(10, 20, 30, 40)) {
	for (i in 1:samples) {
	  xbar[i] <- mean(sample(x, size = size, 
              replace = TRUE))

    }

    hist(xbar, prob = TRUE, 
     breaks = 15, xlim=c(0,6), ylim = c(0, 1.5),
     main = paste("Sample Size =", size))
     
    cat("Sample Size = ", size, " Mean = ", mean(xbar),
        " SD = ", sd(xbar), "\n")
 }

par(mfrow = c(1,1))

1.7 / sqrt(c(10,20,30,40))

####################################

#### 3. Sampling Methods

#### 3.2. Simple Random Sampling

library(sampling)


# SRSWR
# Equal Probability

set.seed(123)

s <- srswr(10, 26)
s

LETTERS[s != 0]

s[s != 0]

rep(LETTERS[s != 0], s[s != 0])

# SRSWOR
# Equal Probability

s <- srswor(10, 26)
s

LETTERS[s != 0]

s[s != 0]

#### 3.3. Example – Simple Random Sampling

data(swissmunicipalities)
names(swissmunicipalities)

head(swissmunicipalities[c(2,4,14,17,22)])

table(swissmunicipalities$REG)

# srswr
set.seed(153)

s <- srswr(70, nrow(swissmunicipalities))
s[s != 0]

rows <- (1:nrow(swissmunicipalities))[s!=0]
rows <- rep(rows, s[s != 0])
rows

sample.1 <- swissmunicipalities[rows, ]
head(sample.1[c(2,4,14,17,22)])

table(sample.1$REG)

set.seed(153)
# srswor

s <- srswor(70, nrow(swissmunicipalities))

sample.2 <- swissmunicipalities[s != 0, ]
head(sample.2[c(2,4,14,17,22)])

table(sample.2$REG)


#### 3.4. Systematic Sampling

set.seed(113)
#
N <- 1000
n <- 50

# items in each group
k <- ceiling(N / n)
k

# random item from first group
r <- sample(k, 1)
r

# select every kth item

seq(r, by = k, length = n)

#### 3.5. Example – Systematic Sampling

set.seed(113)
#

N <- nrow(swissmunicipalities)
n <- 70

k <- ceiling(N / n)
k

r <- sample(k, 1)
r

# select every kth item

s <- seq(r, by = k, length = n)

sample.3 <- swissmunicipalities[s, ]
head(sample.3[c(2,4,14,17,22)])

table(sample.3$REG)

#### 3.6. Unequal Probabilities

set.seed(113)
# UPsystematic

pik <- inclusionprobabilities(swissmunicipalities$POPTOT, 70)
length(pik)

sum(pik)

s <- UPsystematic(pik)

sample.4 <- swissmunicipalities[s != 0, ]
head(sample.4[c(2,4,14,17,22)])

table(sample.4$REG)

#### 3.7. Stratified Sampling

set.seed(123)
# Stratified, equal sized strata

section.ids <- rep(LETTERS[1:4], each = 25)

section.scores <- round(runif(100, 60, 80))

data <- data.frame(
  Section = section.ids, 
  Score = section.scores)

head(data)

table(data$Section)

st.1 <- strata(data, stratanames = c("Section"),
               size = rep(3, 4), method = "srswor",
               description = TRUE)

st.1

st.sample1 <- getdata(data, st.1)

st.sample1


#### 3.8. Example – Unequal Strata

set.seed(123)
# Stratified, unequal sized strata

section.ids <- rep(LETTERS[1:4], c(10, 20, 30, 40))

section.scores <- round(runif(100, 60, 80))

data <- data.frame(
  Section = section.ids, 
  Score = section.scores)

head(data)

freq <- table(data$Section)
freq

st.sizes <- 20 * freq / sum(freq)
st.sizes

st.2 <- strata(data, stratanames = c("Section"),
               size = st.sizes, method = "srswor",
               description = TRUE)

st.2

st.sample2 <- getdata(data, st.2)

st.sample2

strata(data, stratanames = c("Section"),
       size = rep(5, 4), method = "srswor",
       description = TRUE)

#### 3.9. Example – Strata with Two Variables

# two variables

set.seed(123)
# Stratified, unequal sized strata

section.ids <- rep(LETTERS[1:4], c(10, 20, 30, 40))
section.genders <- 
  rep(rep(c("F", "M"), 4), 
      c(10, 0, 5, 15, 20, 10, 15, 25))
section.scores <- round(runif(100, 60, 80))

data <- data.frame(
  Section = section.ids, 
  Gender = section.genders,
  Score = section.scores)

head(data)

data <- data[order(data$Section, data$Gender), ]

freq <- table(data$Section, data$Gender)
freq

st.sizes <- 20 * freq / sum(freq)
st.sizes

as.vector(st.sizes)

as.vector(t(st.sizes))

st.sizes <- as.vector(t(st.sizes))
st.sizes <- st.sizes[st.sizes != 0]

st.sizes

st.3 <- strata(data, 
               stratanames = c("Section", "Gender"),
               size = st.sizes, method = "srswor",
               description = TRUE)

st.3

st.sample3 <- getdata(data, st.3)

st.sample3

#### 3.10. Ordering Data   

set.seed(113)
#
order.index <- order(swissmunicipalities$REG)
data <- swissmunicipalities[order.index, ]

head(data[c(2,4,14,17,22)])

st <- strata(data, stratanames = c("REG"),
             size = c(14,22,8,4,11,5,6) , 
             method = "srswor")

sample.5 <- getdata(data, st)
table(sample.5$REG)

# Proportion

freq <- table(swissmunicipalities$REG)
freq

sizes <- round(70 * freq / sum(freq))
sizes
sum(sizes)

st <- strata(data, stratanames = c("REG"),
             size = sizes, method = "srswor")

head(st)

sample <- getdata(data, st)
head(sample[1:5])

####

# Cluster

set.seed(123)

section.ids <- rep(LETTERS[1:4], c(10, 20, 30, 40))

section.scores <- round(runif(100, 60, 80))

data <- data.frame(
  Section = section.ids, 
  Score = section.scores)

table(data$Section)

cl <- cluster(data, c("Section"), size = 2, 
              method="srswor")

cl.sample <- getdata(data, cl)

table(cl.sample$Section)

#
set.seed(113)

table(swissmunicipalities$REG)

cl <- cluster(swissmunicipalities, c("REG"), 
              size = 4, method="srswr")

sample.6 <- getdata(swissmunicipalities, cl)

table(sample.6$REG)

# writesample
