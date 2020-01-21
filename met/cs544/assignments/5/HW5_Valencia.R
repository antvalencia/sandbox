# Part1) Central Limit Theorem (20 points)
library(prob)
options(digits=4)
# The input data consists of the sequence from 1 to 20 (1:20). Show the following three plots in a single row.
s <- 1:20
# a) Show the histogram of the densities of this distribution.
size <- 1
samples <- combn(s, size)
samples
xbar <- apply(samples, size, FUN = mean) 
xbar
hist(xbar, prob = TRUE)

# b) Using all samples of this data of size 2, show the histogram of the densities of the sample means.
sz <- 2
samples_2 <- urnsamples(s, sz)
samples_2
xbar_2 <- (samples_2$X1 + samples_2$X2)/sz
xbar_2
hist(xbar_2, prob = TRUE)

# samples <- combn(s, sz)
# samples
# xbar <- apply(samples, sz, FUN = mean) 
# xbar
# hist(xbar, prob = TRUE)
# c) Using all samples of this data of size 5, show the histogram of the densities of the sample means.
sz <- 5
samples_5 <- urnsamples(s, sz)
samples_5
xbar_5 <- (samples_5$X1 + samples_5$X2 + samples_5$X3 + samples_5$X4 + samples_5$X5)/sz
xbar_5
hist(xbar_5, prob = TRUE)

# samples <- combn(s, sz)
# samples
# xbar <- apply(samples, sz, FUN = mean) 
# xbar
# hist(xbar, prob = TRUE)
# d) Compare of means and standard deviations of the above three distributions.
mean(xbar)
sd(xbar)

mean(xbar_2)
sd(xbar_2)

mean(xbar_5)
sd(xbar_5)

# Part2) Central Limit Theorem (20 points)
# The data in the file queries.csv contains the number of queries Google has had each day for a one year period
# (365 days). The data file is also available at http://kalathur.com/cs544/data/queries.csv. Use this link to
# read the data using read.csv function when submitting the homework.
queries <- read.csv("http://kalathur.com/cs544/data/queries.csv", stringsAsFactors = FALSE)
help(read.csv)
qs <- queries$queries
qs
# a) Show the histogram of the distribution of the number of queries. Compute the mean and standard deviation
# of the number of queries Google has had per day.
hist(qs, xlim = c(min(qs), max(qs)), breaks = 25)

# b) Draw 1000 samples of this data of size 5, show the histogram of the densities of the sample means. Compute
# the mean of the sample means and the standard deviation of the sample means.
samples <- 1000
sample.size <- 5
xbar_b <- numeric(samples)
for (i in 1:samples) {
  xbar_b[i] <- mean(sample(qs, size = sample.size, replace = TRUE))
}
hist(qs, xlim = c(min(qs), max(qs)), breaks = 25)
mean(xbar)
sd(xbar)

# c) Draw 1000 samples of this data of size 20, show the histogram of the densities of the sample means. Compute
# the mean of the sample means and the standard deviation of the sample means.
samples <- 1000
sample.size <- 20
xbar <- numeric(samples)
for (i in 1:samples) {
  xbar[i] <- mean(sample(qs, size = sample.size, replace = TRUE))
}
hist(qs, xlim = c(min(qs), max(qs)), breaks = 25)
mean(xbar)
sd(xbar)

# d) Compare of means and standard deviations of the above three distributions.

# Part3) Central Limit Theorem – Negative Binomial distribution (20 points)
# Suppose the input data follows the negative binomial distribution with the parameters size = 5 and prob = 0.5.
r <- 5
p <- 0.5
# a) Generate 1000 random numbers from this distribution. Show the barplot with the proportions of the distinct
# values of this distribution.
z <- 1000
samples <- rnbinom(z, size = r, prob = p)
hist(samples, prob = TRUE, breaks = min(samples):max(samples), xlim = c(min(samples),max(samples)))
mean(samples)
sd(samples)

# b) With samples sizes of 10, 20, 30, and 40, generate the data for 5000 samples using the same distribution.
# Show the histograms of the densities of the sample means. Use a 2 x 2 layout.
z <- 5000

par(mfrow = c(2,2))
help(rnbinom)
for (r in c(10, 20, 30, 40)) {
  samples <- rnbinom(z, size = r, prob = p)
  hist(samples, prob = TRUE, main = paste("Sample Size =", r))
  cat("Sample Size = ", r, " Mean = ", mean(samples), " SD = ", sd(samples), "\n")
}

par(mfrow = c(1,1))

# c) Compare of means and standard deviations of the data from a) with the four sequences generated in b).
# from above:
# a) mean: 40; sd: 9.1
# b) size: 10; mean: 10; sd: 4.4
#    size: 20; mean: 20; sd: 6.3
#    size: 30; mean: 30; sd: 7.8
#    size: 40; mean: 40; sd: 8.9

# Part4) Sampling (40 points)
# Use the MU284 dataset from the sampling package. Use a sample size of 20 for each of the following.
library(sampling)
data(MU284)
MU284$REG
N <- nrow(MU284)
n <- 20
# a) Show the sample drawn using simple random sampling without replacement.
set.seed(123)
sample_simple <- srswor(n, N)
# Show the frequencies for each region (REG).
sample_simple_reg <- rep(MU284$REG[sample_simple != 0], sample_simple[sample_simple != 0])
hist(sample_simple_reg,
     main = "freq of MU284 REG sample (srswor)",
     xlab = "REG")
# Show the percentages of these with respect to the entire dataset.
t <- table(sample_simple_reg)
setNames(as.vector(t)/N*100, as.numeric(names(t)))

# b) Show the sample drawn using systematic sampling.
set.seed(113)
N
k <- floor(N / n)
k
r <- sample(k, 1)
r
# select every kth item
sq <- seq(r, by = k, length = n)
sq
sample_ss <- MU284[sq, ]
# Show the frequencies for each region (REG).
hist(sample_ss$REG, main = "freq of MU284 REG sample (systematic)", xlab = "REG")
# Show the percentages of these with respect to the entire dataset.
t <- table(sample_ss$REG)
setNames(as.vector(t)/N*100, as.numeric(names(t)))

# c) Calculate the inclusion probabilities using the S82 variable.
# Using these values, show the sample drawn using systematic sampling.
set.seed(103)
pik <- inclusionprobabilities(MU284$REG, n)
length(pik)
sum(pik)
s <- UPsystematic(pik)
sample_ip <- MU284[s != 0, ]
sample_ip
# Show the frequencies for each region (REG).
hist(sample_ip$REG, main = "freq of MU284 REG sample (incl. prob.)", xlab = "REG")
# Show the percentages of these with respect to the entire dataset.
t <- table(sample_ip$REG)
setNames(as.vector(t)/N*100, as.numeric(names(t)))

# d) Order the data using the REG variable.
# Draw a stratified sample using proportional sizes based on the REG variable.
set.seed(93)
freq <- table(MU284$REG)
freq
sizes <- round(n * freq / sum(freq))
sizes
sum(sizes)
order.index <- order(MU284$REG)
data <- MU284[order.index, ]
st <- strata(data, stratanames = c("REG"),
             size = sizes, method = "srswor")
sample_or <- getdata(data, st)
# Show the frequencies for each region (REG).
hist(sample_or$REG, main = "freq of MU284 REG sample (stratified w/prop.)", xlab = "REG")
# Show the percentages of these with respect to the entire dataset.
t <- table(sample_or$REG)
setNames(as.vector(t)/N*100, as.numeric(names(t)))

# e) Compare the means of RMT85 variable for these four samples with the entire data.
sample_simple_rmt85 <- rep(MU284$RMT85[sample_simple != 0], sample_simple[sample_simple != 0])
mean(sample_simple_rmt85)
mean(sample_ss$RMT85)
mean(sample_ip$RMT85)
mean(sample_or$RMT85)
