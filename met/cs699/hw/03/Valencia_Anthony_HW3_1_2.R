# Problem 1 (20 points)
# Consider the following dataset (sorted in non-decreasing order): 
#   <10, 12, 16, 16, 29, 32, 51, 60, 60, 66, 70, 72, 87, 96, 120>
ds <- c(10, 12, 16, 16, 29, 32, 51, 60, 60, 66, 70, 72, 87, 96, 120)

# (1) Perform the equal width binning on the above data with 3 bins.
#   Note that the bin boundaries are integers in the textbook and in
#   the online lecture module (to make the discussion simple). But,
#   for this assignment your bin boundaries will include fractions.
#   So, you must follow the example in the lecture slides. For each
#   bin, show:
#     - the bin interval
#     - data values in the bin
#     - smoothed values using:
#         - bin means
#         - bin medians
#         - bin boundaries.
A <- min(ds)
B <- max(ds)
N <- 3
W <- (B - A)/N
I1.min <- A
I1.max <- I1.min + W
I2.min <- I1.max
I2.max <- I2.min + W
I3.min <- I2.max
I3.max <- I3.min + W
sprintf("Bin 1 interval: [%f, %f)", I1.min, I1.max)
sprintf("Bin 2 interval: [%f, %f)", I2.min, I2.max)
sprintf("Bin 3 interval: [%f, %f]", I3.min, I3.max)

I1.values <- ds[(ds>=I1.min) & (ds<I1.max)]
I2.values <- ds[(ds>=I2.min) & (ds<I2.max)]
I3.values <- ds[(ds>=I3.min) & (ds<=I3.max)]
print("Bin 1 values:")
print(I1.values)
print("Bin 2 values:")
print(I2.values)
print("Bin 3 values:")
print(I3.values)

print("Bin 1 means:")
print(rep(mean(I1.values), length(I1.values)))
print("Bin 2 means:")
print(rep(mean(I2.values), length(I2.values)))
print("Bin 3 means:")
print(rep(mean(I3.values), length(I3.values)))

print("Bin 1 medians:")
print(rep(median(I1.values), length(I1.values)))
print("Bin 2 medians:")
print(rep(median(I2.values), length(I2.values)))
print("Bin 3 medians:")
print(rep(median(I3.values), length(I3.values)))

I1.fn <- function(arg) {
  rng <- c(min(I1.values), max(I1.values))
  result <- rng[which.min(abs(rng - arg))]
}
I1.boundaries <- unlist(lapply(I1.values, FUN=I1.fn), recursive=FALSE)
I2.fn <- function(arg) {
  rng <- c(min(I2.values), max(I2.values))
  result <- rng[which.min(abs(rng - arg))]
}
I2.boundaries <- unlist(lapply(I2.values, FUN=I2.fn), recursive=FALSE)
I3.fn <- function(arg) {
  rng <- c(min(I3.values), max(I3.values))
  result <- rng[which.min(abs(rng - arg))]
}
I3.boundaries <- unlist(lapply(I3.values, FUN=I3.fn), recursive=FALSE)
print("Bin 1 boundaries:")
print(I1.boundaries)
print("Bin 2 boundaries:")
print(I2.boundaries)
print("Bin 3 boundaries:")
print(I3.boundaries)

# (2) Repeat the same with equal depth binning with 3 bins.
ds.mtx <- matrix(ds, ncol=3)
I1.values <- ds.mtx[,1]
I2.values <- ds.mtx[,2]
I3.values <- ds.mtx[,3]
print("Bin 1 values:")
print(I1.values)
print("Bin 2 values:")
print(I2.values)
print("Bin 3 values:")
print(I3.values)

print("Bin 1 means:")
print(rep(mean(I1.values), length(I1.values)))
print("Bin 2 means:")
print(rep(mean(I2.values), length(I2.values)))
print("Bin 3 means:")
print(rep(mean(I3.values), length(I3.values)))

print("Bin 1 medians:")
print(rep(median(I1.values), length(I1.values)))
print("Bin 2 medians:")
print(rep(median(I2.values), length(I2.values)))
print("Bin 3 medians:")
print(rep(median(I3.values), length(I3.values)))

I1.fn <- function(arg) {
  rng <- c(min(I1.values), max(I1.values))
  result <- rng[which.min(abs(rng - arg))]
}
I1.boundaries <- unlist(lapply(I1.values, FUN=I1.fn), recursive=FALSE)
I2.fn <- function(arg) {
  rng <- c(min(I2.values), max(I2.values))
  result <- rng[which.min(abs(rng - arg))]
}
I2.boundaries <- unlist(lapply(I2.values, FUN=I2.fn), recursive=FALSE)
I3.fn <- function(arg) {
  rng <- c(min(I3.values), max(I3.values))
  result <- rng[which.min(abs(rng - arg))]
}
I3.boundaries <- unlist(lapply(I3.values, FUN=I3.fn), recursive=FALSE)
print("Bin 1 boundaries:")
print(I1.boundaries)
print("Bin 2 boundaries:")
print(I2.boundaries)
print("Bin 3 boundaries:")
print(I3.boundaries)

# (3) If you transform the dataset into the interval of [0, 1] using
#   Min-max normalization, what is the new value of 51?
V <- 51
new_min <- 0
new_max <- 1
V.min.max.norm <- ((V - min(ds))/(max(ds) - min(ds)))*(new_max - new_min) + new_min
print(V.min.max.norm)

# (4) If you transform the dataset using z-score normalization using
#   the standard deviation, what is the new value of 51?
mu <- mean(ds)
omega <- sd(ds)
V.z.score.norm <- (V - mu)/omega
print(V.z.score.norm)

# (5) If you transform the dataset using z-score normalization using
#   the mean absolute deviation, what is the new value of 51?
V.z.score.norm.abs <- abs((V - mu)/omega)
print(V.z.score.norm.abs)



# Problem 2 (10 points)
# This problem is a practice of calculating correlations between input
#   attributes (or predictive attributes) and the output attribute
#   (or predictable attribute) in the a3-p2.csv dataset. This dataset
#   has 5 attributes and 100 tuples. The first 4 attributes are input
#   attributes and the last attribute, A5, is the output attribute. Your
#   task is to calculate the correlation between each input attribute and
#   the output attribute. In other words, you are required to calculate the
#   following four correlations:
#
#   correl(A1, A5)
#   correl(A2, A5)
#   correl(A3, A5)
#   correl(A4, A5)
#
#   Here, correl(X, Y) denotes the correlation between X and Y.
#   In your submission, include all four correlations, and indicate the
#   attribute that has the strongest correlation with A5.
dat = read.csv("a3-p2.csv", header = TRUE)
A1.cor <- cor(dat$A1, dat$A5)
A2.cor <- cor(dat$A2, dat$A5)
A3.cor <- cor(dat$A3, dat$A5)
A4.cor <- cor(dat$A4, dat$A5)
print(A1.cor)
print(A2.cor)
print(A3.cor)
print(A4.cor)
# A3 is most strongly correlated

cor.a.b <- function(vec.a, vec.b) {
    a.bar <- mean(vec.a)
    b.bar <- mean(vec.b)
    a.sd <- sd(vec.a)
    b.sd <- sd(vec.b)
    total <- 0
    for(i in 1:length(vec.a)) {
      total <- total + ((vec.a[i] - a.bar) * (vec.b[i] - b.bar))
    }
    result <- total/((length(vec.a)-1) * a.sd * b.sd)
}

A1.cor <- cor.a.b(dat$A1, dat$A5)
A2.cor <- cor.a.b(dat$A2, dat$A5)
A3.cor <- cor.a.b(dat$A3, dat$A5)
A4.cor <- cor.a.b(dat$A4, dat$A5)
print(A1.cor)
print(A2.cor)
print(A3.cor)
print(A4.cor)
# A3 is most strongly correlated
