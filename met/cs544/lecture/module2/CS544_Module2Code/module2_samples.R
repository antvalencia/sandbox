#### 2.1. Probability - Sample Space

if (!is.element("prob", installed.packages()[,"Package"]))
  install.packages("prob", repos="http://cran.us.r-project.org", dependencies = TRUE)

library(prob)

tosscoin(1)

tosscoin(2)

tosscoin(3)

rolldie(1)

rolldie(1, nsides = 4)

#### 2.2. Sampling from an Urn

urnsamples(1:3, size = 2)

urnsamples(c("r","g","b"), size = 2)

urnsamples(1:3, size = 2, 
           replace = TRUE)

urnsamples(c("r","g","b"), size = 2,
           replace = TRUE)

urnsamples(1:3, size = 2, 
           replace = TRUE, ordered = TRUE)

urnsamples(c("r","g","b"), size = 2,
           replace = TRUE, ordered = TRUE)

urnsamples(1:3, size = 2, 
           replace = FALSE, ordered = TRUE)

urnsamples(c("r","g","b"), size = 2, 
           replace = FALSE, ordered = TRUE)

#### 2.3. Counting Tools

nsamp(n = 3, k = 2, replace = FALSE, ordered = FALSE)

nsamp(n = 3, k = 2, replace = TRUE, ordered = FALSE)

nsamp(n = 3, k = 2, replace = FALSE, ordered = TRUE)

nsamp(n = 3, k = 2, replace = TRUE, ordered = TRUE)

#### 2.4. Events

S <- tosscoin(3, makespace = TRUE)

S[2:4, ]

S[seq(1,8, by = 2), ]

S[c(2,4,6,8), ]

subset(S, toss3 == 'H')

subset(S, toss1 == 'H' & toss3 == 'H')

S <- rolldie(2, makespace = TRUE)

nrow(S)

subset(S, X1 == X2)

subset(S, X1 + X2 >= 10)

subset(S, X1 %in% 5:6)

subset(S, X1 %in% 5:6 & X2 %in% c(1,3))


S <- rolldie(3, makespace = TRUE)

nrow(S)

subset(S, isin(S, c(4,5,6), ordered = TRUE))

subset(S, isin(S, c(4,5,6)))

subset(S, isin(S, c(4,6), ordered = TRUE))

S <- cards(makespace = TRUE)

nrow(S)

head(S, n = 2)

subset(S, suit == "Club")
subset(S, suit == "Diamond")
subset(S, suit == "Heart")
subset(S, suit == "Spade")

subset(S, rank %in% 2:4)

subset(S, rank %in% c('K', 'Q'))


S <- cards(makespace = TRUE)
A <- subset(S, suit == "Heart")
B <- subset(S, rank %in% c(10, "Q"))

A

B

union(A, B)

intersect(A, B)

setdiff(A, B)

setdiff(B, A)

setdiff(S, A)

#### 2.5. Setting up the Probabilty Space

outcomes <- rolldie(1)
outcomes

p <- rep(1/6, times = 6)
p

probspace(outcomes, probs = p)

rolldie(1, makespace = TRUE)

p <- c(0.2, 0.15, 0.15, 0.15, 0.15, 0.2)
probspace(outcomes, probs = p)

#### 2.6. The Prob function

S <- cards(makespace = TRUE)

A <- subset(S, rank == "Q")
A

Prob(A)

Prob(S, rank == "Q")

B <- subset(S, suit == "Heart")
B

Prob(B)

Prob(S, suit == "Heart")


#### 3.2. Conditional Probability Example – Rolling Die Twice

S <- rolldie(2, makespace = TRUE)
head(S, n = 2)
tail(S, n = 2)


A <- subset(S, X1 == X2)
A
Prob(A)


B <- subset(S, X1 + X2 == 8)
B
Prob(B)


Prob(A, given = B)
Prob(B, given = A)


S <- rolldie(2, makespace = TRUE)
Prob(S, X1 == X2, given = (X1 + X2 == 8))
Prob(S, X1 + X2 == 8, given = (X1 == X2))

#### 3.3. Conditional Probability Example – Coin Toss Twice

S <- tosscoin(2, makespace = TRUE)
S


A <- subset(S, isin(S, c('H')))
A


B <- subset(S, isin(S, c('H', 'T')))
B


Prob(A, given = B)
Prob(B, given = A)

#### 3.4. Conditional Probability Example - Card Deck

L <- cards()
head(L, n = 4)


M <- urnsamples(L, size = 2)
head(M, n = 3)


length(M)
choose(52, 2)


S <- probspace(M)
Prob(S, all(rank == "A"))


Prob(S, all(suit == "Club"))

#### 3.5. Conditional Probability Example - Red and Blue balls

L <- rep(c("red", "blue"), times = c(3, 2))
L


M <- urnsamples(L, size = 2, ordered = TRUE)
M


S <- probspace(M)
head(S, n = 2)


Prob(S, isrep(S, "red", 2))
Prob(S, isrep(S, "blue", 2))


Prob(S, isin(S, c("red", "blue"), ordered = TRUE))

#### 3.7. Independent Events Example - Coin Toss

S <- tosscoin(5, makespace = TRUE)
B <- subset(S, isrep(S, "T", 5))
B


Prob(B)
1 - Prob(B)

#### 3.8. Independent Events, Repeated Experiments 

iidspace(c('H', 'T'), ntrials = 3, probs = c(0.6, 0.4))

###
iidspace(c('H', 'T'), ntrials = 3)



#### 3.10. Bayes RuleExample

bayes <- function (prior, likelihood) {
  numerators <- prior * likelihood
  return (numerators / sum(numerators)) 
}

prior <- c(0.07, 0.93)
like <- c(0.9, 0.25)

bayes(prior, like)

#### 4.1. Random Variables

S <- rolldie(2, makespace = TRUE)
head(S, n = 2)
tail(S, n = 2)


S <- addrv(S, FUN = sum, name = "U")


head(S, n = 2)
S[10:16,]
tail(S, n = 2)


Prob(S, U <= 6)
Prob(S, U > 6)

#### 4.2. Multiple Random Variables

S <- addrv(S, FUN = max,
           invars = c("X1", "X2"), name = "V")
S <- addrv(S, FUN = min,
           invars = c("X1", "X2"), name = "W")


head(S, n = 2)
S[10:16,]
tail(S, n = 2)

#### 4.3. Random Variables with User-defined functions

countHeads <- function(x) {
  return (sum(x == "H"))
}


S <- tosscoin(3, makespace = TRUE)
S


S <- addrv(S, FUN = countHeads, name = "U")
S


#### 4.4. Marginal Distributions of Random Variables

S <- tosscoin(3, makespace = TRUE)
S <- addrv(S, FUN = countHeads, name = "U")
S


marginal(S, vars = "U")


S <- rolldie(2, makespace = TRUE)
S <- addrv(S, FUN = sum, name = "U")
marginal(S, vars = "U")


S <- rolldie(2, makespace = TRUE)
S <- addrv(S, FUN = sum, name = "U")
S <- addrv(S, FUN = max,
           invars = c("X1", "X2"), name = "V")
S <- addrv(S, FUN = min,
           invars = c("X1", "X2"), name = "W")


marginal(S, vars = c("V", "W"))

#### 5.1. Functions

inc.0 <- function (x) {
  return (x)
}

inc.1 <- function (x) {
	return (x + 1)
}

inc.1(10)

inc.1(c(10,20,30))

inc.2 <- function (x, y) {
	return (x - y)
}

inc.2(10, 20)
inc.2(x = 10, y = 20)
inc.2(y = 20, x = 10)

inc.2(10, y = 20)
inc.2(y = 20, 10)

inc.2(10)

inc.3 <- function (x, y = 100) {
	return (x + y)
}

inc.3(10)
inc.3(10, 20)

inc.4 <- function (x = 5, y = 7) {
	return (x + y)
}

inc.4()
inc.4(2)
inc.4(2, 3)
inc.4(y = 3)

inc.5 <- function (x, y = 100, z = 10) {
	return (x - y - z)	
}

inc.5(1000)
inc.5(1000, 500)
inc.5(1000, 500, 200)

inc.5(1000, z = 200)

inc.5(1000, z = 200, y = 500)

#### 5.2. Scope of Variables

x <- 10

foo <- function() {
  cat("#2 In foo ", x, "\n");
  x <- 20
  cat("#3 In foo ", x, "\n");
  bar()
  cat("#6 In foo ", x, "\n");
  
}

bar <- function() {
  cat("#4 In bar ", x, "\n");
  x <- 30
  cat("#5 In bar ", x, "\n");
}

cat("#1 Before foo ", x, "\n");
foo()
cat("#7 After foo ", x, "\n");

rm(z)

x <- 10
y <- 20

test.1 <- function(a, b) {
  x <- a
  y <- b
  z <- a + b
  return (z)
}

test.1(1, 2)

x
y
z

x <- 20
test.2 <- function(a) {
  z <- a + x
  return (z)
}

test.2(1)


x <- 100
y <- 200

test.3 <- function(a, b) {
  x <<- a
  y <- b
  z <<- x + y
  return (z)
}

test.3(1, 2)

x
y
z

#### 5.3. Control Structures

x <- 10
y <- 20

if (x < y) {
	max <- y
	min <- x
} else {
	max <- x
	min <- y
}

max
min

max <- if (x < y) y else x

max


x <- c(10, 20, 30)

for (i in x) {
	cat("Square of ", i, " = ", i*i, "\n")
}

i

for (i in seq(1,10, by = 2)) {
	cat("Square of ", i, " = ", i*i, "\n")
}

# while
n <- 10
sum <- 0
i <- 1

while (i <= n) {
	sum <- sum + i
	i <- i+1
}
cat("Sum of first ", n, " numbers = ", sum)

limit <- 55
sum <- 0
i <- 0

while (TRUE) {
	i <- i+1
	sum <- sum + i
	if (sum >= limit) break
	}
cat("Sum of first ", i, " numbers = ", sum)

# repeat

i <- 1
repeat {
	cat("Square of ", i, " = ", i*i, "\n")
	i <- i+2
	if (i > 10) break
}



#### 5.4. Reading & Writing Data


x <- scan()


x

x <- scan(sep = ",")

x

x <- scan(what = character())

x

x <- scan(what = logical())

x

x <- scan(what =
       list(age = numeric(), name = character()))

x

# Set the working directory using setwd(...)

dir()

x <- scan("athletedata.txt", what=character())

x

x <- scan("athletedata.txt", skip = 1, 
       what = list(Name = character(), 
                   Salary = numeric(),
                   Endorsements = numeric(), 
                   Sport = character()))

x

as.data.frame(x)



athlete.info <- read.table("athletedata.txt", 
         header = TRUE)

athlete.info

athlete.info <- read.table("athletedata.txt", 
  header = TRUE,
  row.names = c("First", "Second", "Third", 
                "Fourth", "Fifth"))
  
athlete.info

athlete.info <- read.csv("athletedata.csv", 
         header=TRUE)

athlete.info

athlete.info <- read.table(
   "http://kalathur.com/cs544/data/athletedata.txt", 
    header = TRUE)

athlete.info

athlete.info <- read.csv(
   "http://kalathur.com/cs544/data/athletedata.csv", 
    header = TRUE)

athlete.info

write.table(athlete.info, file="test.txt", 
  row.names = FALSE, quote = FALSE)

write.csv(athlete.info, file="test.csv", 
  row.names = FALSE, quote = FALSE)
  
  
  
