library(sampling)

# Q1
#Initialize the House and Senate data as shown below:
#  house <- read.csv("http://kalathur.com/house.csv", stringsAsFactors = FALSE)
#Calculate the number of states represented and store this into a variable, sample.size
house <- read.csv("http://kalathur.com/house.csv", stringsAsFactors = FALSE)
house
sample.size <- length(unique(house$State))


# Q2
#Use SRSWR and starting seed 123, draw the sample of the sample.size.
#Examine the sample. How many states were represented in this sample.
#Save this sample as sample.1
set.seed(123)
s <- srswr(sample.size, nrow(house))
s[s!=0]
rows <- (1:nrow(house))[s!=0]
rows <- rep(rows, s[s != 0])
rows
sample.1 <- house[rows, ]
sample.1
table(sample.1$State)
length(unique(sample.1$State))


# Q3
#Use SRSWOR and starting seed 123, draw the sample of the sample.size.
#Examine the sample. How many states were represented in this sample.
#Save this sample as sample.2
set.seed(123)
s <- srswor(sample.size, nrow(house))
s[s != 0]
rows <- (1:nrow(house))[s!=0]
sample.2 <- house[rows, ]
sample.2
table(sample.2$State)
length(unique(sample.2$State))


# Q4
#Use Systematic Sampling and starting seed 123, draw the sample of the sample.size.
#Examine the sample. How many states were represented in this sample.
#Save this sample as sample.3.
#Fix the last item picked as the last row in the data if it is outside the range.

# Systematic Sampling
set.seed(123)

# items in each group
k <- ceiling(nrow(house) / sample.size)
k

# random item from first group
r <- sample(k, 1)
r

# select every kth item
s <- seq(r, by = k, length = sample.size)
s

if (s[sample.size] > nrow(house))
  s[sample.size] <- nrow(house)
sample.3 <- house[s, ]
sample.3
table(sample.3$State)
length(unique(sample.3$State))

# Q5
#Use Systematic Sampling and Unequal Probabilities with Years In Office
#and starting seed 123, draw the sample of the sample.size.
#Examine the sample. How many states were represented in this sample.
#Save this sample as sample.4.

# Unequal Probabilities
set.seed(123)
pik <- inclusionprobabilities(
  house$Years_in_office, sample.size)
length(pik)
sum(pik)
s <- UPsystematic(pik)
sample.4 <- house[s != 0, ]
sample.4
table(sample.4$State)
length(unique(sample.4$State))


# Q6
#Use Stratified Sampling with the State as the Strata
#and starting seed 123, draw the sample of the sample.size.
#Examine the sample. How many states were represented in this sample.
#Save this sample as sample.5.

# Stratified Sampling
set.seed(123)
order.index <- order(house$State)
data <- house[order.index, ]
st <- strata(data, stratanames = c("State"),
             size = rep(1, sample.size) , 
             method = "srswor")
sample.5 <- getdata(data, st)
table(sample.5$State)
length(unique(sample.5$State))

# Q7
#Use Clustering with State as the group
#and starting seed 123, draw the sample of  size 5.
#Examine the sample. How many states were represented in this sample.
#What is the size of this sample?
#  Save this sample as sample.6.

set.seed(123)
cl <- cluster(house, c("State"), 
              size = 5, method="srswor")
sample.6 <- getdata(house, cl)
table(sample.6$State)
table(sample.6$State) -> x
x[x!=0]
sum(x)


# Q8
#A placeholder - End of Lab	  						 
