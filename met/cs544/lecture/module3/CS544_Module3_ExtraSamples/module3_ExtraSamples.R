# Iris Data Set

class(iris)

names(iris)

head(iris)

data <- iris[c(1:4)]

head(data)

summary(data)

pairs(data, pch=16, col="blue")

cor(data)

boxplot(data, col=rainbow(4))

hist(data$Sepal.Length, col="blue")

# Titanic Data Set

class(Titanic)

dim(Titanic)

dimnames(Titanic)

# By Sex
t1 <- margin.table(Titanic, c(2))
t1

mosaicplot(t1, col=c("red", "blue"))

# Sex, Survived
t2 <- margin.table(Titanic, c(2,4))
t2

class(t2)

mosaicplot(t2, col=c("red", "blue"))

m2 <- apply(Titanic, c(2,4), sum)
m2

class(m2)

mosaicplot(m2, col=c("red", "blue"))

# Class, Survived
t3 <- margin.table(Titanic, c(1, 4))
t3

mosaicplot(t3, col=c("red", "blue"))

# Class, Age, Survived
margin.table(Titanic, c(1,3,4))

# apply

help(apply)

# Given the following sales data, where each row represents the sales of the stores and each column represents the weekly sales of that store:
#   data <- c(80, 75, 85, 82, 90, 88, 92, 95, 81, 78, 84, 87)
# sales <- matrix(data, nrow = 3, ncol = 4, byrow = TRUE)
# a) What are the maximum sales for each store? 
# b) What are the maximum sales for each week?

data <- c(80, 75, 85, 82, 
          90, 88, 92, 95,
          81, 78, 84, 87)

sales <- matrix(data,
                nrow = 3, ncol = 4,
                byrow = TRUE)

sales

# By row (maximum sales for each store)
apply(sales, 1, max)

# By column (maximum sales for each week)
apply(sales, 2, max)

# Show for each store which week's sales differ 
# from its median sales by the most. 
# Use apply with a user-defined function.

find.outlier <- function (data) {
  deviations <- abs(data - median(data))
  return (which.max(deviations))
}

outliers <- apply(sales, 1, find.outlier)
outliers

paste("Store ", 1:nrow(sales), "- Week ", outliers)

# sweep

data <- c(80, 75, 85, 82, 
          90, 88, 92, 95,
          81, 78, 84, 87)

sales <- matrix(data,
                nrow = 3, ncol = 4,
                byrow = TRUE)

sales

sweep(sales, 1, c(2,3,4), FUN="*")

apply(sales, 1, max)

sweep(sales, 1, apply(sales, 1, max), FUN="-")

# tapply

# Given voter's data
# 
# ages <- c(25,26,55,37,21,42)
# 
# affils <- c("R","D","D","R","U","D")
# 
# show the average age of the voters for each affiliation.


ages <- c(25,26,55,37,21,42)
ages

affils <- c("R","D","D","R","U","D")
affils

tapply(ages, affils, mean)

# Given the following data frame
# 
# d <- data.frame(
#   gender=c("M","M","F","M","F","F"),        
#   age=c(47,59,21,32,33,24),
#   income=c(55000,88000,32450,76500,123000,45650))
# 
# what is the average income by gender?


d <- data.frame( 
  gender=c("M","M","F","M","F","F"), 
  age=c(47,59,21,32,33,24),
  income=c(55000,88000,32450,76500,123000,45650))
d

# average income by gender
tapply(d$income,d$gender,mean)

# average age by gender
tapply(d$age,d$gender,mean)

# For the above data, what is the average income for people 
# whose age is over 25 versus people whose age is 25 or below?¶

d$over25 <- ifelse(d$age > 25,TRUE,FALSE)
d

tapply(d$income,d$over25,mean)

# For the above data, explore the average income by both the gender and over 25.

tapply(d$income,list(d$gender,d$over25),mean)

tapply(d$income,list(d$gender,d$over25),length)

# split

d

split(d$income,d$gender)

class(split(d$income,d$gender))

split(d$income,d$over25)

split(d$income,list(d$gender,d$over25))

# lapply

d

lapply(d, mean)

sapply(d, mean)

team.names  <- c("Patriots", "Red Sox")
player.names <- c("Brady", "Federer", "Pele")

favorites <- list(teams = team.names, 
                  players = player.names)

favorites

lapply(favorites, length)

sapply(favorites, length)

# Abraham Lincoln's Gettysburg Address

file <- "http://kalathur.com/cs544/data/lincoln.txt"

words <- scan(file, what=character())

length(words)

head(words)

words <- tolower(words)

head(words)

tail(words, n = 10)

words <- gsub("[[:punct:]]", "", words)

tail(words, n = 10)

words[nchar(words) == 0]

words <- words[nchar(words) != 0]

length(words)

length(unique(words))

unique(words)

table(words)

word_list <- list()

for (i in 1:length(words)) {
  wrd <- words[i]
  word_list[[wrd]] <- c(word_list[[wrd]],i) 
}  

head(word_list)

length(word_list)

names(word_list)

word_list$the

word_list$people

word_list$`in`

word_list[["in"]]

result1 <- lapply(word_list, length)

head(result1)

result2 <- sapply(word_list, length)

head(result2)

# alphabetic order
x <- result2[order(names(result2))]
head(x)

#sorted order by frequencies (top 5)
result2[order(result2, decreasing = TRUE)][1:5]

sort(result2, decreasing = TRUE)[1:5]

barplot(sort(result2, decreasing = TRUE)[1:5])





