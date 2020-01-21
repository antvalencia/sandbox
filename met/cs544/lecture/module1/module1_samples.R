#### 3.2 - Assignments

x <- 10

x

y = "Hello"

y

TRUE -> z

z

#### 3.3 - Data Types

x <- 7
y <- x/2

x
y

mode(x)
typeof(x)

mode(y)
typeof(y)

x <- as.integer(7)
y <- as.integer(x/2)

x
y

mode(x)
typeof(x)

mode(y)
typeof(y)

a <- 10
b <- 20
x <- a >= b
y <- (a > 5) & (b < 25)

x
y

mode(x)
typeof(x)

mode(y)
typeof(y)

x <- "Hello"
y <- as.character(123)

x
y

mode(x)
typeof(x)

mode(y)
typeof(y)

x <- "Hello"
y <- 123

paste(x, y)

paste(x, y, "Testing", sep = ",")

z <- sprintf("You want to say %s on %d", x, y)
z

nchar(z)

substr(z, start = 5, stop = 21)

sub("Hello", "Bye", z)

x <- 2 + 3i
y <- 3 - 1i

mode(x)
typeof(x)

x + y

x * y

#### 3.4 - Data Type Conversions

x <- TRUE
y <- FALSE

as.numeric(x)
as.numeric(y)

as.character(x)
as.character(y)
#
x <- 1
y <- 0
z <- 20

as.logical(x)
as.logical(y)
as.logical(z)

as.character(x)
as.character(y)
as.character(z)
#
x <- "TRUE"
y <- "FALSE"
z <- "20"

as.logical(x)
as.logical(y)
as.logical(z)

as.numeric(x)
as.numeric(y)
as.numeric(z)


#### 3.6 - Vectors

ages <- c(20, 22, 23, 23, 26)

ages

mode(ages)
typeof(ages)
#
sizes <- c(12.6, 8.4)

sizes

mode(sizes)
typeof(sizes)
#
x <- as.integer(sizes)

x

mode(x)
typeof(x)
#
first.names <- c("Alice", "Bob", "Charlie")

first.names

mode(first.names)
typeof(first.names)
#

voted <- c(TRUE, FALSE, TRUE, TRUE)

voted

mode(voted)
typeof(voted)

x <- as.numeric(voted)

x

as.logical(x)
#

c(ages, sizes)

x <- c(ages, first.names)

x

as.numeric(x)

#
ages

length(ages)
sum(ages)

ages/2

2 * ages

ages < 25

ages >= 23
#
c(0, ages, 100)

c(ages, 2*ages)
#
x <- c(10, 20, 30, 40)
y <- c(2, 4)

x*y

x+y
#
paste(first.names, ages)
paste(first.names, ages, sep=",")

#### 3.7 - Indexing
ages
ages[3]
ages[1:3]
ages[c(1,5)]

ages[-1]
ages[-(1:3)]
ages[c(-1,-5)]

ages[c(TRUE, TRUE, TRUE, FALSE, FALSE)]

ages[c(TRUE, FALSE)]

letters[c(1, 26)]

letters[1:5]

letters[c(TRUE, FALSE)]

#### 3.8 - Sequences

20:25

25:20

mode(20:25)
typeof(20:25)


2 * 1:5

x <- 2 * 1:5
mode(x)
typeof(x)


n <- 5

1:n-1

1:(n-1)

seq(0, 5)

seq(from = 0, to = 5)

seq(2, 10, by = 2)

seq(from = 2, to = 10, by = 2)

seq(2, by = 2, length = 5)

seq(10, 2)

seq(from = 10, to = 2, by = -2)

seq(from = 10, by = -2, length = 5)


seq(1, 2, by = 0.25)

seq(from = 1.5, to = 4.5, by = 0.5)


x <- c(1,2,3,4,5)
x

rep(x, 3)

rep(x, times = 3)

rep(x, each = 3)

rep(x, times = 2, each = 3)

rep(x, c(1,2,1,2,1))

rep(x, c(0,2,0,3,0))

rep(x, x)

#### 3.9 - Modifying vectors

x <- 1:5
x

x[1] <- 10
x

x[c(2,5)] <- c(20, 50)
x

x[8] <- 80
x

length(x) <- 10
x

length(x) <- 3
x

#### 3.10 - Named vectors

ages <- c(20, 22, 23, 23, 26)

names(ages) <- c("Alice", "Bob", 
    "Charlie", "Dave", "Ed")
    
ages

ages["Bob"]

ages[c("Alice", "Ed")]

ages[2]

ages[c(1,5)]

names(ages)

names(ages)[2] <- "Robert"

ages

#### 3.11 - Scalar as Vector

x <- 10
x
is.vector(x)
length(x)

10 == c(10)

y <- "Hello"
y

z <- TRUE
z


#### 3.12 - Factors

voted <- c("yes", "no", "yes", "yes", "no")
voted

f1 <- factor(voted)
f1

levels(f1)

is.character(voted)

is.character(f1)

is.factor(f1)

as.numeric(voted)

as.numeric(f1)


f2 <- factor(voted, labels=c("bad", "good"))
f2

levels(f2)

f3 <- factor(voted, labels=c("good", "bad"))
f3

levels(f3)

summary(voted)

summary(f1)

summary(f2)

summary(f3)

voted <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
voted

f1 <- factor(voted)
f1

levels(f1)

is.logical(voted)

is.logical(f1)

is.factor(f1)

voted <- c(1, 0, 1, 1, 0)
voted

f1 <- factor(voted)
f1

levels(f1)

is.numeric(voted)

is.numeric(f1)

is.factor(f1)

f1[1] < f1[2]

# Ordered Factors

skills <- c("advanced", "novice", "novice",
   "intermediate", "advanced")

skills

o1 <- ordered(skills)
o1

o1 <- factor(skills, ordered = TRUE)
o1

levels(o1)

is.factor(o1)

is.ordered(o1)

  
o2 <- ordered(skills, 
  levels=c("novice", "intermediate", "advanced"))
o2

levels(o2)

o2[1] > o2[2]
o2[1] < o2[4]

#### 3.13 - Matrices

# By row
data <- c(80, 75, 85, 82, 
          90, 88, 92, 95,
          81, 78, 84, 87)

scores <- matrix(data,
  nrow = 3, ncol = 4,
  byrow = TRUE)

scores

# By column
data <- c(80, 90, 81, 
          75, 88, 78,
          85, 92, 84,
          82, 95, 87)

scores <- matrix(data,
  nrow = 3, ncol = 4)

scores


scores[1, 2]

scores[1, ] 

scores[ ,1]

scores[ , c(1,3)] 

scores[c(2,3), ]

scores[c(2,3), c(1,3)]

#### 3.14 - Named Matrices

dimnames(scores) <- list(
   c("Alice", "Bob", "Charlie"),
   c("Quiz1", "Quiz2", "Quiz3", "Quiz4"))
   
scores

scores["Alice", "Quiz2"]

scores["Alice", ]

scores[ ,"Quiz1"]

scores[ , c("Quiz1", "Quiz3")] 

scores[c("Alice", "Charlie"), ]

scores[c("Alice", "Charlie"), 
       c("Quiz1", "Quiz3")]

rownames(scores)

colnames(scores)

is.matrix(scores)

dim(scores)

nrow(scores)

ncol(scores)

as.vector(scores)

# 3.15. Modifying Matrix Entries

scores[1, 1] <- 100
scores

scores[1, ] <- 90
scores

scores["Alice", ] <- c(91, 92, 93, 94)
scores

scores["Alice", ] <- c(80, 90)
scores

scores[,] <- 100
scores

# 3.16 - Lists

num.data  <- c(3, 5, 7)
char.data <- c("a1", "b2", "c3", "d4")

list.data <- list("Hello", num.data, char.data, 20)

list.data

list.data[2]

list.data[c(2, 3)]

list.data[[2]]

list.data[[c(2,1)]]
list.data[[c(2,3)]]

#### 3.17 - Modifying Lists

list.data[[2]][1] <- 30
list.data

list.data[[2]] <- 1:10
list.data

num.data

x <- list.data[[2]]

x

list.data[[2]] <- 10:1

list.data

x

#### 3.18 - Named Lists

team.names  <- c("Patriots", "Red Sox")
player.names <- c("Brady", "Federer", "Pele")

favorites <- list(teams = team.names, 
                  players = player.names)
                  
favorites

favorites[[1]]

favorites[["teams"]]

favorites$teams

favorites[[2]]

favorites[["players"]]

favorites$players

attach(favorites)

teams

players

detach(favorites)

favorites$teams[2] <- "Yankees"

favorites

is.list(list.data)

is.list(list.data[2])

is.list(favorites)

is.list(favorites$players)

is.list(favorites["players"])


#### 3.19 - Data Frames

athlete.names <- c("Mayweather", "Ronaldo", 
                "James", "Messi", "Bryant")
athlete.sport <- c("Boxing", "Soccer",
              "Basketball", "Soccer", "Basketball")
athlete.salary <- c(105, 52, 19.3, 41.7, 30.5)
athlete.endorsements <- c(0, 28, 53, 23, 31)

athlete.info <- data.frame(
              athlete.names, athlete.salary,
              athlete.endorsements, athlete.sport)

athlete.info

athlete.info <- data.frame(
     Name = athlete.names, 
     Salary = athlete.salary,
     Endorsements =  athlete.endorsements, 
     Sport = athlete.sport)

athlete.info

colnames(athlete.info)

x <- data.frame(c(1,2), c(3,4))
x

colnames(x) <- c("Col1", "Col2")
x


dim(athlete.info) 

nrow(athlete.info) 

ncol(athlete.info)
            
dimnames(athlete.info)

rownames(athlete.info)

names(athlete.info)

#### 3.20 - Accesing Data Frame Data

athlete.info[[1]]

athlete.info[["Salary"]]

athlete.info$Sport

athlete.info[ ,1]

athlete.info[ ,"Salary"]

summary(athlete.info$Sport)

summary(athlete.info$Salary)

attach(athlete.info)

Salary

summary(Sport)

summary(Salary)

detach(athlete.info)

athlete.info[3, 2]
athlete.info[3,"Salary"]
athlete.info[athlete.info$Name == "James","Salary"]

attach(athlete.info)
athlete.info[Name == "James","Salary"]
detach(athlete.info)

#### 3.21 -  Slicing Columns of Data Frame

athlete.info[1]

athlete.info["Sport"]

athlete.info[c(1,4)]

athlete.info[c("Name", "Sport")]

#### 3.22 - Naming Data Frame Rows

rownames(athlete.info)

rownames(athlete.info) <-
    c("First", "Second", "Third",
      "Fourth", "Fifth")
      
athlete.info

rownames(athlete.info)

#### 3.23 - Slicing Data Frame Rows

athlete.info[2, ]

athlete.info["Second", ]

athlete.info[c(1, 3), ]

athlete.info[c("First", "Third"), ]

# Logical indexing

athlete.info[c(FALSE, TRUE, FALSE, TRUE, FALSE), ]

athlete.info$Sport == "Soccer"

athlete.info[athlete.info$Sport == "Soccer", ]

#### 3.24 - Subset of a Data Frame

subset(athlete.info, Sport == "Soccer")

subset(athlete.info, 
  Sport == "Soccer" & Salary > 50)
  
subset(athlete.info, Sport == "Soccer", 
  select = c(Name, Salary))



####  3.25 - Modifying a Data Frame

data.orig <- athlete.info

athlete.info$Pay <- 
   athlete.info$Salary + athlete.info$Endorsements

athlete.info

data.orig

athlete.info[1,2] <- 0
athlete.info["First", "Endorsements"] <- 100
athlete.info

athlete.info$Salary <- 50
athlete.info

athlete.info$Salary <- c(10, 20, 30, 40, 50)
athlete.info

athlete.info$Pay <- 
   athlete.info$Salary + athlete.info$Endorsements

athlete.info


athlete.info$Pay <- NULL

athlete.info

####

     


