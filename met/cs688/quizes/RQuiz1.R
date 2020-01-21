# MET CS688 R Quiz 1
rm(list=ls()); cat("\014") # clear all

# Q1: (5 points). Assign your First name Space Last name to an R object called "My.Name"
My.Name <- "anthony valencia"

# Vectors and Sequences
# Q2: (1 point) Create a sequence from -14 to 13 in increments of 6 and assign it to to object x  
 # -14  -8  -2   4  10
x <- seq(-14, 10, 6)

# Q3: (2 points) Write a line of code that will divide the second and fourth element of x by 4
 # -2  1
x[c(2, 4)]/4

# Q4: (2 points) Find the mean of all elements of x except the third
 # -2
mean(x[-3])

# Q5: (2 points) Create a sequence of 5 equally spaced elements starting from -1 and ending with 9 and assign it to object x1. 
 #  -1.0  1.5  4.0  6.5  9.0
x1 <- seq(-1, 9, length.out=5)

# Q6: (1 point) Write a line of code that will find what data type is x1
 # "numeric"
typeof(x1)

# Q7: (2 points) Subset elements of x1 that are larger than 3.6
 # 4.0 6.5 9.0
x1[x1>3.6]

# Q8: (1 point) Modify the third element of x1 to the letter "a"
 # "1"    "3.25" "a"    "7.75" "10"  
x1[3] <- "a"

# Q9: (1 point) Write a line of code that will find what data type is x1
 # "character"
typeof(x1)

# Q10: (1 point) Assign to object zn ONLY the numeric elements of x1.
 # -1.0  1.5  6.5  9.0
zn <- as.numeric(x1)
zn <- zn[!is.na(zn)]

# Q11: (2 points) Execute "x2 <- as.numeric(x1)" and write a line of code to find the max value of x2.
 # 9
x1 <- seq(-1, 9, length.out=5) # have to reset, otherwise will return NA
x2 <- as.numeric(x1)
max(x2)

# +20 Points === Total 20

# === ==== 
# Type mtcars in your R console and familiarize yourself with this dataset. 
# Q12: (2 points) Use the factor() function to find what are the possible values of “gear” in the "mtcars" dataset. 
 # "3" "4" "5"
factor(mtcars$gear)

# Q13: (3 points) Create a data frame that sumarizes how many cars are there for each number of forward gears.

#   Var1 Freq
# 1    3   15
# 2    4   12
# 3    5    5
as.data.frame(table(mtcars$gear))

# +5 Points === Total 25

# === ==== 
# Execute the following code:  
zstr <- "Given a string of characters zstr, find  the frequency of each character."
# Q14:(5 points) Given a string of characters zstr, find the frequency of each character ====

#     ,  .  a  c  d  e  f  g  G  h  i  n  o  q  r  s  t  u  v  y  z 
# 12  1  1  6  6  1  7  4  1  1  4  3  4  2  1  7  3  5  1  1  1  1  
table(zstr)

# Execute the following code:  
TextData1 <- c("asd","qwer","zsdb")
TextData2 <- c("qwer","asd","zxsdx","psdoisd")
Pattern <- "sd+" 
# Q15: (5 points) Write a line of code that will give the indices of elements in TextData2 that are also present in TextData1
 # 1 2
which(TextData1 %in% TextData2)

# Q16: (5 points) Write a line of code that will give the indices of elements in TextData2 that are matched by the string "Pattern"
 # 2 3 4
grep(Pattern, TextData2)

# Q17:(5 points): Create a function "Is.Full.Sq()" that returns TRUE/FALSE if n>=0 is a full square (i.e. for n=16 returns TRUE, for n=15 returns FALSE)
Is.Full.Sq <- function(n) {
  x <- sqrt(n)
  y <- floor(x)
  return(0 == (x-y))
}
Is.Full.Sq(16)
Is.Full.Sq(15)

# +20 Points === Total 45

# Q18: (1 point) Use an R function to substitute the space (i.e. " ") in "My.Name" with "~".
gsub(" ", "-", My.Name)



# Execute the following code:  
z1 <- rep(list(1),3)
z2 <- rep(list(c(1,2,3,4)),3)
My.Id.List <- lapply(seq_along(z1), function(Id, Val,i) { list(Id=Id[[i]]+i-1, Score=Val[[i]]^(i+1)) }, Id=z1, Val=z2)

# Q19: (2 points) Write a line of code to find how many elements does the My.Id.List list have? 
 # 3
length(My.Id.List)

# Q20: (2 points) What line of code would you use to find the names of the second element of My.Id.List? 
 # "Id"  "Score"
names(My.Id.List[[2]])

# +5 Points === Total 50

# === For Extra Credit ==== 
# Q21: (5 points) Write a function "Mean.Pi()" that will take as an argument an element of My.Id.List[[.]]$Score
# (i.e. My.Id.List[[2]]$Score), add the number pi to the "Score" field and return the mean ( i.e. mean(Some.List + pi) ). 
 # 28.14159
Mean.Pi <- function(n) {
  return(mean(My.Id.List[[n]]$Score + pi))
}

# Q22: (10 points) Write single line of code, that uses R's "do.call()" to find the "Mean.Pi" for all the Score elements 
# in My.Id.List as row binded ("rbind") 

#          [,1]
# [1,] 10.64159
# [2,] 28.14159
# [3,] 91.64159
do.call("rbind", Mean.Pi)
