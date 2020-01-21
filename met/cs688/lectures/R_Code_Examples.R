## R Examples 1


# ---- R Data Type Examples -------
# Matrices ~~~~~~~~~~~~~~~~~~~
m <- matrix(nrow=2,ncol=3) # Matrix 
m1 <- matrix(1:6,nrow=2,ncol=3) # Another way to defins a Matrix
x <- 1:3; y <- 1:6; 
m2 <- cbind(x,y) # Yet Another way to defins a Matrix
m3 <- rbind(x,y) # Yet Another way to defins a Matrix

# Lists ~~~~~~~~~~~~~~~~~~~
# (contain elements of different classes)
lst1 <- list(myInt=1:4,myLogic=c(T,F)) # List 

lst2 <- list(a=list(1,2,3,4),b=c(T,F)) # List of a List
z=lst2$a[[2]] # Subset element 2 from list a

lst3 <- list(myList=1:5)

# Factors ~~~~~~~~~~~~~~~~~~~
# (categorical data) 
x <- factor(c("yes","no","yes","yes"),levels=c("yes","no")) # Factor
table(x) # content count of each level (3 for "yes")
x1 <- factor(c("yes","no","yes","yes"),levels=c("no","yes")) # inverse order in levels

# Dataframe ~~~~~~~~~~~~~~~~~~~
# special type of list - Columns can be mixed classes
x <- data.frame(myInt=1:4,myLogic=c(T,T,T,F)) # Dataframe with specified col names  
row.names(x)=c("1a","1b","1c","1d") # give rows a name



# ---- Subsetting Examples -------
#    [] - Always returns an object of same class 
#    [[]] - Used to extract a single element from a List or Data Frame. 
#    $ - Used to extract a List or Data Frame elements by name. 

m1[1,] # subsets the first row of matrix object "m" 

x <- list(myInt=1:4,myLogic=c(T,F)) # create a list object "x" 
x[1] # returns a List  "1 2 3 4"
x[[1]] # returns integers "1 2 3 4" 
x$myLogic # returns "TRUE FALSE" 	
x$myInt[3:4] # returns "3 4" from the list name "myInt" 
x[c(1,2)] # returns 2 List objects "myInt and myLogic"

# Sub setting nested elements of a list (note the difference from the previous example)
x1 <- list(a=list(1,2,3,4),b=c(T,F)) # create a list object "x" 
x1[1] # returns a List of 4 lists "[1] [2] [3] [4]"

# Note: 
x1[2] # returns List "2" since [] is used
x1[[1]][2] # returns element "2" from List "a", since [] is used
x1$a[[2]] # returns element "2" from List "a" since [[]] is used 

# Partial Matching with [[]] - useful at command line
x <- list(myList=1:5) # create a list object "x" 
x$m # returns the List  named "myList" by matching "m" to it from the available objects in the workspace. 
x[["m",exact=FALSE]] # another way to achieve the same with [[]]


# ---- Removing Missing NA from an object -------
x <- c(1, 2, NA, 4, NA, 5) #
bad <- is.na(x) # logical of NA
x[!bad] # subset only the non missing data

# Using function complete.cases(y). 
y <- c("a", "b", NA, NA, "e", "f") # create data 
y[complete.cases(y)] # subset only the non missing data

# Note x and y have to be of same size.
good <- complete.cases(x,y) # Also note that it finds the intersect of NA in x and y


# ---- Vectorized operations -------
x <-1:4; y <-6:9 # create 2 objects 
x+y # elementwise addition
x > 2 # logical ( FFTT )
x >= 2 # logical ( FTTT )
y == 8 # logical ( FFTF )
x * y # elementwise multiplication  
x / y # elementwise division 

z <- seq(-20,3, by=2) # Creates a sequence from -20 to 3 with step 2

# ---- Matrix operations -------
x <- matrix(1:4,nrow=2,ncol=2) #  create 2x2 matrix 
y <- matrix(rep(10,4),2,2) #  create 2x2 matrix with element "10"
x * y # elementwise multiplication of 2 matrices
x / y # elementwise division of 2 matrices
x %*% y # regular matrix multiplication 




# ---- Paths in R - Examples -------
# --- File Import 
pth <- file.path("c:", "Users", "ZlatkoFCX","Documents","FCX","R Files","Insert ECfg")
CSV.files <- list.files(pth,".csv") # List of files with pattern ".csv"
R.files <- list.files(pth,".R") # List of files with pattern ".R"
Import.Files <- file.path(pth,"VAV007_OrigConfig.csv") # Specific File Name to Import 
Import.Files <- file.path(pth,CSV.files[2]) # Another Way  

# Ways to Import a File
temp.CSV <- read.csv(Import.Files,stringsAsFactors = FALSE,blank.lines.skip = TRUE,header=F)
temp1.CSV <- read.csv(file.path(pth,"temp.csv"),stringsAsFactors = FALSE,blank.lines.skip = TRUE,header=F)


# ###--- Set Operations ----
a <- c("asd","qwer","zxcvb")
b <- c("qwer","asd","zxcvb","poi")
setdiff(b,a) # Difference (larger first)
intersect(a,b) # Common 


# ---- Moving data between R and the clipboard  -------
# --- Clipboard Import & Edditing 
y1 <- readClipboard() # Set 1 from "Two_Sets_Example.xlsx"
y1 <-  gsub("-","_",y1) # Replace "-" with "_"

y2 <- readClipboard() # Set 2 from "Two_Sets_Example.xlsx"
y2 <- paste0("ECfg_",y2) # Paste & Insert Names

#--- Set Operations
Difference1 <- setdiff(y1,y2) # What is Different in y1
Difference2 <- setdiff(y2,y1) # What is Different in y2
Common <- intersect(y1,y2) # Common 

#--- Export to Clipboard 
writeClipboard(as.character(Difference)) # Place it on Clipboard 
writeClipboard(as.character(Common)) # Place it on Clipboard 

#--- Import Numbers from Excel 
y <- scan() 


# ---- Moving data between R and the clipboard  ------- 
















