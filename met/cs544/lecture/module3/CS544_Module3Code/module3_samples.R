#### Module3

#### 2. Univariate Data

#### 2.1. Types of Data

state.abb

#### 2.2. Example Datasets for R

if (!is.element("UsingR", installed.packages()[,"Package"]))
  install.packages("UsingR", dep = TRUE)

library(UsingR)

#### 2.4. Categorial Data

x <- c("yes", "no", "no", "yes", "no")
table(x)

help(central.park.cloud)
central.park.cloud

table(central.park.cloud)

table(central.park.cloud) / length(central.park.cloud)

#### 2.5. Graphical Representations of Categorical Data

## For jupyter notebook - plot heights

if (!is.element("repr", installed.packages()[,"Package"]))
  install.packages("repr", dep = TRUE)

library(repr)

options(repr.plot.height=4)
##

barplot(table(central.park.cloud), 
  col = "cyan", ylim=c(0,12),
  xlab = "Type", ylab = "Frequency")
  
barplot(table(central.park.cloud), 
  col = "cyan", ylim=c(8,12),
  xlab = "Type", ylab = "Frequency", xpd=FALSE)

  
barplot(table(central.park.cloud), horiz = TRUE,
  col = "cyan", xlim=c(0,12), las=2,
  xlab = "Frequency")

pie(table(central.park.cloud), 
  col=hcl(c(0, 60, 120)))
  
data <- table(central.park.cloud)
slice.labels <- names(data)
slice.percents <- round(data/sum(data)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")

pie(data, labels = slice.labels, 
  col=hcl(c(0, 60, 120)))


  
#### 2.6. Numerical Data

# Odd number of values
x <- c(71,72,73,73,74,75,77,81,83,87,91)
x

mean(x)

mean(x, trim=0.1)

median(x)

table(x)
which(table(x) == max(table(x)))

range(x)
diff(range(x))

var(x)
sd(x)

fivenum(x)

summary(x)

quantile(x, c(0, 0.25, 0.5, 0.75, 1))

IQR(x)

scale(x)

# Even number of values
x <- c(71,72,73,73,74,75,77,81,83,87,90,91)
x

fivenum(x)

summary(x)

quantile(x, c(0, 0.25, 0.5, 0.75, 1))

IQR(x)

#### 2.7. Graphical Representations of Numerical Data

help(central.park)

central.park$MIN

summary(central.park$MIN)

library(repr)
options(repr.plot.width=4, repr.plot.height=3)

attach(central.park)

barplot(MIN, names.arg=1:31, 
  xlab="Day", ylab = "Min Temp",
  col = "cyan", las=2)

dotchart(MIN, labels=1:31, 
  xlab="Min Temp")

table(MIN)

barplot(table(MIN), 
  col = "cyan", ylim=c(0,6),
  xlab = "Min Temp", ylab = "# of Days")

detach(central.park)

#### 2.8. Stem plots

x <- c(72,73,71,73,74,75,77,81,83,87,91,92)
stem(x)

#### 2.9. Histograms and Boxplots

help(alltime.movies)

head(alltime.movies, n = 2)

attach(alltime.movies)
hist(Gross, col=hcl(0), ylim=c(0,35))

x <- hist(Gross)
names(x)

x$breaks

x$counts


par(mfrow=c(1,2))

x1 <- hist(Gross, breaks=seq(170,620,25), 
   col=hcl(0), ylim=c(0,35))

x2 <- hist(Gross, breaks=5, 
   col=hcl(0), ylim=c(0,35))

par(mfrow=c(1,1))

x1$breaks
x1$counts

x2$breaks
x2$counts

detach(alltime.movies)

# Boxplot

x <- c(71,72,73,73,74,75,77,81,83,87,91,92)
boxplot(x, horizontal = TRUE)

fivenum(x)

boxplot(x, horizontal = TRUE, xaxt = "n")
axis(side = 1, at = fivenum(x), labels = TRUE)


y <- c(35, 40,50,65,72,73,73,74,75,81,83,87,92)
boxplot(y, horizontal = TRUE, xaxt = "n")
axis(side = 1, at = fivenum(y), labels = TRUE)

f <- fivenum(y)
f
c(f[2] - 1.5*(f[4] - f[2]),
  f[4] + 1.5*(f[4] - f[2]))
  

z <- c(35, 40,55, 65,66,70,72,72,73,73,74,74,79,90)
boxplot(z, horizontal = TRUE, xaxt = "n")
axis(side = 1, at = fivenum(z), labels = TRUE, las=2)

f <- fivenum(z)
f
c(f[2] - 1.5*(f[4] - f[2]),
  f[4] + 1.5*(f[4] - f[2]))


attach(alltime.movies)
boxplot(Gross, col=hcl(0), 
   xlab = "Gross Sales", horizontal = TRUE)

fivenum(Gross)

boxplot(Gross, col=hcl(0), xaxt = "n",
   xlab = "Gross Sales", horizontal = TRUE)
axis(side = 1, at = fivenum(Gross), labels = TRUE, 
  las=2)
text(fivenum(Gross), rep(1.2,5), srt=90, adj=0,
  labels=c("Min","Lower Hinge", "Median",
           "Upper Hinge", "Max"))

f <- fivenum(Gross)
titles <- rownames(alltime.movies)

titles[Gross > f[4] + 1.5*(f[4] - f[2])]
Gross[Gross > f[4] + 1.5*(f[4] - f[2])]

detach(alltime.movies)


#### 3. Bivariate Data

library(UsingR)

#### 3.2. Two-way tables - Unsummarized data

help(grades)

head(grades)

attach(grades)
table(prev, grade)

#### 3.3 Two-way tables - Summarized data

x <- rbind(c(56,8), c(2,16))
x

x <- cbind(c(56,2), c(8,16))
x

x <- matrix(c(56,2,8,16), nrow=2)
x

x <- matrix(c(56,8,2,16), nrow=2,
  byrow = TRUE)
x

rownames(x) <- c("buckled", "unbuckled")
colnames(x) <- c("buckled", "unbuckled")
x

row1 <- c(56,8)
names(row1) <- c("buckled", "unbuckled")

row2 <- c(2,16)

x <- rbind(buckled = row1, unbuckled = row2)
x

tmp <- c("buckled", "unbuckled")
dimnames(x) <- list(parent=tmp, child=tmp)
x


#### 3.4. Marginal distributions of two-way tables

x

apply(x, 1, sum)

margin.table(x, 1)

apply(x, 2, sum)

margin.table(x, 2)

margin.table(x)

addmargins(x)

#
y <- table(prev, grade)
y

margin.table(y, 1)

margin.table(y, 2)

margin.table(y)

#### 3.5. Conditional distributions of two-way tables

options(digits = 2)

addmargins(x)

prop.table(x, 1)

prop.table(x, 2)

prop.table(x)

options(digits = 1)

prop.table(y, 1)

prop.table(y, 2)

options(digits = 4)


#### 3.6. Graphical Summarization of two-way tables
# Mosaic plot

x

mosaicplot(x, color=c("red", "blue"))

mosaicplot(table(grades), color="cyan")

# Bar plot

x

barplot(x, xlab = "Parent", 
  main = "Child Seat-Belt Usage",
  ylim=c(0,60), col=c("red", "blue"))

barplot(x, xlab = "Parent", 
  beside = TRUE, legend.text = TRUE,
  main = "Child Seat-Belt Usage",
  ylim=c(0,60), col=c("red", "blue"))
  
t(x)

barplot(t(x), xlab = "Child", 
  beside = TRUE, legend.text = TRUE,
  main = "Parent Seat-Belt Usage",
  ylim=c(0,60), col=c("red", "blue"))
  
barplot(table(grades), xlab = "Prev Grade", 
  beside = TRUE, legend.text = TRUE,
  main = "Current Grade", border=FALSE,
  args.legend = list(x = "center"),
  ylim=c(0,20), col = rainbow(9)) 
  
#### 3.7. Relationships in Numeric Data

help(home)

head(home, n=2)

options(scipen = 5)

attach(home)

plot(old, new)

summary(old)

summary(new)

summary(new/old)

detach(home)

#

help(maydow)

head(maydow, n = 2)

attach(maydow)

plot(max.temp, DJA)

detach(maydow)

#

help(kid.weights)
boys <- subset(kid.weights, gender == 'M')
girls <- subset(kid.weights, gender == 'F')

par(mfrow=c(1,2))

plot(boys$height, boys$weight, ylim=c(0,160))

plot(girls$height, girls$weight, ylim=c(0,160))

par(mfrow=c(1,1))

x <- seq(10:60)
plot(x, x^2, type="l")

plot(kid.weights$height, kid.weights$weight,
  pch = as.character(kid.weights$gender))
  

#### 4. Multivariate Data

library(UsingR)

names(student.expenses)

head(student.expenses, n = 2)

attach(student.expenses)
table(cable.tv, cable.modem)

table(cable.tv, car)

table(cable.tv, cable.modem, car)

ftable(table(cable.tv, cable.modem, car),
  col.vars = c("car", "cable.modem"))

ftable(table(cable.tv, cell.phone, cable.modem, car),
  col.vars = c("car", "cable.modem", "cell.phone"))

detach(student.expenses)

#### 4.2. Independent Samples

names(ewr)

head(ewr, n = 2)
tail(ewr, n = 2)

attach(ewr)

boxplot(AA,CO,DL,HP,NW,TW,UA,US,
  col = rainbow(8))
# or 
boxplot(ewr[3:10], col = rainbow(8))

par(mfrow=c(1,2))

boxplot(ewr[inorout == "in", 3:10],
  main = "Taxi in", col = rainbow(8))
  
boxplot(ewr[inorout == "out", 3:10],
  main = "Taxi out", col = rainbow(8))

par(mfrow=c(1,1))

#### 4.3. Relationship Between Pairs of Variables

head(ewr[3:6], n = 2)

pairs(ewr[inorout == "in", 3:6], 
  main = "Taxi in", pch=16)

pairs(ewr[inorout == "out", 3:6], 
  main = "Taxi out", pch=16)

#### 4.4. Summarized Data

dimnames(UCBAdmissions)

UCBAdmissions

margin.table(UCBAdmissions, 2)
prop.table(margin.table(UCBAdmissions, 2))

margin.table(UCBAdmissions, 1)
prop.table(margin.table(UCBAdmissions, 1))

margin.table(UCBAdmissions, c(1, 2))

x <- margin.table(UCBAdmissions, c(1, 2))
prop.table(x, margin = 1)

prop.table(x, margin = 2)

mosaicplot(x, color=c("green", "red"))

par(mfrow=c(3,2))
for (i in 1:2) 
  barplot(UCBAdmissions[,,i],
     col = c("green", "red"), legend.text=TRUE,
     main=paste("Dept",LETTERS[i],sep=" "))
for (i in 3:6) 
  barplot(UCBAdmissions[,,i],
     col = c("green", "red"), 
     main=paste("Dept",LETTERS[i],sep=" "))
par(mfrow=c(1,1))

par(mfrow=c(3,2))
for (i in 1:2) 
  barplot(t(UCBAdmissions[,,i]),
     col = c("cyan", "violet"), legend.text=TRUE,
     main=paste("Dept",LETTERS[i],sep=" "))
for (i in 3:6) 
  barplot(t(UCBAdmissions[,,i]),
     col = c("cyan", "violet"), 
     main=paste("Dept",LETTERS[i],sep=" "))
par(mfrow=c(1,1))



par(mfrow=c(3,2))
for (i in 1:6) 
  mosaicplot(UCBAdmissions[,,i],
     col = c("green", "red"),
     main=paste("Dept",LETTERS[i],sep=" "))
par(mfrow=c(1,1))

par(mfrow=c(3,2))
for (i in 1:6) 
  mosaicplot(t(UCBAdmissions[,,i]),
     col =  c("cyan", "violet"), 
     main=paste("Dept",LETTERS[i],sep=" "))
par(mfrow=c(1,1))




