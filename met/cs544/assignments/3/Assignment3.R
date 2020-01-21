library(UsingR)

# Part 1
primes
ds <- diff(primes)
ds.table <- table(ds)
ds.table
barplot(ds.table, xlab = "Prime diff", ylab = "Frequency")


# Part 2
# a
t <- table(coins$value)
# b
df <- as.data.frame(t)
colnames(df) <- c("value", "freq")
df$total <- as.numeric(as.character(df$value)) * as.numeric(as.character(df$freq))
df[,c("value","total")]
# c
sum(df$total)
# d
barplot(table(coins$year))


# Part 3
# a
stem(south)
# there are a lot of numbers in the low teens
# b
f <- fivenum(south)
c(f[2] - 1.5*(f[4] - f[2]),
  f[4] + 1.5*(f[4] - f[2]))
# outliers are 29 and 33
# c
boxplot(south, col=hcl(0), xaxt = "n",
        xlab = "south", horizontal = TRUE)
axis(side = 1, at = fivenum(south), labels = TRUE, 
     las=2)
text(fivenum(south), rep(1.2,5), srt=90, adj=0,
     labels=c("Min","Lower Hinge", "Median",
              "Upper Hinge", "Max"))


# Part 4
# a
table(pi2000)
# b
100 * table(pi2000)/sum(table(pi2000))
# c
hist(pi2000, right=F, breaks=10, col=hcl(0))


# Part 5
# a
x <- matrix(c(25, 10, 15, 20, 40, 20), nrow=2,
            byrow = TRUE)
# b
rownames(x) <- c("Men", "Women")
# c
colnames(x) <- c("NFL", "NBA", "MLB")
# d
dimnames(x) <- list(Gender=rownames(x), Sport=colnames(x))
# e
apply(x, 1, sum)
margin.table(x, 1)
apply(x, 2, sum)
margin.table(x, 2)
# f
margin.table(x)
addmargins(x)
# h
mosaicplot(x, color=c("red", "blue"))


# Part 6
# a
pairs(midsize)
# b
# 1) Accord increased every year
# 2) Camry increased every year
# 3) Taurus increased every year, with dramatic increase in last year
# 4) Accord and Camry increase is proportional


# Part 7
# a
bal.wins <- MLBattend[MLBattend$franchise == 'BAL',]$wins
bos.wins <- MLBattend[MLBattend$franchise == 'BOS',]$wins
det.wins <- MLBattend[MLBattend$franchise == 'DET',]$wins
la.wins <- MLBattend[MLBattend$franchise == 'LA',]$wins
phi.wins <- MLBattend[MLBattend$franchise == 'PHI',]$wins
# b
d <- data.frame(
  bal=bal.wins,        
  bos=bos.wins,
  det=det.wins,
  la=la.wins,
  phi=phi.wins)
# c
boxplot(d)
# d
# 1) medians of BAL, BOS, DET & LA between 80 and 90
# 2) median of PHI between 70 and 80
# 3) quartile range for BAL is largest
# 4) BOS & LA have low outliers
# 5) quartile range for BOS is very small