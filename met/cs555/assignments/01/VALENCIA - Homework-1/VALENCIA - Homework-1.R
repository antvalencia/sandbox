# Question 1
setwd("/Users/anthony.valencia/met/cs555/assignments/01/")
data <- read.csv(file="data.csv", header=FALSE, sep=",")
data <- unlist(data, use.names = FALSE)


# Question 2
boundaries <- seq(min(data)-0.5, max(data)+0.5, by=1)
h <- hist(data, breaks = boundaries, plot = FALSE)
plot(
  h,
  xaxt = "n",
  xlab = "days",
  ylab = "frequency",
  main = "days spent in hospital",
  col = "cadetblue"
  )
axis(1, h$mids, labels=seq(1,max(data),1), tick=FALSE, padj= -1.5)
iqr <- IQR(data)
s <- summary(data)
bottom_cuttoff <- s[2] - 1.5*iqr
top_cuttoff <- s[5] + 1.5*iqr
table(data[data <= bottom_cuttoff])
table(data[data >= top_cuttoff])


# Question 3
s
sd(data)


# Question 4
pnorm(7, mean=5, sd=3)*100

1-pnorm((7-5)/(3/sqrt(10)), mean=5, sd=3)

      