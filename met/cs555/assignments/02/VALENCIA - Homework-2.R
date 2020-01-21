# Question 1
setwd("/Users/anthony.valencia/met/cs555/assignments/02/")

participants <- read.csv("participants")
participants <- participants$CalorieIntakeForparticipants
non.participants <- read.csv("non-participants")
non.participants <- non.participants$CalorieIntakeForNon.participants

max.len = max(length(participants), length(non.participants))
participants = c(participants, rep(NA, max.len - length(participants)))
non.participants = c(non.participants, rep(NA, max.len - length(non.participants)))

df <- data.frame(participants, non.participants)
names(df) <- c("participants", "non.participants")
df
summary(df$participants)
summary(df$non.$participants)

participants <- df$participants[!is.na(df$participants)]
boundaries <- seq(min(participants)-10, max(participants)+10, by=20)
h <- hist(participants, breaks=boundaries, plot=FALSE)
plot(
  h,
  xaxt = "n",
  xlab = "calorie intake",
  ylab = "frequency",
  main = "calorie intake for participants",
  col = "cadetblue"
)
axis(1, h$mids, labels=seq(floor(min(participants)), ceiling(max(participants)), by=20), tick=FALSE, padj=-1.5)

non.participants <- df$non.participants[!is.na(df$non.participants)]
boundaries <- seq(min(non.participants)-10, max(non.participants)+10, by=20)
h <- hist(non.participants, breaks=boundaries, plot=FALSE)
plot(
  h,
  xaxt = "n",
  xlab = "calorie intake",
  ylab = "frequency",
  main = "calorie intake for non-participants",
  col = "cadetblue"
)
axis(1, h$mids, labels=seq(floor(min(non.participants)), ceiling(max(non.participants)), by=20), tick=FALSE, padj=-1.5)



# Q2
#     Does the mean calorie consumption for those who participated in the meal preparation differ from 425? 
#     Formally test at the alpha=0.05 level using the 5 steps outlined in the module.
sample.mean.participants <- mean(participants)
sample.sd.participants <- sd(participants)
n.participants <- length(participants)
t <- (sample.mean - 425)/(sample.sd/sqrt(n))
# critical value for df = 25 - 1 = 24 and p = 0.05 is 1.711
# reject H0 if |t|≥1.711
# |t| = 0.3027212, so calorie consumption does not differ


# Q3
#     Calculate a 90% confidence interval for the mean calorie intake for participants in the meal preparation
#     Interpret the confidence interval.
sample.mean.participants + (1.711 * (sample.sd.participants/sqrt(n.participants))) # 467.5706
sample.mean.participants - (1.711 * (sample.sd.participants/sqrt(n.participants))) # 395.2286
sample.mean.participants                                                           # 431.3996

# Q4 Formally test whether or not participants consumed more calories than non-participants at the alpha=0.05 level using
#     the 5 steps outlined in the module.
sample.mean.non.participants <- mean(non.participants)
sample.sd.non.participants <- sd(non.participants)
n.non.participants <- length(non.participants)
s.non.participants.sqd <- sample.sd.non.participants^2
s.participants.sqd <- sample.sd.participants^2
t <- (sample.mean.participants - sample.mean.non.participants)/(sqrt((s.participants.sqd/n.participants) + (s.non.participants.sqd/n.non.participants)))
# t = 2.824836
# critical value for df = 25 - 1 = 24 and p = 0.05 is 1.711
# t = 2.824836 >= 1.711, so we reject H0 (participants consumed more calories than non-participants)

# Q5 Are the assumptions of the test used in (4) met?  How do you know?
The assumptions used in 4 are the following:
1. samples must be independent (not infueniing eaih other) and randomly selected from the two distinct populations of interest
    the samples are assumed to be randomly selected fro mtwo populations of interest, but attmitedly, this is assumed.
2. the variable of interest must be measured in the same way in each of the populations
    calorie consumption is assumed to be measured in the same way.
3. the parameter of interest should be normally distributed (or at least have similar shapes and without outliers)
