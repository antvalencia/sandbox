# load 2010-present event data
load ("/Users/anthony.valencia/met/cs544/project/data/rdata/2010-present-event.RData")

# change home team name from FLO to MIA (for consistency)
events2010$home_team <- with(events2010, ifelse(home_team == "FLO", "MIA", home_team))
# to dataframe
events2010.df <- data.frame(events2010)
# remove erroneously recorded 0 defensive positions
events2010.df <- events2010.df[events2010.df$defensive_position != 0, ]

# import positions data
load ("/Users/anthony.valencia/met/cs544/project/data/rdata/positions.RData")

# merge 2010 events data frame with positions data frame
events2010.df <- merge(x=events2010.df, y=positions, by.x="defensive_position", by.y="position_index", fill=-9999)
# drop defensive position as index
events2010.df$defensive_position <- NULL
# rename named position to defensive position
names(events2010.df)[names(events2010.df) == 'position'] <- 'defensive_position'

# import event types data
load ("/Users/anthony.valencia/met/cs544/project/data/rdata/event_types.RData")

# merge 2010 events data frame with event type data frame
events2010.df <- merge(x=events2010.df, y=event_types, by.x="event_type", by.y="event_type_index", fill=-9999)
# drop event_type as index
events2010.df$event_type <- NULL
# rename named position to defensive position
names(events2010.df)[names(events2010.df) == 'event'] <- 'event_type'

# view HR events (event type 23 is HR)
hr.events <- events2010[events2010$event_type == 23, ]
# HR frequency by position
hr.table <- table(hr.events$defensive_position)
# HR percentage by position
100*hr.table/nrow(hr.events)

# view SO events (event type 3 is SO)
so.events <- events2010[events2010$event_type == 3, ]
# SO frequency by position
so.table <- table(so.events$defensive_position)
# SO percentage by position
100*so.table/nrow(so.events)

# view player hit events (event type 16 is player hit by ball)
hit.events <- events2010[events2010$event_type == 16, ]
# player hit frequency by position
hit.table <- table(hit.events$defensive_position)
# player hit percentage by position
100*hit.table/nrow(hit.events)

# install & use package repr
if (!is.element("repr", installed.packages()[,"Package"]))
  install.packages("repr", dep = TRUE)
library(repr)

options(repr.plot.height=4)
# percentage of HRs by position as bar plot
barplot(100*table(hr.events$defensive_position)/nrow(hr.events), 
        col = "cyan", ylim=c(0,20),
        xlab = "Position", ylab = "Percentage")
# percentage of HRs by position as bar plot
barplot(100*table(hr.events$defensive_position)/nrow(hr.events), 
        col = "cyan", ylim=c(0,20),
        xlab = "Position", ylab = "Percentage", xpd=FALSE)
# percentage of HRs by position (horizontal) as bar plot
barplot(100*table(hr.events$defensive_position)/nrow(hr.events), horiz = TRUE,
        col = "cyan", xlim=c(0,20), las=2,
        xlab = "Percentage")
# percentage of HRs by position as pie chart
pie(table(hr.events$defensive_position)/nrow(hr.events), 
    col=hcl(c(0, 60, 120)))
# pie chart labeled with percentages and positions
data <- table(hr.events$defensive_position)/nrow(hr.events)
slice.labels <- names(data)
slice.percents <- round(data/sum(data)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
pie(data, labels = slice.labels, 
    col=hcl(c(0, 60, 120)))

# Numerical Data
library(dplyr)
# group RBIs by player and year
rbi.by.player <- events2010.df %>%
  group_by(year, res_batter) %>%
  summarise(rbis_on_year=sum(RBI_on_play))
# display players with more than 125 RBIs/year
rbi.by.player[rbi.by.player$rbis_on_year > 125,]

# get player names
load ("/Users/anthony.valencia/met/cs544/project/data/rdata/player_manager_coach.RData")
# merge 2010 events data frame with positions data frame
rbi.by.player <- merge(x=rbi.by.player, y=player_manager_coach, by.x="res_batter", by.y="ID", fill=-9999)

# display players with more than 125 RBIs/year
over.125.rbis <- rbi.by.player[rbi.by.player$rbis_on_year > 125,]
over.125.rbis[with(over.125.rbis, order(-rbis_on_year, -year, LAST, FIRST)), ]

# get all players who played more than 10 games
games.per.player <- events2010.df[c("game_id", "res_batter")]
games.per.player <- games.per.player[!duplicated(games.per.player), ]
games.per.player.df <- as.data.frame(table(games.per.player$res_batter))
colnames(games.per.player.df) <- c("player_id", "game_count")
games.per.player.trimmed.df <- games.per.player.df[games.per.player.df$game_count > 10,]
rbi.by.featured.player <- merge(x=rbi.by.player, y=games.per.player.trimmed.df, by.x="res_batter", by.y="player_id")
rbi.by.featured.player

x <- rbi.by.featured.player$rbis_on_year

mean(x)
# 24.85745

# trim 0.05 from each end of mean
mean(x, trim=0.1)
# 20.02718

median(x)
# 13

table(x)

# get max
which(table(x) == max(table(x)))

range(x)
# 0 139

diff(range(x))

# variance
var(x)
# 822.0773

# standard deviation
sd(x)
# 28.67189

# minimum, lower-hinge, median, upper-hinge, maximum
fivenum(x)
# 0   2  13  41 139

summary(x)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    2.00   13.00   24.86   41.00  139.00

# ###################
quantile(x, c(0, 0.25, 0.5, 0.75, 1))
# 0%  25%  50%  75% 100% 
# 0    2   13   41  139 

# inter-quartile range
IQR(x)

# scale
scale(x)

#### 2.7. Graphical Representations of Numerical Data
stem(x)

options(repr.plot.width=4, repr.plot.height=3)

hr.df <- as.data.frame(hr.table)
colnames(hr.df) = c("position", "frequency")
barplot(hr.df$frequency, names.arg=names(hr.table),
        xlab="position", ylab="HRs",
        col = "cyan", las=2)

dotchart(hr.df$frequency, groups=hr.df$position, 
         xlab="HRs",
         ylab="position")

barplot(hr.table, 
        xlab = "position",
        ylab = "HRs",
        col = "cyan", las=2)

# Central Limit Theorem
ones <- rep(1, nrow(events2010))
events2010["unearned_runs"] <- (
  (ones & ((events2010$batter_dest == 5) | (events2010$batter_dest == 6))) + 
    (ones & ((events2010$first_runner_dest == 5) | (events2010$first_runner_dest == 6))) + 
    (ones & ((events2010$second_runner_dest == 5) | (events2010$second_runner_dest == 6))) + 
    (ones & ((events2010$third_runner_dest == 5) | (events2010$third_runner_dest == 6)))
)
events2010["other_runs"] <- (
  (ones & (events2010$batter_dest == 4)) + 
    (ones & (events2010$first_runner_dest == 4)) + 
    (ones & (events2010$second_runner_dest == 4)) + 
    (ones & (events2010$third_runner_dest == 4)) - 
    events2010$RBI_on_play
)
events2010["all_runs_on_play"] <- events2010$RBI_on_play + events2010$unearned_runs + events2010$other_runs
maybe.last.inning <- events2010$inning >= 9
third.out <- (events2010$outs + events2010$outs_on_play == 3)
home.team.batting <- events2010$batting_team == 1
events2010$home_score_after_play <- with(events2010, ifelse(batting_team == 1, home_score + all_runs_on_play, home_score))
events2010$vis_score_after_play <- with(events2010, ifelse(batting_team == 0, vis_score + all_runs_on_play, vis_score))
final.play <- (
  # HT ahead in mid of 9th or higher
  ((events2010$home_score_after_play > events2010$vis_score_after_play) & maybe.last.inning & third.out & !home.team.batting) |
  # HT ahead in 9th or higher
  ((events2010$vis_score_after_play < events2010$home_score_after_play) & maybe.last.inning & home.team.batting) |
  # VT ahead at end of 9th or higher
  ((events2010$vis_score_after_play > events2010$home_score_after_play) & maybe.last.inning & third.out & home.team.batting)
)
final.event.df <- subset(events2010[,], final.play)
final.event.df$score_diff <- final.event.df$home_score_after_play - final.event.df$vis_score_after_play
# how many games ended early?
other.games <- setdiff(
  unique(events2010[,c("game_id")]),
  final.event.df$game_id
)
length(other.games)
# 44 games ended early after 2010


# Central Limit Theorem: home field advantage
final <- final.event.df$score_diff
min(final)
max(final)
hist(final,
     xlim = c(-20, 20),
     breaks = 40,
     main = "Score diff",
     col="darkcyan",
     xlab="score diff",
     ylab="Freq")
median(final)
mean(final)
sd(final)


samples <- 1000
sample.size <- 5
xbar <- numeric(samples)
for (i in 1:samples) {
  xbar[i] <- mean(sample(final, size = sample.size, replace = TRUE))
}
hist(xbar,
     xlim = c(min(xbar), max(xbar)),
     breaks = 75,
     main = "Score diff (sample size=5)",
     col="darkcyan",
     xlab="score diff",
     ylab="Freq")
median(xbar)
mean(xbar)
sd(xbar)

samples <- 1000
sample.size <- 20
xbar <- numeric(samples)
for (i in 1:samples) {
  xbar[i] <- mean(sample(final, size = sample.size, replace = TRUE))
}
hist(xbar,
     xlim = c(min(xbar), max(xbar)),
     breaks = 75,
     main = "Score diff (sample size=20)",
     col="darkcyan",
     xlab="score diff",
     ylab="Freq")
median(xbar)
mean(xbar)
sd(xbar)

samples <- 1000
sample.size <- 100
xbar <- numeric(samples)
for (i in 1:samples) {
  xbar[i] <- mean(sample(final, size = sample.size, replace = TRUE))
}
hist(xbar,
     xlim = c(min(xbar), max(xbar)),
     breaks = 75,
     main = "Score diff (sample size=100)",
     col="darkcyan",
     xlab="score diff",
     ylab="Freq")
median(xbar)
mean(xbar)
sd(xbar)


# Sampling Methods: Ballpark HR analysis
# load 2010-present event data
load ("/Users/anthony.valencia/met/cs544/project/data/rdata/park_details.RData")

library(sampling)
N <- nrow(hr.events)
n <- 1000
set.seed(123)
srswor_sample <- srswor(n, N)
srswor_sample_ht <- rep(hr.events$home_team[srswor_sample != 0], srswor_sample[srswor_sample != 0])
hr.home.team.df.1000 <- as.data.frame(table(srswor_sample_ht))
colnames(hr.home.team.df.1000) = c("team.code", "hrs")

# merge ball park details with home run events
hr.home.team.df.1000 <- merge(
  x=hr.home.team.df.1000,
  y=park_details[,c("Team.Code","feet.to.center.field")],
  by.x="team.code",
  by.y="Team.Code",
  fill=-9999)

# HRs in each ballpark
ggplot(hr.home.team.df.1000) + 
  geom_bar(
    stat ="identity",
    aes(
      x=reorder(team.code,-hrs),
      y=hrs),
    fill="steelblue4",
    colour="steelblue1"
    ) +
  theme(
    axis.text.x=element_text(angle=90, hjust=1)
    ) + 
  labs(
    x="Ball Park",
    y="#HRs",
    title="Home Runs in each ballpark"
    )

library(scales)
# distance to center in each ballpark
ggplot(hr.home.team.df.1000) + 
  geom_bar(
    stat ="identity",
    aes(
      x=reorder(team.code,-hrs),
      y=feet.to.center.field),
    fill="steelblue4",
    colour="steelblue1"
  ) +
  theme(
    axis.text.x=element_text(angle=90, hjust=1)
  ) + 
  labs(
    x="Ball Park",
    y="Ft. to CF",
    title="Dist. to CF in each ballpark"
  ) + 
  scale_y_continuous(
    limits=c(
      min(hr.home.team.df.1000$feet.to.center.field),
      max(hr.home.team.df.1000$feet.to.center.field)
      ),
    oob = rescale_none
    )

ggplot(hr.home.team.df.1000) + 
  geom_bar(
    stat ="identity",
    aes(
      x=reorder(team.code,-feet.to.center.field),
      y=feet.to.center.field),
    fill="steelblue4",
    colour="steelblue1"
  ) +
  theme(
    axis.text.x=element_text(angle=90, hjust=1)
  ) + 
  labs(
    x="Ball Park",
    y="Ft. to CF",
    title="Dist. to CF in each ballpark"
  ) + 
  scale_y_continuous(
    limits=c(
      min(hr.home.team.df.1000$feet.to.center.field),
      max(hr.home.team.df.1000$feet.to.center.field)
    ),
    oob = rescale_none
  )


set.seed(113)
N
k <- floor(N / n)
k
r <- sample(k, 1)
r
# select every kth item
sq <- seq(r, by = k, length = n)
sq
srswor_sample_ht <- hr.events[sq, ]
hr.home.team.df.1000 <- as.data.frame(table(srswor_sample_ht$home_team))
colnames(hr.home.team.df.1000) = c("team.code", "hrs")

# merge ball park details with home run events
hr.home.team.df.1000 <- merge(
  x=hr.home.team.df.1000,
  y=park_details[,c("Team.Code","feet.to.center.field")],
  by.x="team.code",
  by.y="Team.Code",
  fill=-9999)

# HRs in each ballpark
ggplot(hr.home.team.df.1000) + 
  geom_bar(
    stat ="identity",
    aes(
      x=reorder(team.code,-hrs),
      y=hrs),
    fill="steelblue4",
    colour="steelblue1"
  ) +
  theme(
    axis.text.x=element_text(angle=90, hjust=1)
  ) + 
  labs(
    x="Ball Park",
    y="#HRs",
    title="Home Runs in each ballpark"
  )

# distance to center in each ballpark
ggplot(hr.home.team.df.1000) + 
  geom_bar(
    stat ="identity",
    aes(
      x=reorder(team.code,-hrs),
      y=feet.to.center.field),
    fill="steelblue4",
    colour="steelblue1"
  ) +
  theme(
    axis.text.x=element_text(angle=90, hjust=1)
  ) + 
  labs(
    x="Ball Park",
    y="Ft. to CF",
    title="Dist. to CF in each ballpark"
  ) + 
  scale_y_continuous(
    limits=c(
      min(hr.home.team.df.1000$feet.to.center.field),
      max(hr.home.team.df.1000$feet.to.center.field)
    ),
    oob = rescale_none
  )

ggplot(hr.home.team.df.1000) + 
  geom_bar(
    stat ="identity",
    aes(
      x=reorder(team.code,-feet.to.center.field),
      y=feet.to.center.field),
    fill="steelblue4",
    colour="steelblue1"
  ) +
  theme(
    axis.text.x=element_text(angle=90, hjust=1)
  ) + 
  labs(
    x="Ball Park",
    y="Ft. to CF",
    title="Dist. to CF in each ballpark"
  ) + 
  scale_y_continuous(
    limits=c(
      min(hr.home.team.df.1000$feet.to.center.field),
      max(hr.home.team.df.1000$feet.to.center.field)
    ),
    oob = rescale_none
  )

# Confidence Levels: score difference (home-away)
conf <- c(80, 90)
conf

alpha <- 1 - conf/100
alpha

# lower tail z-score
qnorm(alpha/2)

# upper tail z-score
qnorm(1 - alpha/2)

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), z: %.2f , %.2f",
                 100*(1-i), i, 
                 qnorm(i/2),
                 qnorm(1-i/2))
  cat(str,"\n")
}

final.sd <- sd(final)
final.sd

sample.size <- 10
sample.data <- sample(final, size=sample.size)
sample.data

sd.sample.means <- final.sd/sqrt(sample.size)
sd.sample.means

xbar <- mean(sample.data)
xbar

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = (%.2f, %.2f)",
                 100*(1-i), i, 
                 xbar - qnorm(1-i/2) * sd.sample.means,
                 xbar + qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

sample.size <- 100
sample.data <- sample(final, size=sample.size)
sample.data

sd.sample.means <- final.sd/sqrt(sample.size)
sd.sample.means

xbar <- mean(sample.data)
xbar

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = (%.2f, %.2f)",
                 100*(1-i), i, 
                 xbar - qnorm(1-i/2) * sd.sample.means,
                 xbar + qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

sample.size <- 1000
sample.data <- sample(final, size=sample.size)
sample.data

sd.sample.means <- final.sd/sqrt(sample.size)
sd.sample.means

xbar <- mean(sample.data)
xbar

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = (%.2f, %.2f)",
                 100*(1-i), i, 
                 xbar - qnorm(1-i/2) * sd.sample.means,
                 xbar + qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

