library(googleVis)
library(SportsAnalytics)

# 2-1) Retrieve the NBA data for the 13-14 season.
nba1314 <- fetch_NBAPlayerStatistics("13-14")

# 2-2) Which player has the best field point percentage?
nba1314$FieldGoalPercentage <- nba1314$FieldGoalsMade/nba1314$FieldGoalsAttempted
fg.percentage.not.na <- nba1314[!is.na(nba1314$FieldGoalPercentage),]
fg.percentage.not.na[fg.percentage.not.na[, "FieldGoalPercentage"] == max(fg.percentage.not.na$FieldGoalPercentage, na.rm=TRUE),][,c("Name","FieldGoalPercentage","FieldGoalsMade","FieldGoalsAttempted")]

# 2-3) Which player has the best free throw percentage?
nba1314$FreeThrowPercentage <- nba1314$FreeThrowsMade/nba1314$FreeThrowsAttempted
ft.percentage.not.na <- nba1314[!is.na(nba1314$FreeThrowPercentage),]
ft.percentage.not.na[ft.percentage.not.na[, "FreeThrowPercentage"] == max(ft.percentage.not.na$FreeThrowPercentage, na.rm=TRUE),][,c("Name","FreeThrowPercentage","FreeThrowsMade","FreeThrowsAttempted")]

# 2-4) Which player has the best three point percentage?
nba1314$ThreePercentage <- nba1314$ThreesMade/nba1314$ThreesAttempted
three.percentage.not.na <- nba1314[!is.na(nba1314$ThreePercentage),]
three.percentage.not.na[three.percentage.not.na[, "ThreePercentage"] == max(three.percentage.not.na$ThreePercentage, na.rm=TRUE),][,c("Name","ThreePercentage","ThreesMade","ThreesAttempted")]

# 2-5) Do you suspect any error in the TotalPoints column in the dataset?
nba1314$TotalPointsCalculated <- (nba1314$FreeThrowsMade + (2*nba1314$FieldGoalsMade) + (3*nba1314$ThreesMade))
nba1314[order(nba1314$TotalPoints),c("Name","TotalPoints","TotalPointsCalculated")]
nrow(nba1314[nba1314$TotalPoints != nba1314$TotalPointsCalculated,][,c("Name","Team","TotalPoints","TotalPointsCalculated")])
# 348 players do not have the same points as calculated by formula.  error suspected.  are total points including post-season?
nrow(nba1314[nba1314$TotalPoints == nba1314$TotalPointsCalculated,][,c("Name","Team","TotalPoints","TotalPointsCalculated")])
# 134 players do have the same points as calculated by formula.

# 2-6) Show the top 10 players in terms of TotalPoints, arranged from the highest to lowest.
head(nba1314[order(nba1314$TotalPoints, decreasing = TRUE),c("Name","TotalPoints")], 10)

# 2-7) Use at least 5 Google charts (your choice) to show relevant data from this dataset.
# (2-7-a) column chart of field goal percentage by position
field.goal.percentage.by.pos.df <- aggregate(
  nba1314$FieldGoalPercentage,
  by=list(Category=nba1314$Position),
  FUN=mean,
  na.rm=TRUE,
  na.action=NULL
  )
names(field.goal.percentage.by.pos.df) <- c("POS", "FG%")
field.goal.percentage.by.pos.column <- gvisColumnChart(field.goal.percentage.by.pos.df)
plot(field.goal.percentage.by.pos.column)

# (2-7-b) stepped chart of all types of shots made by position
shots.made.by.pos.df <- aggregate(
  list(nba1314$FieldGoalsMade, nba1314$ThreesMade, nba1314$FreeThrowsMade),
  by=list(Category=nba1314$Position),
  FUN=mean,
  na.rm=TRUE,
  na.action=NULL
  )
names(shots.made.by.pos.df) <- c("POS", "FG avg", "3s avg", "FT avg")
shots.made.by.pos.stepped.area <- gvisSteppedAreaChart(
  shots.made.by.pos.df,
  xvar="POS",
  yvar=c("3s avg", "FT avg", "FG avg"),
  options=list(isStacked=TRUE)
  )
plot(shots.made.by.pos.stepped.area)

# (2-7-c) bar chart of all types of shots made by team
shots.made.by.team.df <- aggregate(
  list(nba1314$FieldGoalsMade, nba1314$ThreesMade, nba1314$FreeThrowsMade),
  by=list(Category=nba1314$Team),
  FUN=mean,
  na.rm=TRUE,
  na.action=NULL
)
names(shots.made.by.team.df) <- c("Team", "FG avg", "3s avg", "FT avg")
shots.made.by.team.bar <- gvisBarChart(
  shots.made.by.team.df,
  xvar="Team",
  yvar=c("3s avg", "FT avg", "FG avg"),
  options=list(height=1500)
)
plot(shots.made.by.team.bar)

# (2-7-d) bar chart of all types of shots made by team
bad.behavior.by.team.df <- aggregate(
  list(nba1314$Technicals, nba1314$PersonalFouls),
  by=list(Category=nba1314$Team),
  FUN=sum,
  na.rm=TRUE,
  na.action=NULL
)
names(bad.behavior.by.team.df) <- c("Team", "Technicals", "PersonalFouls")
bad.behavior.by.team.df <- bad.behavior.by.team.df[bad.behavior.by.team.df$Team != "NA",]
bad.behavior.by.team.line <- gvisLineChart(
  bad.behavior.by.team.df,
  "Team",
  c("Technicals", "PersonalFouls"),
  options=list(
    series="[{targetAxisIndex: 0},
    {targetAxisIndex:1}]",
    vAxes="[{title:'Technicals'}, {title:'PersonalFouls'}]"
  )
)
plot(bad.behavior.by.team.line)

# (2-7-e) pie chart of total 3s by position
nba1314$threes.made.fraction <- nba1314$ThreesMade/sum(nba1314$ThreesMade)
threes.made.fraction.by.pos.df <- aggregate(
  nba1314$threes.made.fraction,
  by=list(Category=nba1314$Position),
  FUN=sum,
  na.rm=TRUE,
  na.action=NULL
)
names(threes.made.fraction.by.pos.df) <- c("POS", "3s%")
threes.made.fraction.by.pos.pie <- gvisPieChart(threes.made.fraction.by.pos.df)
plot(threes.made.fraction.by.pos.pie)
