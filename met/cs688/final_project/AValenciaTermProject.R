library(googleVis)
library(SportsAnalytics)


# Sports Data Analytics term project. Suggested Dataset: The websites:
#   http://www.landofbasketball.com/championships/year_by_year.htm
# http://www.landofbasketball.com/world_cup_stats/medals_by_year.htm
# • Use the SportsAnalytics API for R, to accomplish several sports analytics tasks.
# • Retrieve the NBA data for the 2007-2008 season.
nba0708 <- fetch_NBAPlayerStatistics("07-08")
names(nba0708)
# • Subset the data for your favorite team. Show the code you used to find:
nba0708.lakers <- nba0708[nba0708$Team == 'LAL',]
nba0708.lakers$Name
#   • Which player has the best three point percentage?
nba0708.lakers$ThreePercentage <- nba0708.lakers$ThreesMade/nba0708.lakers$ThreesAttempted
three.percentage.lakers.not.na <- nba0708.lakers[!is.na(nba0708.lakers$ThreePercentage),]
three.percentage.lakers.not.na[three.percentage.lakers.not.na[, "ThreePercentage"] == max(three.percentage.lakers.not.na$ThreePercentage, na.rm=TRUE),][,c("Name","ThreePercentage","ThreesMade","ThreesAttempted")]
#   • Which player has played the largest number of minutes?
minutes.played.lakers.not.na <- nba0708.lakers[!is.na(nba0708.lakers$TotalMinutesPlayed),]
minutes.played.lakers.not.na[minutes.played.lakers.not.na[, "TotalMinutesPlayed"] == max(minutes.played.lakers.not.na$TotalMinutesPlayed, na.rm=TRUE),][,c("Name","TotalMinutesPlayed")]
#   • Which player has the most "Steals"?
steals.lakers.not.na <- nba0708.lakers[!is.na(nba0708.lakers$Steals),]
steals.lakers.not.na[steals.lakers.not.na[, "Steals"] == max(steals.lakers.not.na$Steals, na.rm=TRUE),][,c("Name","Steals")]
#   • Show 5 teams for the 2007-2008 season that have the most wins in descending order.
# https://www.landofbasketball.com/yearbyyear/2007_2008_standings.htm
# see below


# 1) points per player per team
games.played.by.team.df <- aggregate(
  nba0708$GamesPlayed,
  by=list(Category=nba0708$Team),
  FUN=sum,
  na.rm=TRUE,
  na.action=NULL
)
names(games.played.by.team.df) <- c("team", "games.played.all.players")
total.points.by.team.df <- aggregate(
  nba0708$TotalPoints,
  by=list(Category=nba0708$Team),
  FUN=sum,
  na.rm=TRUE,
  na.action=NULL
)
names(total.points.by.team.df) <- c("team", "total.points.all.players")
points.per.game.by.team.df <- merge(
  games.played.by.team.df,
  total.points.by.team.df,
  by.x="team",
  by.y="team"
)
points.per.game.by.team.df$points.per.game.per.player <- points.per.game.by.team.df$total.points.all.players/points.per.game.by.team.df$games.played.all.players
points.per.game.by.team.column <- gvisColumnChart(points.per.game.by.team.df[order(points.per.game.by.team.df$points.per.game.per.player),][,c("team", "points.per.game.per.player")])
plot(points.per.game.by.team.column)
print(points.per.game.by.team.column, file = "points.per.game.by.team.column.html")


# 2) compare points per game per player (team) to offensive rebounds per game per player (team)
offensive.rebounds.by.team.df <- aggregate(
  nba0708$OffensiveRebounds,
  by=list(Category=nba0708$Team),
  FUN=sum,
  na.rm=TRUE,
  na.action=NULL
)
names(offensive.rebounds.by.team.df) <- c("team", "offensive.rebounds.all.players")
offensive.rebounds.per.game.by.team.df <- merge(
  games.played.by.team.df,
  offensive.rebounds.by.team.df,
  by.x="team",
  by.y="team"
)
offensive.rebounds.per.game.by.team.df$offensive.rebounds.per.game.per.player <- offensive.rebounds.per.game.by.team.df$offensive.rebounds.all.players/offensive.rebounds.per.game.by.team.df$games.played.all.players

offensive.rebounds.total.points.by.team.df <- merge(
  offensive.rebounds.per.game.by.team.df,
  points.per.game.by.team.df,
  by.x="team",
  by.y="team"
)

points.vs.rebounds.line <- gvisLineChart(
  offensive.rebounds.total.points.by.team.df,
  "team",
  c("offensive.rebounds.per.game.per.player","points.per.game.per.player"),
  options=list(
    series="[{targetAxisIndex: 0},
    {targetAxisIndex:1}]",
    vAxes="[{title:'offensive.rebounds'}, {title:'points'}]"
  )
)
plot(points.vs.rebounds.line)
print(points.per.game.by.team.column, file = "points.vs.rebounds.line.html")


# 3) compare points per game per player (team) to steals per game per player (team)
steals.by.team.df <- aggregate(
  nba0708$Steals,
  by=list(Category=nba0708$Team),
  FUN=sum,
  na.rm=TRUE,
  na.action=NULL
)
names(steals.by.team.df) <- c("team", "steals.all.players")
steals.per.game.by.team.df <- merge(
  games.played.by.team.df,
  steals.by.team.df,
  by.x="team",
  by.y="team"
)
steals.per.game.by.team.df$steals.per.game.per.player <- steals.per.game.by.team.df$steals.all.players/steals.per.game.by.team.df$games.played.all.players

steals.total.points.by.team.df <- merge(
  steals.per.game.by.team.df,
  points.per.game.by.team.df,
  by.x="team",
  by.y="team"
)

points.vs.steals.line <- gvisLineChart(
  steals.total.points.by.team.df,
  "team",
  c("steals.per.game.per.player","points.per.game.per.player"),
  options=list(
    series="[{targetAxisIndex: 0},
    {targetAxisIndex:1}]",
    vAxes="[{title:'steals'}, {title:'points'}]"
  )
)
plot(points.vs.steals.line)
print(points.per.game.by.team.column, file = "points.vs.steals.line.html")

# 4) compare points per game per player (team) to turnovers per game per player (team)
turnovers.by.team.df <- aggregate(
  nba0708$Turnovers,
  by=list(Category=nba0708$Team),
  FUN=sum,
  na.rm=TRUE,
  na.action=NULL
)
names(turnovers.by.team.df) <- c("team", "turnovers.all.players")
turnovers.per.game.by.team.df <- merge(
  games.played.by.team.df,
  turnovers.by.team.df,
  by.x="team",
  by.y="team"
)
turnovers.per.game.by.team.df$turnovers.per.game.per.player <- turnovers.per.game.by.team.df$turnovers.all.players/turnovers.per.game.by.team.df$games.played.all.players

turnovers.total.points.by.team.df <- merge(
  turnovers.per.game.by.team.df,
  points.per.game.by.team.df,
  by.x="team",
  by.y="team"
)

points.vs.turnovers.line <- gvisLineChart(
  turnovers.total.points.by.team.df,
  "team",
  c("turnovers.per.game.per.player","points.per.game.per.player"),
  options=list(
    series="[{targetAxisIndex: 0},
    {targetAxisIndex:1}]",
    vAxes="[{title:'turnovers'}, {title:'points'}]"
  )
)
plot(points.vs.turnovers.line)
print(points.per.game.by.team.column, file = "points.vs.turnovers.line.html")

points.vs.all <- gvisMerge(
  gvisMerge(
    points.vs.rebounds.line,
    points.vs.steals.line,
    horizontal=FALSE),
  points.vs.turnovers.line,
  horizontal=FALSE
)

plot(points.vs.all)
print(points.per.game.by.team.column, file = "points.vs.all.html")


# 5) see below

library(googleVis); library(XML); library(stringr)
library(httr)

# get data
webpage <- paste0("http://www.landofbasketball.com/", "yearbyyear/2007_2008_standings.htm")
LoB.temp.data <- readHTMLTable(rawToChar(GET(webpage)$content), header = T, stringsAsFactors = F)
LoB.temp.data[1]
# preprocessing
west <- as.character(unlist(LoB.temp.data[1]))
west.df <- as.data.frame(
  list(
    west[1:15],
    west[16:30],
    west[31:45],
    west[46:60],
    west[61:75],
    west[76:90],
    west[91:105]
    )
  )
names(west.df) <- c("idx", "team", "w", "l", "pct", "gb", "?")
east <- as.character(unlist(LoB.temp.data[2]))
east.df <- as.data.frame(
  list(
    east[1:15],
    east[16:30],
    east[31:45],
    east[46:60],
    east[61:75],
    east[76:90],
    east[91:105]
  )
)
names(east.df) <- c("idx", "team", "w", "l", "pct", "gb", "?")
# concatenate east/west data frames
team.summary.0708.df <- rbind(east.df, west.df)
# remove extraneous columns
drops <- c("idx", "?")
team.summary.0708.df <- team.summary.0708.df[ , !(names(team.summary.0708.df) %in% drops)]
# column type correction: proper types instead of factors
team.summary.0708.df$team <- as.character(team.summary.0708.df$team)
team.summary.0708.df$w <- as.integer(as.character(team.summary.0708.df$w))
team.summary.0708.df$l <- as.integer(as.character(team.summary.0708.df$l))
team.summary.0708.df$gb <- as.integer(as.character(team.summary.0708.df$gb))
team.summary.0708.df$pct <- as.numeric(as.character(team.summary.0708.df$pct))
# top 5 teams
head(team.summary.0708.df[order(team.summary.0708.df$w, decreasing = TRUE),], 5)[,c("team", "w")]

# • Use at least 5 Google charts (your choice) to show relevant data from this dataset.
# • Use gvisGeoChart function to display the location on the world map all of the Basketball World Cup
# Champion countries that you can find at:
#   http://www.landofbasketball.com/world_cup_stats/medals_by_year.htm

# get data
webpage <- paste0("http://www.landofbasketball.com/", "world_cup_stats/medals_by_year.htm")
LoB.temp.data <- readHTMLTable(rawToChar(GET(webpage)$content), header = T, stringsAsFactors = F)
LoB.temp.data
# preprocessing
medals.by.year.df = as.data.frame(LoB.temp.data)
colnames(medals.by.year.df) = medals.by.year.df[1, ]
medals.by.year.df <- medals.by.year.df[seq(from=3, to=35, by=2),]

# correct data labels for Gold
# United States instead of USA
medals.by.year.df$Gold[medals.by.year.df$Gold == "USA"] <- "United States"
# Russia instead of Soviet Union
medals.by.year.df$Gold[medals.by.year.df$Gold == "Soviet Union"] <- "RU"
# Serbia instead of FR of Yugoslavia
medals.by.year.df$Gold[medals.by.year.df$Gold == "FR of Yugoslavia"] <- "Serbia"
# Serbia instead of Yugoslavia
medals.by.year.df$Gold[medals.by.year.df$Gold == "Yugoslavia"] <- "Serbia"

gold.medals.country.freq.df <- as.data.frame(table(medals.by.year.df$Gold))
names(gold.medals.country.freq.df) <- c("country", "golds")
gold.medals.country.freq.df$country <- as.character(gold.medals.country.freq.df$country)

# plot data
geo.gold <- gvisGeoChart(
  gold.medals.country.freq.df,
  locationvar="country", 
  colorvar="golds"
)
plot(geo.gold)
print(geo.gold, file = "geo.gold.html")

# ###################################
# correct data labels for Silver
# United States instead of USA
medals.by.year.df$Silver[medals.by.year.df$Silver == "USA"] <- "United States"
# Russia instead of Soviet Union
medals.by.year.df$Silver[medals.by.year.df$Silver == "Soviet Union"] <- "RU"
# Serbia instead of FR of Yugoslavia
medals.by.year.df$Silver[medals.by.year.df$Silver == "FR of Yugoslavia"] <- "Serbia"
# Serbia instead of Yugoslavia
medals.by.year.df$Silver[medals.by.year.df$Silver == "Yugoslavia"] <- "Serbia"

silver.medals.country.freq.df <- as.data.frame(table(medals.by.year.df$Silver))
names(silver.medals.country.freq.df) <- c("country", "silvers")
silver.medals.country.freq.df$country <- as.character(silver.medals.country.freq.df$country)

# plot data
geo.silver <- gvisGeoChart(
  silver.medals.country.freq.df,
  locationvar="country", 
  colorvar="silvers"
)
plot(geo.silver)
print(geo.silver, file = "geo.silver.html")

# ###################################
# correct data labels for Bronze
# United States instead of USA
medals.by.year.df$Bronze[medals.by.year.df$Bronze == "USA"] <- "United States"
# Russia instead of Soviet Union
medals.by.year.df$Bronze[medals.by.year.df$Bronze == "Soviet Union"] <- "RU"
# Serbia instead of FR of Yugoslavia
medals.by.year.df$Bronze[medals.by.year.df$Bronze == "FR of Yugoslavia"] <- "Serbia"
# Serbia instead of Yugoslavia
medals.by.year.df$Bronze[medals.by.year.df$Bronze == "Yugoslavia"] <- "Serbia"

bronze.medals.country.freq.df <- as.data.frame(table(medals.by.year.df$Bronze))
names(bronze.medals.country.freq.df) <- c("country", "bronzes")
bronze.medals.country.freq.df$country <- as.character(bronze.medals.country.freq.df$country)

# plot data
geo.bronze <- gvisGeoChart(
  bronze.medals.country.freq.df,
  locationvar="country", 
  colorvar="bronzes"
)
plot(geo.bronze)
print(geo.bronze, file = "geo.bronze.html")

geo.all <- gvisMerge(
  gvisMerge(
    geo.gold,
    geo.silver,
    horizontal=FALSE),
  geo.bronze,
  horizontal=FALSE
  )

plot(geo.all)
print(geo.all, file = "geo.all.html")

# resources:
#   https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html
