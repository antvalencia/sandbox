# Module 6 - Sports Analytics

#### Basketball Data Analytics ---- 


# setwd("~/MyCourses/CS688/Samples/Module6")

library(googleVis)

library(SportsAnalytics)

nba0708 <- fetch_NBAPlayerStatistics("07-08")

save(nba0708, file="nba0708.RData")

load(file="nba0708.RData")

names(nba0708)

nba0708.bos <- subset(nba0708, Team == 'BOS')

chart1 <- gvisTable(nba0708.bos[,c(2,4:12)])
plot(chart1)

print(chart1, file = "chart1.html")


chart2 <- 
  gvisColumnChart(
    nba0708.bos,
    xvar = "Name",
    yvar = c("FieldGoalsMade", "FieldGoalsAttempted"),
    options=list(
      legend="top",
      height=500, width=850))
plot(chart2)

print(chart2, file = "chart2.html")



#### Basketball Championship Data -----
library(googleVis); library(XML); library(stringr)
library(httr)

webpage <- paste0("http://www.landofbasketball.com/", "championships/year_by_year.htm")
# data <- readHTMLTable(webpage,which = 1, stringsAsFactors = FALSE)
LoB.temp.data <- readHTMLTable(rawToChar(GET(webpage)$content), header = T, stringsAsFactors = F)
LoB.temp.data <- as.character(unlist(LoB.temp.data))

split.data <- strsplit(LoB.temp.data, split='(\n|\t)')

Year <- sapply(split.data, FUN = function (x) substr(x[[1]][1], 3, 7))
Winner <- sapply(split.data,FUN = function (x) str_trim(x[[7]]))
Series <- sapply(split.data,FUN = function (x) str_trim(x[[11]]))
Opponent <- sapply(split.data,FUN = function (x) str_trim(x[[13]]))


LoB.data <- data.frame(Year=Year, Winner= Winner, Series=Series, Opponent=Opponent)

chart3 <- gvisTable(LoB.data,
                    options=list(
                      height=500))
plot(chart3)








