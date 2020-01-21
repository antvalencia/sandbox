#### Basketball Championship Data -----
library(googleVis)
library(XML)
library(stringr)
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


# 1) How many times was the series swept, i.e., decided by the series score 4-0?
nrow(LoB.data[LoB.data$Series == '4-0',])

# 2) How many times was the series decided by game 7?  (Series score 4-3)
nrow(LoB.data[LoB.data$Series == '4-3',])


# 3) Show 5 teams that have the most wins in descending order.
winners.df <- as.data.frame(table(LoB.data$Winner))
names(winners.df) <- c("team", "championships")
head(winners.df[order(winners.df$championships, decreasing = TRUE),], 5)$team
