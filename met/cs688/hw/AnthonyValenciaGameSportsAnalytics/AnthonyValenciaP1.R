library(googleVis)
library(RCurl)
library(RJSONIO)

load(file="glitch1.RData")

nodes.info <- do.call(
  "rbind",
  lapply(data$nodes, data.frame)
  )

# 1-1) Using the aggregate function, compute the data frame for the total players joining each month. Name the columns as Month and Joining.
joining.by.month.df <- aggregate(
  nodes.info$joining, 
  by=list(nodes.info$month), 
  FUN = sum
  )
joining.by.month.df <- cbind(
  joining.by.month.df[-ncol(joining.by.month.df)],
  joining.by.month.df[[ncol(joining.by.month.df)]]
  )
names(joining.by.month.df) <- c("Month", "Joining")
joining.by.month.df


# 1-2) Using the aggregate function, compute the data frame for the total players departing each month. Names the columns as Month and Departing.
departing.by.month.df <- aggregate(
  nodes.info$departing, 
  by=list(nodes.info$month), 
  FUN = sum
)
departing.by.month.df <- cbind(
  departing.by.month.df[-ncol(departing.by.month.df)],
  departing.by.month.df[[ncol(departing.by.month.df)]]
)
names(departing.by.month.df) <- c("Month", "Departing")
departing.by.month.df


# 1-3) Merge the two data frames by Month column with sort option as FALSE.
join.depart.by.month.df <- merge(
  joining.by.month.df, 
  departing.by.month.df, 
  by = "Month",
  sort = FALSE
  )
join.depart.by.month.df


# 1-4) Show month-by-month comparison of the above numbers using the Google Line chart and Google Column chart. Merge the two into a single chart.
join.depart.by.month.line <- gvisLineChart(join.depart.by.month.df)
join.depart.by.month.column <- gvisColumnChart(join.depart.by.month.df)
join.depart.by.month.final <- gvisMerge(
  join.depart.by.month.line,
  join.depart.by.month.column
)

plot(join.depart.by.month.final)


# 1-5) Show the Google Gauge chart with default options for the monthly departing data. Use the range from 0 to 4030.
depart.by.month.gauge <- gvisGauge(departing.by.month.df, options = list(min=0, max=4030))
plot(depart.by.month.gauge)


# 1-6) Show the Google Gauge chart for the monthly departing data with the green range 0 – 1000, yellow range 1000 – 2000, and the red range 2000 – 4030.
depart.by.month.gauge.colors <- gvisGauge(
  departing.by.month.df,
  options = list(
    min=0,
    max=4030,
    greenFrom=0,
    greenTo=1000,
    yellowFrom=1000,
    yellowTo=2000,
    redFrom=2000,
    redTo=4030
  )
  )
plot(depart.by.month.gauge.colors)

