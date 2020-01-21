# Set the correct path to your SPARK installation
if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/Users/skalathur/MyApps/spark")
}

Sys.setenv(SPARK_LOCAL_IP="localhost")


library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

sparkR.session(master = "local[*]", sparkConfig = list(spark.driver.memory = "2g"))

# Set the correct path for the dataset
inputFile <- "/temp/datasets/usa_daily_avg_temps.csv"

usaDailyTemps <- read.df(inputFile, source = "csv", 
                         header='true', 
                         inferSchema='true')

usaDailyTemps

printSchema(usaDailyTemps)

count(usaDailyTemps)

head(usaDailyTemps)

maxAvgTemp <- summarize(usaDailyTemps, max(usaDailyTemps$avgtemp))
maxAvgTemp

count(maxAvgTemp)

collect(maxAvgTemp)

maxAvgTemp <- summarize(usaDailyTemps, MaxValue = max(usaDailyTemps$avgtemp))
maxAvgTemp

localDf <- collect(maxAvgTemp)
localDf

maxData <- filter(usaDailyTemps, usaDailyTemps$avgtemp == localDf[1, 'MaxValue'])
maxData

collect(maxData)

##


maxTempByYear <- summarize(groupBy(usaDailyTemps, usaDailyTemps$Year), 
                           MaxValue = max(usaDailyTemps$avgtemp))
maxTempByYear
count(maxTempByYear)
collect(maxTempByYear)

arrange(maxTempByYear, maxTempByYear$Year)
collect(arrange(maxTempByYear, maxTempByYear$Year))


maxTempByState <- summarize(groupBy(usaDailyTemps, usaDailyTemps$State), 
                           MaxValue = max(usaDailyTemps$avgtemp))
maxTempByState
count(maxTempByState)
collect(maxTempByState)

arrange(maxTempByState, maxTempByState$State)
collect(arrange(maxTempByState, maxTempByState$State))



stateCounts <- summarize(groupBy(usaDailyTemps, usaDailyTemps$state), 
                         count = n(usaDailyTemps$state))

collect(arrange(stateCounts, desc(stateCounts$count)))

stateCityCounts <- summarize(groupBy(usaDailyTemps, usaDailyTemps$state, usaDailyTemps$city), 
                             count = n(usaDailyTemps$state))

collect(arrange(stateCityCounts, asc(stateCityCounts$state)))

collect(arrange(stateCityCounts, asc(stateCityCounts$state),
                asc(stateCityCounts$city)))

# Number of cities for each state
collect(summarize(groupBy(stateCityCounts, stateCityCounts$state), 
                  count = n(stateCityCounts$state)))

##

bostonDailyTemps <- subset(usaDailyTemps, usaDailyTemps$city == 'Boston')
count(bostonDailyTemps)

bostonAvgTempsByYear <- summarize(groupBy(bostonDailyTemps, bostonDailyTemps$Year), 
                               Average = avg(bostonDailyTemps$avgtemp))
collect(
  arrange(bostonAvgTempsByYear, bostonAvgTempsByYear$Year)
  )

bostonAvgTempsByMonth <- summarize(groupBy(bostonDailyTemps, bostonDailyTemps$Month), 
                                  Average = avg(bostonDailyTemps$avgtemp))
collect(
  arrange(bostonAvgTempsByMonth, bostonAvgTempsByMonth$Month)
)

bostonAvgTempsByYearAndMonth <- summarize(groupBy(bostonDailyTemps, bostonDailyTemps$Year, bostonDailyTemps$Month), 
                                  Average = avg(bostonDailyTemps$avgtemp))
collect(
  arrange(bostonAvgTempsByYearAndMonth, bostonAvgTempsByYearAndMonth$Year, bostonAvgTempsByYearAndMonth$Month)
)


bostonYears <- select(bostonDailyTemps, 'year')
distinctBostonYears <- distinct(bostonYears)
yearsDF <- collect(distinct(bostonYears))
yearsDF
yearsDF[order(yearsDF$year), ]

# Stop the SparkSession now
sparkR.session.stop()


