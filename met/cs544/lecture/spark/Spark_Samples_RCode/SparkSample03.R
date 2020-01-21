# SparkSQL

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/Users/skalathur/MyApps/spark")
}

Sys.setenv(SPARK_LOCAL_IP="localhost")


library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

sparkR.session(master = "local[*]", sparkConfig = list(spark.driver.memory = "2g"))

inputFile <- "/temp/datasets/usa_daily_avg_temps.csv"

usaDailyTemps <- read.df(inputFile, source = "csv", 
                         header='true', 
                         inferSchema='true')

usaDailyTemps

printSchema(usaDailyTemps)

count(usaDailyTemps)

head(usaDailyTemps)

persist(usaDailyTemps, "MEMORY_AND_DISK")

# Register this DataFrame as a table.
createOrReplaceTempView(usaDailyTemps, "usaDailyTempsTable")

query <- "SELECT max(avgtemp) FROM usaDailyTempsTable"
query

maxAvgTemp <- sql(query)
count(maxAvgTemp)

collect(maxAvgTemp)

query <- "SELECT max(avgtemp) AS MaxValue FROM usaDailyTempsTable"
maxAvgTemp <- sql(query)
maxAvgTemp

localDf <- collect(maxAvgTemp)
localDf

query <- paste("SELECT * from usaDailyTempsTable WHERE avgtemp = ", localDf[1, 'MaxValue'])
query

maxData <- sql(query)
maxData

collect(maxData)


# Aggregate to find the maximum of avgtemp grouping by Year

query <- "SELECT year, max(avgtemp) AS MaxValue FROM usaDailyTempsTable GROUP BY year"
query

maxTempByYear <- sql(query)
maxTempByYear
count(maxTempByYear)
collect(maxTempByYear)

query <- "SELECT year, max(avgtemp) AS MaxValue FROM usaDailyTempsTable GROUP BY year ORDER BY year"
query

maxTempByYear <- sql(query)
maxTempByYear
count(maxTempByYear)
collect(maxTempByYear)

# Aggregate to find the maximum of avgtemp grouping by State

query <- "SELECT state, max(avgtemp) AS MaxValue FROM usaDailyTempsTable GROUP BY state ORDER BY state"
query

maxTempByState <- sql(query)
maxTempByState
count(maxTempByState)
collect(maxTempByState)

# Aggregate to find the number of entries grouping by State

query <- "SELECT state, count(*) AS count FROM usaDailyTempsTable GROUP BY state ORDER BY count DESC"
query

stateCounts <- sql(query)

collect(stateCounts)

# Aggregate to find the number of entries grouping by State and City

query <- "SELECT state, city, count(*) AS count FROM usaDailyTempsTable GROUP BY state, city ORDER BY state, city"
query


stateCityCounts <- sql(query)
stateCityCounts

collect(stateCityCounts)

#
# Number of cities for each state in the dataset

createOrReplaceTempView(stateCityCounts, "stateCityCountsTable")

query <- "SELECT state, count(*) AS count FROM stateCityCountsTable GROUP BY state ORDER BY state"
query

collect(sql(query))

## Create a subset SparkDataFrame for Boston


bostonDailyTemps <- sql("SELECT * FROM usaDailyTempsTable WHERE city == 'Boston'")
count(bostonDailyTemps)

createOrReplaceTempView(bostonDailyTemps, "bostonDailyTempsTable")

#### Boston Average Temperatures By Year

query <- "SELECT year, avg(avgtemp) AS Average FROM bostonDailyTempsTable GROUP BY year ORDER BY year"
query

bostonAvgTempsByYear <- sql(query)
bostonAvgTempsByYear

collect(bostonAvgTempsByYear)

#### Boston Average Temperatures By Month

query <- "SELECT month, avg(avgtemp) AS Average FROM bostonDailyTempsTable GROUP BY month ORDER BY month"
query

bostonAvgTempsByMonth <- sql(query)
bostonAvgTempsByMonth

collect(bostonAvgTempsByMonth)

#### Boston Average Temperatures By Year and Month

query <- "SELECT year, month, avg(avgtemp) AS Average FROM bostonDailyTempsTable GROUP BY year, month ORDER BY year, month"
query

bostonAvgTempsByYearAndMonth <- sql(query)
bostonAvgTempsByYearAndMonth

collect(bostonAvgTempsByYearAndMonth)

#### Boston years in data

query <- "SELECT distinct(year) FROM bostonDailyTempsTable ORDER BY year"
query

yearsDF <- collect(sql(query))
yearsDF

# Stop the SparkSession now
sparkR.session.stop()

