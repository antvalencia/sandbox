if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/Users/skalathur/MyApps/spark")
}

Sys.setenv(SPARK_LOCAL_IP="localhost")


library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

sparkR.session(master = "local[*]", sparkConfig = list(spark.driver.memory = "2g"))

inputFile <- "/temp/datasets/usa_zipcodes.json"

usaZipCodes <- read.df(inputFile, source = "json", 
                         inferSchema='true')

usaZipCodes

printSchema(usaZipCodes)

count(usaZipCodes)

head(usaZipCodes)

persist(usaZipCodes, "MEMORY_AND_DISK")

createOrReplaceTempView(usaZipCodes, "usaZipCodesTable")

query <- "SELECT * FROM usaZipCodesTable WHERE pop > 100"
query

usaZipCodes <- sql(query)
usaZipCodes

createOrReplaceTempView(usaZipCodes, "usaZipCodesTable")

query <- "SELECT max(pop) as MaxPop, min(pop) as MinPop from usaZipCodesTable"
query

maxAndMin <- sql(query)
maxAndMin

localDf <- collect(maxAndMin)
localDf

# zip codes in each state

query <- "SELECT state, count(*) as Count FROM usaZipCodesTable GROUP BY state"
query

zipCodesByState <- sql(query)
zipCodesByState

count(zipCodesByState)
collect(zipCodesByState)
collect(arrange(zipCodesByState, zipCodesByState$state))
collect(arrange(zipCodesByState, desc(zipCodesByState$Count)))

# 10 Most populous zip codes

arrange(usaZipCodes, desc(usaZipCodes$pop))
head(arrange(usaZipCodes, desc(usaZipCodes$pop)), n = 10)

# or,
collect(sql("SELECT * FROM usaZipCodesTable ORDER BY pop DESC LIMIT 10"))

# Most populous states

query <- "SELECT state, sum(pop) as TotalPop FROM usaZipCodesTable GROUP BY state ORDER BY TotalPop DESC"
query

popByState <- sql(query)
popByState

count(popByState)
collect(popByState)

# Stop the SparkSession now
sparkR.session.stop()
