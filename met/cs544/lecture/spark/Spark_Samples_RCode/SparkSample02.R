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

collect(subset(usaZipCodes, usaZipCodes$pop <= 100))

usaZipCodes <- subset(usaZipCodes, usaZipCodes$pop > 100)
usaZipCodes

maxAndMin <- summarize(usaZipCodes, MaxPop = max(usaZipCodes$pop),
                       MinPop = min(usaZipCodes$pop))
maxAndMin

localDf <- collect(maxAndMin)
localDf

# zip codes in each state

zipCodesByState <-  summarize(groupBy(usaZipCodes, usaZipCodes$state), 
                              Count = n(usaZipCodes$state))

zipCodesByState
count(zipCodesByState)
collect(zipCodesByState)
collect(arrange(zipCodesByState, zipCodesByState$state))
collect(arrange(zipCodesByState, desc(zipCodesByState$Count)))

# 10 Most populous zip codes

arrange(usaZipCodes, desc(usaZipCodes$pop))
head(arrange(usaZipCodes, desc(usaZipCodes$pop)), n = 10)

# Most populous states

popByState <-  summarize(groupBy(usaZipCodes, usaZipCodes$state), 
                              TotalPop = sum(usaZipCodes$pop))

popByState
count(popByState)
collect(popByState)
head(arrange(popByState, desc(popByState$TotalPop)))

# Stop the SparkSession now
sparkR.session.stop()
