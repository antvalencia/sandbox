rm(list = ls()); cat("\014")
library(tm) # Load the Text Mining package

reuters21578 <- system.file("texts", "reuters-21578.xml", package = "tm")
reuters21578.c <- VCorpus(URISource(reuters21578),readerControl=list(reader=readPlain))
inspect(reuters21578.c)
data("acq")
reuters <- acq

reuters[4]$meta

reuters[4]$description

reuters[4]$description <- "Midterm Exam Test"
reuters[4]$description
