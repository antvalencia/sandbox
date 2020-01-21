install.packages("tm",dependencies=TRUE)
rm(list = ls()); cat("\014")
library(tm) # Load the Text Mining package

# Similar to the classification example given in class, process and classify the newsgroup document data.
# Download this data from Blackboard and save it on your computer in your R packages folder under "tm/text/".
# Your code MUST access it from there!
# 
# Note that the data is separated into one test and one train folder, each containing 20 sub folders on
# different subjects. Choose these 2 subjects to analyze (sci.space and rec.autos) and 100 documents from each.
# Classify the Newsgroups data (by date version data set) from Blackboard:
#   •        Save data in your "tm/text/" folder so you can specify path using system.file()
#   •        Note that the data is separated into one test and one train folder, each containing 20 sub folders
#            on different subjects.
# Choose 2 subjects to analyze (sci.space and rec.autos) and 100 documents from each.
#   •        For each subject select:
#       –       100 documents for training from the train folder
Train1 <- DirSource('tm/text/20Newsgroups/20news-bydate-train/sci.space')
Doc1.Train <- VCorpus(URISource(Train1$filelist[1:100]),readerControl=list(reader=readPlain))

Train2 <- DirSource('tm/text/20Newsgroups/20news-bydate-train/rec.autos')
Doc2.Train <- VCorpus(URISource(Train2$filelist[1:100]),readerControl=list(reader=readPlain))
#       –       100 documents for testing from the test folder
Test1 <- DirSource('tm/text/20Newsgroups/20news-bydate-test/sci.space')
Doc1.Test <- VCorpus(URISource(Test1$filelist[1:100]),readerControl=list(reader=readPlain))

Test2 <- DirSource('tm/text/20Newsgroups/20news-bydate-test/rec.autos')
Doc2.Test <- VCorpus(URISource(Test2$filelist[1:100]),readerControl=list(reader=readPlain))
#   •        Obtain the merged Corpus (of 400 documents), please keep the order as
#       –       Doc1.Train from the "sci.space" newsgroup train data
#       –       Doc1.Test from the "sci.space" newsgroup test data
#       –       Doc2.Train from the " rec.autos" newsgroup train data
#       –       Doc2.Test from the " rec.autos" newsgroup test data
doc <- c(Doc1.Train, Doc1.Test, Doc2.Train, Doc2.Test)
#   •        Implement preprocessing (clearly indicate what you have used)
# Preprocessing
corpus.temp <- tm_map(doc, removePunctuation) # Remove Punctuation
corpus.temp <- tm_map(corpus.temp, stemDocument, language = "english") # Perform Stemming
#   •        Create the Document-Term Matrix using the following arguments (word lengths of at least 2, word
#            frequency of at least 5) – use proper syntax.
dtm <- DocumentTermMatrix(
  corpus.temp,
  control=list(
    wordLengths=c(2,Inf),
    bounds=list(
      global=c(5, Inf)
      )
  )
  )# Document term matrix
library(class) # Using kNN
# freq <- colSums(dtm.mtx) # Term frequencies
              # ord <- order(freq) # Ordering the frequencies
              # freq[tail(ord)] # Most frequent terms
              # freq[head(ord)] # Least frequent terms
              # findFreqTerms(dtm, lowfreq=5) # List terms (alphabetically) with frequency higher than 5
#   •        Split the Document-Term Matrix into
#       –       train dataset containing rows (1:100,201:300)
train.doc <- dtm[c(1:100,201:300),] # Dataset for which classification is already known
#       –       test dataset containing rows (101:200,301:400)
test.doc <- dtm[c(101:200,301:400),] # Dataset you are trying to classify
#   •        Use the abbreviations "Sci" and "Rec" as tag factors in your classification.
#       –       Check if the tag order is correct using table(Tags)
Tags <- factor(
  c(
    rep("Sci",100),
    rep("Rec",100)
  ),
  levels=c("Sci", "Rec")
) # Tags - Correct answers for the training dataset
table(Tags)
#       –       You should get
#   •        Tags
#   •        Sci Rec
#   •        100 100
#       –       If the order is not right make proper changes.
#   •        Classify text using the kNN() function
set.seed(0)
prob.test <- knn(train.doc, test.doc, Tags, k = 2, prob = TRUE)
#   •        Display classification results as a R dataframe and name the columns as:
#       –       "Doc"
#       –       "Predict"  - Tag factors of predicted subject ("Sci" or "Rec")
#       –       "Prob" - The classification probability
#       –       "Correct' - TRUE/FALSE
v.doc <- 1:length(prob.test)
v.predict <- levels(prob.test)[prob.test]
v.prob <- attributes(prob.test)$prob
v.correct <- prob.test==Tags
result <- data.frame(
  Doc = v.doc,
  Predict = v.predict,
  Prob = v.prob,
  Correct = v.correct
  )
result
#   •        What is percentage of correct (TRUE) classifications?
sum(v.prob)/length(Tags)
sum(prob.test==Tags)/length(Tags)
#   •        Estimate the effectiveness of your classification:
RecClassified <- (prob.test==Tags)[1:100]
TP <- sum(RecClassified=="TRUE")
FN <- sum(RecClassified=="FALSE")

SciClassified <- (prob.test==Tags)[101:200]
FP <- sum(SciClassified=="FALSE")
TN <- sum(SciClassified=="TRUE")
#       –       Consider "rec.autos" as positive and "sci.space" as negative event.
#       –       Clearly mark the values TP, TN, FP, FN
#       –       Create the confusion matrix
CM <- data.frame(
  Rec=c(TP, FN),
  Sci=c(FP, TN),
  row.names=c("Rec","Sci")
  )
CM
#       –       Calculate Precision
P <- TP/(TP+FP)
#       –       Calculate Recall
R <- TP/(TP+FN)
#       –       Calculate F-score
Fscore <- 2*P*R/(P+R)
