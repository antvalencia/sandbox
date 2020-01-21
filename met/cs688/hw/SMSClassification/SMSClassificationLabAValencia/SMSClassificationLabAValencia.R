# KNN Classification of SMS Dataset
rm(list=ls()); cat("\014") # clear all
library(tm) # Use this package for Text Mining

load("Data/SMS_DTM.RData") # Load dtm from saved data

dtm <- as.matrix(dtm)
dtm <- dtm[1:1000,] # Subset DTM

# Split the Document-Term Matrix into Train & Test Datasets
library(class) # 
# Consider "spam" as Positive and "ham" as Negative
Positive <- "spam"; Negative <- "ham"; CM.Names <- c(Positive,Negative)
DS.Size <- dim(dtm)[1]
Test.Train.Percent <- 0.6 # Split Data into 60% for Training and 40% for Testing
ix.Range <- round(DS.Size*Test.Train.Percent)
train.Range <- seq(from = 1, to = ix.Range, by = 1); test.Range <- seq(from = (ix.Range+1), to = DS.Size, by = 1)

train.doc <- dtm[train.Range,] # Dataset for which classification is already known
test.doc <- dtm[test.Range,] # Dataset we are trying to classify

# Generate TAGS - Correct answers for the Train dataset
z <- c(SMS.Data$Tag[train.Range])
sum(z==Positive) # 88 - Number of Positive events ("spam") in the Test Data
sum(z==Negative) # 512 - Number of Negative events ("ham") in the Test Data
z <- gsub(Positive,"Positive",z); z <- gsub(Negative,"Negative",z) # Replace Tags with 
Tags.Train <- factor(z, levels = c("Positive","Negative")) # kNN expects Tags to be Factors data type

# Generate TAGS - Correct answers for the Test dataset
z <- c(SMS.Data$Tag[test.Range]); 
sum(z==Positive) # 64 - Number of Positive events ("spam") in the Test Data
sum(z==Negative) # 336 - Number of Negative events ("ham") in the Test Data
z <- gsub(Positive,"Positive",z); z <- gsub(Negative,"Negative",z) # Replace Tags with 
Tags.Test <- factor(z, levels = c("Positive","Negative")) # kNN expects Tags to be Factors data type

# 1) KNN Classificationusing package "class" ====
set.seed(0)  
prob.test <- knn(train.doc, test.doc, Tags.Train, k = 2, prob=TRUE) # k-number of neighbors considered

### -----------
# Display Classification Results
a <- 1:length(prob.test)
b <- levels(prob.test)[prob.test] # asign your classification Tags (Talk or Sci)
c <- attributes(prob.test)$prob # asign your classification probability values 
d <- prob.test==Tags.Test # Logicaly match your classification Tags with the known "Tags"
result <- data.frame(Doc=a,Predict=b,Prob=c, Correct=d) # Tabulate your result
sum(d)/length(Tags.Test) # % Correct Classification (0.86)

result

# Insert your code to find:
# Confusion Matrix
tp <- nrow(result[which(result$Predict == 'Positive' & result$Correct == TRUE), ])
fp <- nrow(result[which(result$Predict == 'Positive' & result$Correct == FALSE), ])
tn <- nrow(result[which(result$Predict == 'Negative' & result$Correct == TRUE), ])
fn <- nrow(result[which(result$Predict == 'Negative' & result$Correct == FALSE), ])

# just making sure
nrow(result[which(result$Predict != 'Negative' & result$Predict != 'Positive' ), ])
# returns 0

# Precision
precision <- tp/(tp+fp)
precision

# Recall
recall <- tp/(tp+fn)
recall

# Fscore
f_score <- 2*((precision*recall)/(precision+recall))
f_score

# Accuracy <- (TP+TN)/(TP+TN+FP+FN)
accuracy <- (tp+tn)/(tp+tn+fp+fn)
accuracy
