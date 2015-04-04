##This file runs a data analysis and creates a tidy data package
## it requires the UCI HAR dataset

## load necessary libraries
library(car)
library(plyr)

## read in, clean and create  column names 
cnames <- c("Group", "Activity", "Subject", 
            as.character(read.table("UCI HAR Dataset/features.txt")$V2))
cnames <- gsub("\\(\\)", "", cnames) 
cnames <- gsub("\\(|\\)", "_", cnames)
cnames <- gsub("\\,", "_", cnames)
cnames <- gsub("-", "_", cnames)

## read in and combine test data 
testLabels <- read.table("UCI HAR Dataset/test/y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
testData <- read.table("UCI HAR Dataset/test/X_test.txt")
testTable <- cbind("test", testLabels)
testTable <- cbind(testTable, testSubjects)
testTable <- cbind(testTable, testData)
colnames(testTable) <- cnames

## read in and combine training data    
trainLabels <- read.table("UCI HAR Dataset/train/y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
trainData <- read.table("UCI HAR Dataset/train/X_train.txt")
trainTable <- cbind("train", trainLabels)
trainTable <- cbind(trainTable, trainSubjects)
trainTable <- cbind(trainTable, trainData)
colnames(trainTable) <- cnames

## Compbine test and training data 
complete <- rbind(testTable, trainTable)

## filter down to the columns wanted
selectColumns <- grep("^Group|^Activity|^Subject|mean|std", cnames, value=TRUE)
completeFiltered <- complete[,selectColumns]

##clean up temp objects
##rm(list = Filter(exists, c("testLabels", "testSubjects", "testData", 
##                "trainLabels", "trainSubjects", "trainData", "testTable", 
##                "trainTable", "complete", "selectColumns", "cnames")))

##set activity names 
completeFiltered$Activity <- as.factor(recode(completeFiltered$Activity, 
            "1='Walking'; 2='Walking_upstairs'; 3='Walking_downstairs'; 
            4='Sitting'; 5='Standing';6='Laying'"))
                             
dataSum <- ddply(completeFiltered, .(Group, Subject), function(x) c(colMeans(completeFiltered[4:82])))