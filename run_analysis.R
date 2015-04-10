##This file runs a data analysis and creates a tidy data package
## it requires the UCI HAR dataset

## Set file paths
fpath <- "../temp/UCI HAR Dataset/" ## set this to root of dataset
features <- paste(fpath, "features.txt", sep = "")
testL <- paste(fpath, "test/y_test.txt", sep = "")
testS <- paste(fpath, "test/subject_test.txt", sep = "")
testD <- paste(fpath, "test/X_test.txt", sep = "")
trainL <- paste(fpath, "train/y_train.txt", sep = "")
trainS <- paste(fpath, "train/subject_train.txt", sep = "")
trainD <- paste(fpath, "train/X_train.txt", sep = "")

## load necessary libraries
library(car)
library(plyr)

## read in, clean and create  column names 
cnames <- c("Group", "Activity", "Subject", 
        as.character(read.table(features)$V2))
cnames <- gsub("\\(\\)", "", cnames) 
cnames <- gsub("\\(|\\)", "_", cnames)
cnames <- gsub("\\,", "_", cnames)
cnames <- gsub("-", "_", cnames)

## read in and combine test data 
testLabels <- read.table(testL)
testSubjects <- read.table(testS)
testData <- read.table(testD)
testTable <- cbind("test", testLabels)
testTable <- cbind(testTable, testSubjects)
testTable <- cbind(testTable, testData)
colnames(testTable) <- cnames

## read in and combine training data    
trainLabels <- read.table(trainL)
trainSubjects <- read.table(trainS)
trainData <- read.table(trainD)
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
rm(list = Filter(exists, c(
           "features", "testD", "testL", "testS", "trainD", "trainL",
           "trainS", "testLabels", "testSubjects", "testData", "fpath",
           "trainLabels", "trainSubjects", "trainData", "testTable", 
           "trainTable", "complete", "cnames")))

##set activity names 
completeFiltered$Activity <- as.factor(recode(completeFiltered$Activity, 
            "1='Walking'; 2='Walking_upstairs'; 3='Walking_downstairs'; 
            4='Sitting'; 5='Standing';6='Laying'"))

##set variable names
newColnames <- selectColumns
newColnames <- gsub("^t","Time_", newColnames)
newColnames <- gsub("^f","Fourier_", newColnames)
newColnames <- gsub("Body_AccJerk","Body_Linear_Acceleration", newColnames)
newColnames <- gsub("BodyGyro","Body_Angular_Velocity", newColnames)
newColnames <- gsub("BodyAcc","Body_Acceleration", newColnames)
newColnames <- gsub("GravityAcc","Gravity_Acceleration", newColnames)
newColnames <- gsub("meanFreq","Mean_Frequency", newColnames)
newColnames <- gsub("Mag","_Magnitude", newColnames)

colnames(completeFiltered) <- newColnames 
                             
dataSum <- ddply(completeFiltered, .(Group, Subject), function(x) c(colMeans(completeFiltered[4:82])))