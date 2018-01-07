# Getting and Cleaning Data week 4 assignment
# 
#  The purpose of this project is to demonstrate your ability to collect,
#  work with, and clean a data set.
#  The goal is to prepare tidy data that can be used
#  for later analysis.  You will be required to submit: 1) a tidy
#  data set as described below, 2) a link to a Github repository with your
#  script for performing the analysis, and 3) a code book that describes the
#  variables, the data, and any transformations or work that you performed to
#  clean up the data called CodeBook.md. You should also include a README.md in
#  the repo with your scripts. This repo explains how all of the scripts work
#  and how they are connected.
#  
   
library(data.table)
library(reshape2) 
library(plyr)
library(RCurl) 

Url = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists('./UCI HAR Dataset.zip')){
  download.file(Url ,'./UCI HAR Dataset.zip', mode = 'wb')
  unzip("UCI HAR Dataset.zip", exdir = getwd())
}

featuresfile <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
featuresCol <- as.character(featuresfile[,2])

train_activity <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
train_subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')
train_x <- read.table('./UCI HAR Dataset/train/X_train.txt')

trainDataset <-  data.frame(train_subject, train_activity , train_x)
names(trainDataset ) <- c(c('subject', 'activity'), features)

test_x <- read.table('./UCI HAR Dataset/test/X_test.txt')
test_activity <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
test_subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

testDataset <-  data.frame(test_subject ,test_activity, test_x)
names(testDataset ) <- c(c('subject', 'activity'), features)
dataAllComb <- rbind(trainDataset ,testDataset )

MeanAndStdInd <- grep('mean|std', featuresCol )
MeanAndStdSubset <- dataAllComb [,c(1,2,MeanAndStdInd  + 2)]

activityLabels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
activityLabels <- as.character(activityLabels[,2])
MeanAndStdSubset$activity <- activityLabels[MeanAndStdSubset$activity]
 
descriptiveName <- names(MeanAndStdSubset )
descriptiveName <- gsub("[(][)]", "", descriptiveName)
descriptiveName <- gsub("^t", "TimeDomain_", descriptiveName)
descriptiveName <- gsub("^f", "FrequencyDomain_", descriptiveName)
descriptiveName <- gsub("Acc", "Accelerometer", descriptiveName)
descriptiveName <- gsub("Gyro", "Gyroscope", descriptiveName)
descriptiveName <- gsub("Mag", "Magnitude", descriptiveName)
descriptiveName <- gsub("-mean-", "_Mean_", descriptiveName)
descriptiveName <- gsub("-std-", "_StandardDeviation_", descriptiveName)
descriptiveName <- gsub("-", "_", descriptiveName)
names(MeanAndStdSubset ) <-descriptiveName

data.tidy <- aggregate(MeanAndStdSubset[,3:81], by = list(activity = MeanAndStdSubset$activity, subject = MeanAndStdSubset$subject),FUN = mean)
write.table(x = data.tidy, file = "data_tidy.txt", row.names = FALSE)

    