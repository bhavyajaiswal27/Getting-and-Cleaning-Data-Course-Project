####################################################################################
#
# Coursera assessment
# "Getting and Cleaning Data" https://class.coursera.org/getdata-010

# Script: run_analysis.R
# Author: Axel Rose <axel.roeslein@googlemail.com>
# No Copyrights, Public Domain
# January 2015

# task description:
# You should create one R script called run_analysis.R that does the following. 
# 1 - Merges the training and the test sets to create one data set.
# 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3 - Uses descriptive activity names to name the activities in the data set
# 4 - Appropriately labels the data set with descriptive variable names. 
# 5 - From the data set in step 4, creates a second, independent tidy data set with the average
#     of each variable for each activity and each subject.

# 6 - Please upload the tidy data set created in step 5 of the instructions.
#     Please upload your data set as a txt file created with write.table() using row.name=FALSE

# 7 - Please submit a link to a Github repo with the code for performing your analysis.
#     The code should have a file run_analysis.R in the main directory that can be run as long
#     as the Samsung data is in your working directory. The output should be the tidy data set
#     you submitted for part 1. You should include a README.md in the repo describing how the
#     script works and the code book describing the variables.


####################################################################################
# 0 - Preparations
####################################################################################

baseDir <- "~/Documents/coursera/getting-and-cleaning-data/assessment-tidy-data"
setwd(baseDir)
# TODO: exit if we cannot set this working directory

# download the source and unzip into a data directory
dataSrc <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dataDir <- "data"
zipFile <- "Dataset.zip"

if (!file.exists(dataDir)) {
	dir.create(dataDir)
}
setwd(dataDir)

# TODO: skip download step if already done and identical
download.file(dataSrc, destfile = zipFile, method = "curl")
date()

list.files(pattern = zipFile)
unzip(zipFile)

# TODO: check if the subdir really exists
# something like if (file.access("data/UCI HAR Dataset"))  ...
dataBaseDir <- "UCI HAR Dataset"

setwd(baseDir)

####################################################################################
# 1 - Merge training and test set
####################################################################################

testDataDir <- paste(dataDir, "/", dataBaseDir, "/test", sep = "")
trainingDataDir <- paste(dataDir, "/", dataBaseDir, "/train", sep = "")

####################################################################################
# 1.1 - get column names
colNamesRaw <- read.table(paste(dataDir, "/", dataBaseDir, "/features.txt", sep = ""))
# should be: 561 2
dim(colNamesRaw)
# remove first column and convert to a vector by transposing with t()
colNames <- t(colNamesRaw[-1])
# rm(colNamesRaw)

####################################################################################
# 1.2 - merge *within* training
train1 <- read.table(paste(trainingDataDir, "/", "X_train.txt", sep = ""))
# check to see if it is what we wanted, should return "[1] 7352  561"
dim(train1)

# train1 <- read.table(paste(trainingDataDir, "/", "X_train.txt", sep=""), col.names=colNames)
# ane have meaningful column names
# head(names(train1))
# "-", "(" or ")" gets replaced in column names as "."

train1subject <- read.table(paste(trainingDataDir, "/", "subject_train.txt", sep = ""))
# should have: "[1] 7352    1"
dim(train1subject)

train1activity <- read.table(paste(trainingDataDir, "/", "y_train.txt", sep = ""))
# should have: "[1] 7352    1"
dim(train1activity)

train <- cbind(train1, train1subject, train1activity)
# should extend train1 with two columns and return "[1] 7352  563"
dim(train)

####################################################################################
# 1.3 - merge *within* test
test1 <- read.table(paste(testDataDir, "/", "X_test.txt", sep = ""))
# check to see if it is what we wanted, should return "[1] 2947  561"
dim(test1)

# test1 <- read.table(paste(testDataDir, "/", "X_test.txt", sep=""), col.names=colNames)
# ane have meaningful column names
# head(names(test1))
# "-", "(" or ")" gets replaced in column names as "."

test1subject <- read.table(paste(testDataDir, "/", "subject_test.txt", sep = ""))
# should have: "[1] 2947    1"
dim(test1subject)

test1activity <- read.table(paste(testDataDir, "/", "y_test.txt", sep = ""))
# should have: "[1] 2947    1"
dim(test1activity)

test <- cbind(test1, test1subject, test1activity)
# should extend test1 with two columns and return "[1] 2947  563"
dim(test)

####################################################################################
# 1.4 - merge training with test

data <- rbind(train, test)
# should return "[1] 10299   563"
dim(data)



####################################################################################
# 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
####################################################################################

# I assume that this are only those columns where "mean()" or "std()" is explicitely in the column name
# It is not sufficient to just include the word "mean" as in "fBodyBodyGyroJerkMag-meanFreq()"

usedCols <- colNamesRaw[which(grepl(pattern = "std\\(\\)|mean\\(\\)", x = colNamesRaw[, 2])), ]
# should return 66 rows in 2 cols: "[1] 66  2"
dim(usedCols)

# now using column 1 of usedCols as index into the data
# but add the subject and activity columns (562 and 563) since we need them later
data2 <- data[, c(usedCols[, 1], 562, 563)]
# we should now have only 68 cols (66 usedCols + subject label + activity label)
# with the same number of rows: [1] 10299    68
dim(data2)


####################################################################################
# 3 - Uses descriptive activity names to name the activities in the data set
####################################################################################

# read activity master data, giving some meaningful header "id" and "label"
activityLabels <- read.table(paste(dataDir, "/", dataBaseDir, "/activity_labels.txt", sep = ""), col.names = c("id", "activity.label"))
# check results
activityLabels

# set id column names for the merge below
colnames(data2) <- t(usedCols[, 2])
names(data2)[67] <- "subject.id"
names(data2)[68] <- "activity.id"

# the merge - using the labeled id columns
data3 <- merge(data2, activityLabels, by.x = "activity.id", by.y = "id")

# check for plausible result
table(data3[, c("activity.id", "activity.label")])
#           label
#activity.id LAYING SITTING STANDING WALKING WALKING_DOWNSTAIRS WALKING_UPSTAIRS
#          1      0       0        0    1722                  0                0
#          2      0       0        0       0                  0             1544
#          3      0       0        0       0               1406                0
#          4      0    1777        0       0                  0                0
#          5      0       0     1906       0                  0                0
#          6   1944       0        0       0                  0                0

# data3 has the required activity name in the column with the "activity.label" header



####################################################################################
# 4 - Appropriately labels the data set with descriptive variable names. 
####################################################################################

# we already set variable description in step 3 out of caution that the merge()
# operation might change the column order and it would be difficult to set it again here

# just checking:
str(data3)
# 'data.frame':	10299 obs. of  69 variables:
#  $ activity.id                : int  1 1 1 1 1 1 1 1 1 1 ...
#  $ tBodyAcc-mean()-X          : num  0.302 0.343 0.27 0.268 0.314 ...
#  ...
#  $ fBodyBodyGyroJerkMag-std() : num  -0.269 -0.382 -0.158 0.149 -0.256 ...
#  $ subject.id                 : int  7 5 6 23 7 7 11 6 10 11 ...
#  $ activity.label             : Factor w/ 6 levels "LAYING","SITTING",..: 4 4 4 4 4 4 4 4 4 4 ...

# looks fine (assuming the reader understands the meaning of "tBodyAcc-mean()-X", otherwise
# have a look into "features_info.txt" as contained in the downloaded data archive, path
# data/UCI HAR Dataset/features_info.txt)

####################################################################################
# 5 - From the data set in step 4, creates a second, independent tidy data set with
#     the average of each variable for each activity and each subject.
####################################################################################


# those are the measured variables:
names(data3)[2:67]

# check a small subset
head(aggregate(data3[, 2:4], by = list(data3$subject.id, data3$activity.label), FUN = mean))
#   Group.1 Group.2 tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z
# 1       1  LAYING         0.2215982       -0.04051395        -0.1132036
# 2       2  LAYING         0.2813734       -0.01815874        -0.1072456
# 3       3  LAYING         0.2755169       -0.01895568        -0.1013005
# 4       4  LAYING         0.2635592       -0.01500318        -0.1106882
# 5       5  LAYING         0.2783343       -0.01830421        -0.1079376
# 6       6  LAYING         0.2486565       -0.01025292        -0.1331196
tail(aggregate(data3[, 2:4], by = list(data3$subject.id, data3$activity.label), FUN = mean))
#     Group.1          Group.2 tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z
# 175      25 WALKING_UPSTAIRS         0.2779954       -0.02698635        -0.1262104
# 176      26 WALKING_UPSTAIRS         0.2726914       -0.02816338        -0.1219435
# 177      27 WALKING_UPSTAIRS         0.2657703       -0.02009533        -0.1235304
# 178      28 WALKING_UPSTAIRS         0.2620058       -0.02794439        -0.1215140
# 179      29 WALKING_UPSTAIRS         0.2654231       -0.02994653        -0.1180006
# 180      30 WALKING_UPSTAIRS         0.2714156       -0.02533117        -0.1246975

# finally the requested tidied data
# leaving out data3 cols 1, 68 and 69 which were introduced during the merge
tidyData <- aggregate(data3[, 2:67], by = list(data3$subject.id, data3$activity.label), FUN = mean)

# header names are ok but for the first col, setting to "subject"
names(tidyData)[1] <- "subject"
names(tidyData)[2] <- "activity"


####################################################################################
# 6 - Please upload the tidy data set created in step 5 of the instructions.
#     Please upload your data set as a txt file created with write.table() using row.name=FALSE
####################################################################################

RESULT <- "tidydata.txt"
# just check where we are
getwd()
write.table(tidyData, file = RESULT, row.names = TRUE)
# check if it was written
list.files()
# dimension should be 180 (30 subjects x 6 activities) and 68 columns for subject, activity and 66 measurements
dim(tidyData)

# verify the output is readable and check content visually ourselves and as a possible
# help for the reviewer
View(read.table(RESULT, header = TRUE))

# uploading to github outside of R
# git add .; git commit -m 'data cleanup'; git push

####################################################################################
# 7 - Please submit a link to a Github repo with the code for performing your analysis.
#     The code should have a file run_analysis.R in the main directory that can be run as long
#     as the Samsung data is in your working directory. The output should be the tidy data set
#     you submitted for part 1. You should include a README.md in the repo describing how the
#     script works and the code book describing the variables.
####################################################################################

# done outside

# over + out.
####################################################################################