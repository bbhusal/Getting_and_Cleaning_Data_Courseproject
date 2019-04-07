## R work of this project ##

library(dplyr)
## Download zip file##
ziplink <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"
if (!file.exists(zipFile)) {
  download.file(ziplink, zipFile, mode = "wb")
}
## Unziping the zip file ##
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}
## Reading training data ##
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))
## Reading test data ##
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))
## Reading features ##
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
## Reading activity labels ##
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")
## Step 1 Merges the training and the test sets to create one data set ##
# concatenating individual data tables to make single data table
humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)
# Removing individual data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity,
   testSubjects, testValues, testActivity)
# Assigning column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

## Step 2-Extracting only the measurements on the mean and standard deviation for each measurement ##
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]

# Step 3 - Uses descriptive activity names to name the activities in the data set ##
humanActivity$activity <- factor(humanActivity$activity,
                                 levels = activities[, 1], labels = activities[, 2])

# Step 4 - Appropriately labeling the data set with descriptive variable names ##
humanActivityCols <- colnames(humanActivity)
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

colnames(humanActivity) <- humanActivityCols

# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
humanActivityMeans <- humanActivity %>%
  group_by(subject, activity) %>%
  summarise_each(funs(mean))
# output to file "tidy.txt"
write.table(humanActivityMeans, "tidy.txt", row.names = FALSE,
            quote = FALSE)

