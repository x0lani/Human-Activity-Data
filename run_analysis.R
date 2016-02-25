# Collect the data from http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#
#

library(dplyr)

readActivityLabels <- function (file = "UCI HAR Dataset/activity_labels.txt") {
    read.table(file, sep = " ",
               col.names = c("Activity.ID", "Activity.Label"))
}

readFeatures <- function (file = "UCI HAR Dataset/features.txt") {
    read.table(file, sep = " ",
               col.names = c("Column.Number", "Feature"),
               colClasses = c("integer","character"),
               stringsAsFactors = FALSE)
}

readActivityData <- function(subject_file = "subject_test.txt",
                             activity_file ="y_test.txt",
                             measurement_file = "X_test.txt") {
    subject.ID <- read.table(subject_file, colClasses = "integer")[,1]
    activity.ID <- read.table(activity_file, colClasses = "integer")[,1]
    measurements <- read.table(measurement_file,
                               colClasses = rep("numeric",561),
                               col.names = columnIDs$Feature)
    cbind(subject.ID,activity.ID, measurements)
}

removeColumns <- function(dataset){
    # this function removes all columns from a data set, except for
    #     "subject.ID"
    #     "activity"
    #     columns containing "std"
    #     columns containing "mean"
    filter <- (names(dataset) =="subject.ID") | (names(dataset) =="activity.ID")
    filter <- filter | grepl("std",names(dataset))
    filter <- filter | grepl("mean",names(dataset))
    dataset[,filter]
}

# main program start
labels <- readActivityLabels()
columnIDs <- readFeatures()

# load each of the individual data sets and combine
testData <- readActivityData(subject_file = "UCI HAR Dataset/test/subject_test.txt",
                 activity_file = "UCI HAR Dataset/test/y_test.txt",
                 measurement_file = "UCI HAR Dataset/test/X_test.txt")
trainData <- readActivityData(subject_file = "UCI HAR Dataset/train/subject_train.txt",
                              activity_file = "UCI HAR Dataset/train/y_train.txt",
                              measurement_file = "UCI HAR Dataset/train/X_train.txt")
fullData <- rbind(testData, trainData)  # combine test and train data
rm(testData, trainData)  # individual test and train data no longer needed

#remove unneeded columns
fullData <- removeColumns(fullData)

# convert activity.ID to names
fullData <- merge(fullData, labels, by.x = "activity.ID", by.y = "Activity.ID")
# rearrange columns so subject.ID and activity label are first
fullData <- fullData[c(2,82,3:81)]

#group and summarize
fullData <- arrange(tbl_df(fullData), subject.ID, Activity.Label) # convert to dplyr datatable
fullData <- group_by(fullData, subject.ID, Activity.Label)
summaryData <- summarise_each(fullData, funs(mean), 3:81)
write.csv(summaryData, file = "ActivityDataSummary.csv", row.names = FALSE)
