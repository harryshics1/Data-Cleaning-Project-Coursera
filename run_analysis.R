# Getting and Cleaning Data Week 4 Project 
# Author: Ziyan (Harry) Shi 

# Criteria: 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation 
#    for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

# Load Packages
library(dplyr)
library(data.table)

# Get Data
filename <- "week4datacleaning_proj.zip"

# Checking if directory already exists.
if (!file.exists("./data")){
  dir.create("./data")
}
dataURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataURL, destfile = paste0("./data/", filename), method="curl")
unzip(zipfile = filename) 

# Read in Activity + Feature Names
Activity_Names <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE, col.names = c("Label", "ActivityType"))
Features_Names <- read.table("UCI HAR Dataset/features.txt", header = FALSE, col.names = c("index", "FeatureType"))

# Read in Activity, Feature, Subject training and testing data 
ActivityTrain <- read.table("UCI HAR Dataset/train/y_train.txt",header = FALSE)
ActivityTest  <- read.table("UCI HAR Dataset/test/y_test.txt",header = FALSE)

FeaturesTrain <- read.table("UCI HAR Dataset/train/X_train.txt",header = FALSE)
FeaturesTest  <- read.table("UCI HAR Dataset/test/X_test.txt",header = FALSE)

SubjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt",header = FALSE)
SubjectTest  <- read.table("UCI HAR Dataset/test/subject_test.txt",header = FALSE)

# Step 1: Merges the training and the test sets to create one data set.

# 1. Bind Data tables
Activity <- rbind(ActivityTrain, ActivityTest)
Feature <- rbind(FeaturesTrain, FeaturesTest)
Subject <- rbind(SubjectTrain, SubjectTest)

# 2. Rename the column 
colnames(Activity) <- c("Activity")
colnames(Subject) <- c("Subject")
colnames(Feature) <- Features_Names$FeatureType

# 3. Get the merged data table
mergedData <- cbind(Subject, Activity, Feature)

# 4. View to check whether the data has been successfully merged
View(mergedData)

# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

# 1. Extract features with "mean()" or "std()"
selectedData <- cbind(Subject, Activity, mergedData[grep("mean\\(\\)|std\\(\\)", colnames(mergedData))])

# 2. Check and bind subject and activity back
View(selectedData)

# Step 3: Uses descriptive activity names to name the activities in the data set

# 1. Make Activity character type
selectedData$Activity <- as.character(selectedData$Activity) 

# 2. Make Activity descriptive and change it to factor variable for later use
selectedData$Activity <- Activity_Names[selectedData$Activity, 2]
selectedData$Activity <- as.factor(selectedData$Activity)

# Step 4: Appropriately labels the data set with descriptive variable names.
names(selectedData)
names(selectedData)<-gsub("-mean()", "Mean", names(selectedData), ignore.case = TRUE)
names(selectedData)<-gsub("-std()", "Std", names(selectedData), ignore.case = TRUE)
names(selectedData)<-gsub("BodyBody", "Body", names(selectedData))
names(selectedData)<-gsub("^t", "Time", names(selectedData))
names(selectedData)<-gsub("^f", "Frequency", names(selectedData))
names(selectedData)<-gsub("Acc", "Accelerometer", names(selectedData))
names(selectedDate)<-gsub("Gyro", "Gyroscope", names(selectedData))
names(selectedData)<-gsub("Mag", "Magnitude", names(selectedData))
# Check
names(selectedData)

# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
outputData <- selectedData %>%
  group_by(Subject, Activity) %>%
  summarise_all(mean)
View(outputData)
write.table(outputData, "outputData.txt", row.name=FALSE)

