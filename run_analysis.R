library(dplyr)
library(data.table)
library(tidyr)

filesPath <- "C:\\Users\\rini\\Documents\\WoRK DoC\\2015\\kursus Data Scientist\\Course 3\\Assignment 1\\UCI HAR Dataset"

# Read subject files
subjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
subjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

# Read activity files
activityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
activityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

#Read data files
train <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
test  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

## 1. Merges the training and the test sets to create one data set.

#Activity and Subject files will merge the training and the test sets by row binding and rename variables "subject" and "activityNum"
dataSubject <- rbind(subjectTrain, subjectTest)
setnames(dataSubject, "V1", "subject")
dataActivity <- rbind(activityTrain, activityTest)
setnames(dataActivity, "V1", "activityNum")

#combine the data training and test files
dataTable <- rbind(train, test)

#name variables according to feature
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#column names for activity labels
activityLabels <- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum", "activityName"))

#merge columns
dataSubjAct <- cbind(dataSubject, dataActivity)
dataTable <- cbind(dataSubjAct, dataTable)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

#reading "features.txt" and extracting only the mean and standard deviation
featuresMeanStd <- grep("mean\\(\\)|std\\(\\)", dataFeatures$featureName, value=TRUE) 

#taking only measurements for the mean and standard deviation and add "subject","activityNum"
featuresMeanStd <- union(c("subject", "activityNum"), featuresMeanStd)
dataTable<- subset(dataTable, select=featuresMeanStd) 

## 3. Uses descriptive activity names to name the activities in the data set.

#enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

#create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr, subject,activityName))

## 4. Appropriately labels the data set with descriptive variable names.

head(str(dataTable),2)

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

#Names after
head(str(dataTable),6)

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

write.table(dataTable, "TidyData.txt", row.name=FALSE)