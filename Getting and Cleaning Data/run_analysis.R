
# 1. Merges the training and the test sets to create one data set. ---------------------

#unzip files
#unzip("getdata-projectfiles-UCI HAR Dataset.zip")

#read xtest and xtrain into data table 
xtest <- read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE)
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt",header=FALSE)
features <- rbind(xtest,xtrain)
names(features) <- read.table("./UCI HAR Dataset/features.txt",header=FALSE)$V2

ytest <- read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE)
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE)
activity <- rbind(ytest,ytrain)
names(activity) <- "activity"

subjectTest <- read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE)
subjectTrain <- read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE)
subject <- rbind(subjectTest,subjectTrain)
names(subject) <- "subject"

#merges the features,subject and activity data table into one data set
data <- cbind(features,subject,activity)


# 2. Extracts only the measurements on the mean and standard deviation -----------------

#create a subset of the data set: measurements of mean and standard deviation.
data_subset <- data[,grepl("mean[()]|std[()]",colnames(data))]

#include the subject and activity columns to the data set
data_subset <- cbind(data_subset,data[,562:563])


# 3. Uses descriptive activity names to name the activities in the data set ------------

#read the activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

#replace the activity column values with descriptive names based on activity labels
data_subset$activity <- sapply(data_subset$activity,function(x){activity_labels[x,2]})


# 4. Appropriately labels the data set with descriptive variable names -----------------

#substitute abbreviated column names with the full description 
names(data_subset) <- gsub("Acc","Accelerometer",names(data_subset))
names(data_subset) <- gsub("Gyro","Gyroscope",names(data_subset))
names(data_subset) <- gsub("Mag","Magnitude",names(data_subset))
names(data_subset) <- gsub("BodyBody","Body",names(data_subset))
names(data_subset) <- gsub("^t","time",names(data_subset))
names(data_subset) <- gsub("^f","frequency",names(data_subset))


# 5. From the data set in step 4, creates a second, independent tidy data set ----------
# with the average of each variable for each activity and each subject

#import the dplyr package
library(dplyr)
data_tidy <- group_by(data_subset,subject,activity)%>% #group the data set by activity and subject
          summarise_each(funs(mean))               #take the mean of column for each subject and activity

#write the output into a text file
write.table(data_tidy,file="tidy data.txt",row.names=FALSE)



