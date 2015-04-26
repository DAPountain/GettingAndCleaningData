#####################################
#  0) Pull in the info on the features, 'tidy' up the names a bit
# We will use these to label the data as we load it
#####################################

#Load the data (and label the features)
features <- read.table("data/features.txt")[,2]
#only get the mean() and std() columns 
#(NOTE: intentionally not getting meanFreq or the angle() columns that others seem to be including)
meanOrStdCols <- grep( "mean\\(\\)|std\\(\\)",features)

#clean up the features for readability, (make.names will resolve any non-unique columns)
features <- gsub( "^t","time", features)
features <- gsub( "^f","freq", features)
features <- make.names(features, unique=TRUE, allow_=TRUE)
features <- gsub( "\\.\\.","", features)
features <- gsub( "mean.Y","Y.mean", features)
features <- gsub( "mean.X","X.mean", features)
features <- gsub( "mean.Z","Z.mean", features)
features <- gsub( "std.Y","Y.std", features)
features <- gsub( "std.X","X.std", features)
features <- gsub( "std.Z","Z.std", features)
features <- gsub( "std","standardDeviation", features)

#####################################
#  1) Merge the training and the test sets to create one data set.
#####################################
### Test Data ###

testData <- read.table("data/test/X_test.txt")
colnames(testData) <- features

#Load the Activity and add it to the Data table
activity <-read.table("data/test/Y_test.txt")
colnames(activity) <- c("activityId")
testData <- cbind(activity,testData)

#Load the Subject and add it to the Data table
subject <- read.table("data/test/subject_test.txt")
colnames(subject) <- c("subject")
testData <- cbind(subject,testData)

#Add a column to track the source, this column will be use to differntiate
#between data originating from the Test or Train sets
#dataSource <- as.vector("TEST")
#testData <- cbind(testData, dataSource)

### Training Data ###

#Load the data (and label the features)
trainingData <- read.table("data/train/X_train.txt")
colnames(trainingData) <- features

#Load the Activity and add it to the Data table
trainingActivity <-read.table("data/train/Y_train.txt")
colnames(trainingActivity) <- c("activityId")
trainingData <- cbind(trainingActivity,trainingData)

#Load the Subject and add it to the Data table
subject <- read.table("data/train/subject_train.txt")
colnames(subject) <- c("subject")
trainingData <- cbind(subject,trainingData)

#Add a column to track the source, this column will be use to differntiate
#between data originating from the Test or Train sets
#dataSource <- as.vector("TRAINING")
#trainingData <- cbind(trainingData, dataSource)

# Merge the test and Train sets to create a single set
fullData <- rbind(testData, trainingData)

#####################################
#  2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#####################################
# Select out only the relevant columns, we add 2 here because we added the subject and activity columns
myData <- select( fullData, c(1:2, meanOrStdCols+2))

#####################################
#  3) Uses descriptive activity names to name the activities in the data set
#####################################
#Load the activity descriptions
activityDesc <- read.table("data/activity_labels.txt")
colnames(activityDesc) <- c("activityId","activityDesc")

# Merge the ActivityDescriptions into the data set 
finalData <- merge(myData, activityDesc, by="activityId", all.x=TRUE)
#move the activityDesc column to the front of the frame
finalData <- finalData[,c(2,1,69,3:68)]
#order the frame by subject and activity
finalData <- arrange(finalData, subject, activityId)

#####################################
# 4) Appropriately labels the data set with descriptive variable names.  
# Names were pulled from features.txt and cleaned up
#####################################

# names(finalData)
# write the output file
write.table( finalData, file="./fullData.txt", row.names=FALSE)

#####################################
#  5) From the data set in step 4, creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject.
#####################################
grouping<-group_by(finalData,subject,activityDesc)
grouped_data <- summarise_each(grouping,funs(mean))
write.table( grouped_data, file="./finalData.txt", row.names=FALSE)


