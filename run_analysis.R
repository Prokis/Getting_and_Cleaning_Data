#1. Merges the training and the test sets to create one data set.
# reading subject training data
subject_train = read.table("train/subject_train.txt", col.names=c("subject_id"))
# assigning row number as ID column
subject_train$ID <- as.numeric(rownames(subject_train))
# reading x training data
X_train = read.table("train/X_train.txt")
# assigning row number as ID column
X_train$ID <- as.numeric(rownames(X_train))
# reading y training data
y_train = read.table("train/y_train.txt", col.names=c("activity_id"))
# assigning row number as ID column
y_train$ID <- as.numeric(rownames(y_train))
# merging subject_train and y_train to train
train <- merge(subject_train, y_train, all=TRUE)
# merging train and X_train
train <- merge(train, X_train, all=TRUE)
# reading subject testing data
subject_test = read.table("test/subject_test.txt", col.names=c("subject_id"))
# assigning row number as ID column
subject_test$ID <- as.numeric(rownames(subject_test))
# reading x testing data
X_test = read.table("test/X_test.txt")
# assigning row number as ID column
X_test$ID <- as.numeric(rownames(X_test))
# reading y testing data
y_test = read.table("test/y_test.txt", col.names=c("activity_id"))
# assigning row number as ID column
y_test$ID <- as.numeric(rownames(y_test))
# merging subject_test and y_test to test
test <- merge(subject_test, y_test, all=TRUE) 
# merging test and X_test
test <- merge(test, X_test, all=TRUE) 
#combining train and test
data1 <- rbind(train, test)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
features = read.table("features.txt", col.names=c("feature_id", "feature_label"),)
selected_features <- features[grepl("mean\\(\\)", features$feature_label) | grepl("std\\(\\)", features$feature_label), ]
data2 <- data1[, c(c(1, 2, 3), selected_features$feature_id + 3) ]

#3. Uses descriptive activity names to name the activities in the data set.
activity_labels = read.table("activity_labels.txt", col.names=c("activity_id", "activity_label"),)
data3 = merge(data2, activity_labels)

#4. Appropriately labels the data set with descriptive activity names. 
selected_features$feature_label = gsub("\\(\\)", "", selected_features$feature_label)
selected_features$feature_label = gsub("-", ".", selected_features$feature_label)
for (i in 1:length(selected_features$feature_label)) {
        colnames(data3)[i + 3] <- selected_features$feature_label[i]
}
data4 = data3

#5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
drops <- c("ID","activity_label")
data5 <- data4[,!(names(data4) %in% drops)]
aggdata <-aggregate(data5, by=list(subject = data5$subject_id, activity = data5$activity_id), FUN=mean, na.rm=TRUE)
drops <- c("subject","activity")
aggdata <- aggdata[,!(names(aggdata) %in% drops)]
aggdata = merge(aggdata, activity_labels)
write.csv(file="tidy.csv", x=aggdata)
