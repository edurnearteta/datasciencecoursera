# 1. Merges the training and the test sets to create one data set.

train <- read.table("train/X_train.txt")
test <- read.table("test/X_test.txt")
completeData <- rbind(train, test)

train <- read.table("train/subject_train.txt")
test <- read.table("test/subject_test.txt")
sub <- rbind(train, test)

train <- read.table("train/y_train.txt")
test <- read.table("test/y_test.txt")
act <- rbind(train, test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
ind <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
completeData <- completeData [, ind]
names(completeData) <- features[ind, 2]
names(completeData) <- gsub("\\(|\\)", "", names(completeData))
names(completeData) <- tolower(names(completeData)) 

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
act[,1] = activities[act[,1], 2]
names(act) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(sub) <- "subject"
cleaned <- cbind(sub, act, completeData)
write.table(cleaned, "cleanData.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(sub)[,1]
numSubjects = length(unique(sub)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
for (a in 1:numActivities) {
result[row, 1] = uniqueSubjects[s]
result[row, 2] = activities[a, 2]
tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
row = row+1
}
}
write.table(result, "tidy_ds_with_the_averages.txt")

