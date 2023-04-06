#1. Merge the training and test datasets

#1.1 Reading files
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

features <- read.table("./UCI HAR Dataset/features.txt")

activityLabels = read.table("./UCI HAR Dataset/activity_labels.txt")

#1.2 Assigning variable names
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityID"
colnames(subject_train) <- "subjectID"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activityID"
colnames(subject_test) <- "subjectID"

colnames(activityLabels) <- c("activityID", "activityType")

#1.3 Merging all datasets
all_train_data <- cbind(y_train, subject_train, x_train)
all_test_data <- cbind(y_test, subject_test, x_test)
final_data <- rbind(all_train_data, all_test_data)

#2. Create factors
final_data[["activityID"]] <- factor(final_data[, "activityID"]
                                  , levels = activityLabels[["activityID"]]
                                  , labels = activityLabels[["activityType"]]
)
final_data[["subjectID"]] <- as.factor(final_data[, "subjectID"])

final_data <- melt(final_data, id=c("subjectID", "activityID")) 
final_data <- dcast(final_data, subjectID + activityID ~ variable, mean)

#3. Make the tidy data into a new text file
fwrite(final_data, file="tidyData.txt")
