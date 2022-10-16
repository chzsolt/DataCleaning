require(dplyr)

run_analysis <- function(){
  
## Reading labels
  feature_labels <- read.csv("features.txt", sep = "", header = FALSE)[2]
  activity_labels <- read.csv("activity_labels.txt", sep = "", header = FALSE)
## Reading, merging and labeling features
  test_features <- read.csv("test/X_test.txt", sep = "", header = FALSE)
  train_features <- read.csv("train/X_train.txt", sep = "", header = FALSE)
  features <- rbind(test_features,train_features)
  colnames(features) <- as.character(t(feature_labels))
## Reading, merging and labeling activities
  test_activities <- read.csv("test/y_test.txt", sep = "", header = FALSE)
  train_activities <- read.csv("train/y_train.txt", sep = "", header = FALSE)
  activities <- rbind(test_activities, train_activities)
  colnames(activities) <- "Activity"
## Reading, merging and labeling subjects
  test_subjects <- read.csv("test/subject_test.txt", sep = "", header = FALSE)
  train_subjects <- read.csv("train/subject_train.txt", sep = "", header = FALSE)
  subjects <- rbind(test_subjects, train_subjects)
  colnames(subjects) <- "Subject"
## Extracting columns which includes measurements
  required_colunms <- grep("std|mean", names(features), ignore.case = TRUE)
  features <- features[,required_colunms]
## Merging subject, activity and features into one data frame
  data <- cbind(subjects, activities, features)  
## Using descriptive activity names
  for (i in 1:6){
    data$Activity[data$Activity == i] <- as.character(activity_labels[i,2])}
## Using descriptive feature names
  names(data)<-gsub("Acc", "Accelerometer", names(data))
  names(data)<-gsub("Gyro", "Gyroscope", names(data))
  names(data)<-gsub("BodyBody", "Body", names(data))
  names(data)<-gsub("Mag", "Magnitude", names(data))
  names(data)<-gsub("^t", "Time", names(data))
  names(data)<-gsub("^f", "Frequency", names(data))
  names(data)<-gsub("tBody", "TimeBody", names(data))
  names(data)<-gsub("-mean()", "Mean", names(data), ignore.case = TRUE)
  names(data)<-gsub("-std()", "STD", names(data), ignore.case = TRUE)
  names(data)<-gsub("-freq()", "Frequency", names(data), ignore.case = TRUE)
  names(data)<-gsub("angle", "Angle", names(data))
  names(data)<-gsub("gravity", "Gravity", names(data))
## Creating  tidy data set with the average of each variable for each activity and each subject
  data <- group_by(data, Subject, Activity) %>% summarise_each(list(mean))
## Creating txt file
  write.table(data, file = "tidy_data.txt", row.names = FALSE)}