# read features.txt
con <- file("features.txt")
features <- readLines(con); close(con)
features <- sapply(strsplit(features, split = " "), function(x){x[2]})

# read activity_labes.txt
con <- file("activity_labels.txt")
activity_labels <- readLines(con); close(con)
activity_labels <- sapply(strsplit(activity_labels, split = " "), function(x){x[2]})

# reading test folder
#=============================================

# read subject_test.txt
con <- file("test/subject_test.txt")
subject_test <- readLines(con); close(con)
subject_test <- as.numeric(subject_test)

# read X_test.txt
X_test.path <- "test/X_test.txt"
X_test <- dfBuilder(X_test.path)

# read y_test.txt
con <- file("test/y_test.txt")
y_test <- readLines(con); close(con)
y_test <- as.numeric(y_test)


# reading train folder
#=============================================

# read subject_train.txt
con <- file("train/subject_train.txt")
subject_train <- readLines(con); close(con)
subject_train <- as.numeric(subject_train)

# read X_train.txt
X_train.path <- "train/X_train.txt"
X_train <- dfBuilder(X_train.path)

# read y_train.txt
con <- file("train/y_train.txt")
y_train <- readLines(con); close(con)
y_train <- as.numeric(y_train)



# running the analysis
#==============================================

#Join and bind tables
accelerometerStudy_test <- data.frame(subjectID = subject_test, activity = y_test, group = "test", X_test)
accelerometerStudy_train <- data.frame(subjectID = subject_train, activity = y_train,group = "train", X_train)
accelerometerStudy <- rbind(accelerometerStudy_test, accelerometerStudy_train)

#Descriptive activity names to name the activities in the data set
accelerometerStudy$activity <- as.factor(accelerometerStudy$activity)
levels(accelerometerStudy$activity) <- activity_labels

#Appropriately labels the data set with descriptive variable names
names(accelerometerStudy)[4:ncol(accelerometerStudy)] <- features

#Extracts only the measurements on the mean and standard deviation for each measurement
extrPattern <- grepl("mean", names(accelerometerStudy)) | grepl("std", names(accelerometerStudy))
extrPattern[1:3] <- TRUE
accelerometerStudy_MeanStd <- accelerometerStudy[,extrPattern]

#From the data set in previos step, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject

library(dplyr)
accelerometerStudy_MeanStd_tibble <- tbl_df(accelerometerStudy_MeanStd)


meanBy_activitySubject <- accelerometerStudy_MeanStd_tibble %>% 
    group_by(activity, subjectID) %>%
    summarize_at(vars(4:82), mean)
newNames <- names(meanBy_activitySubject)
newNames[4:length(newNames)] <- paste("Average", newNames[4:length(newNames)], sep = "-")
meanBy_activitySubject <- setNames(meanBy_activitySubject, newNames)

# writing results
write.table(meanBy_activitySubject, "tidy data set.txt", row.names = F)
