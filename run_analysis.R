# Download, save and unzip files
if(!file.exists("./run_analysis")) {dir.create("./run_analysis")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./run_analysis/humanact.zip", method = "curl")
unzip("./run_analysis/humanact.zip", exdir = "./run_analysis")

#0. open and clean datasets

### test dataset
testdata <- read.table("./run_analysis/UCI HAR Dataset/test/X_test.txt")

#######Redefine header. The headers are in the file features.txt. Here, we extract that list of headers as a vector and then use it to rename the headers in the database.
features <- read.table("./run_analysis/UCI HAR Dataset/features.txt")
new_header  <- features$V2
colnames(testdata) <- new_header

#######Adding activities. File y_test.txt contains the activities observed as numeric index.
testlabels <- read.table("./run_analysis/UCI HAR Dataset/test/y_test.txt")
activities <- testlabels$V1
testdata<- cbind(activities, testdata)

####### adding subjects. Available in the file subject_test.txt. The procedure is analogous to the previous step.
testsubjects <- read.table("./run_analysis/UCI HAR Dataset/test/subject_test.txt")
subject <- testsubjects$V1
testdata<- cbind(subject, testdata)


#### train dataset (the process is analogous to the one followed to open an clean the test dataset)
traindata <- read.table("./run_analysis/UCI HAR Dataset/train/X_train.txt")

colnames(traindata) <- new_header

trainlabels <- read.table("./run_analysis/UCI HAR Dataset/train/y_train.txt")
activities <- trainlabels$V1
traindata<- cbind(activities, traindata)

trainsubjects <- read.table("./run_analysis/UCI HAR Dataset/train/subject_train.txt")
subject <- trainsubjects$V1
traindata <- cbind(subject, traindata)


#1. Merging test and train datasets, adding rows to the columns
test_train <- rbind(testdata, traindata)


#2. Extract measurments on the mean and standard deviation
mean <- grep("mean", names(test_train), value = TRUE)
std <- grep("std", names(test_train), value = TRUE)
test_train_msd <- test_train[,c("subject", "activities",mean,std)]

#3. Adding activities lables to the values in the variable.  File activity_labels.txt contains the labels for those activities.
actlabels <- read.table("./run_analysis/UCI HAR Dataset/activity_labels.txt")
al <- actlabels$V2
test_train_msd$activities <- factor(test_train_msd$activities, levels = c(1:6), labels = al)


#4. Making variable names descriptive with appropriate lables
final_header <- colnames(test_train_msd)
final_header <- gsub("\\()", "", final_header,)
final_header <- gsub("-", ".", final_header,)
final_header <- gsub("^t", "time", final_header,)
final_header <- gsub("^f", "freq", final_header,)
colnames(test_train_msd) <- final_header


#5. Creating an independent tidy dataset with the average of each variable for each activity and each subject. We create first a variable contining each combination of activity and subject, then aggreagate the dataset by the variable we created, storing the mean of values of each other variable per value of the new variable. Finally, we return the variables "subject" and "activities" to their original form.
library(tidyr); library(dplyr)
test_train_msd <- mutate(test_train_msd, sub.act = paste(subject,activities, sep="."))
testrain_reduced <- aggregate(test_train_msd[,3:81], list(test_train_msd$sub.act), mean)
testrain_reduced <- separate(data = testrain_reduced, col = Group.1, into = c("subject", "activities"), sep =  "\\.")

##### Saving testrain_reduced as a file
write.table(testrain_reduced, file = "./run_analysis/humanact_reduced.txt")




