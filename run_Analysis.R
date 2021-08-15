## Date: 15/08/2021
## Author: H.Smit

## OVERVIEW
# Work with data collected from Samsung S dataset, to make it clean and tidy
# Save the tidy data to "tidy_data.txt"
# See README.TXT for details

# Set working directory
setwd("C://Users/smi06688/Desktop/Getting and cleaning data")
# Create variable for URL
URLzip <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# Create variable for file
Filezip <- "UCI HAR Dataset.zip"

# # GET DATA
if(!file.exists(Filezip)){
       download.file(URLzip,Filezip, mode = "wb")
}

Pathdata <- "UCI HAR Dataset"


if(!file.exists(Pathdata)){
  
  unzip(Filezip)
}

# # READ THE DATA

TrainSubject <- read.table(file.path(Pathdata,"train","subject_train.txt"))
TrainValues <- read.table(file.path(Pathdata,"train","X_train.txt"))
TrainActivity <- read.table(file.path(Pathdata,"train","y_train.txt"))

TestSubjects <- read.table(file.path(Pathdata,"test","subject_test.txt"))
TestValues <- read.table(file.path(Pathdata,"test","X_test.txt"))
Testactivity <- read.table(file.path(Pathdata,"test","y_test.txt"))

features <- read.table(file.path(Pathdata,"features.txt"), as.is = TRUE)
Activities <- read.table(file.path(Pathdata,"activity_labels.txt"))
colnames(Activities) <- c("activityId","activityLabel")

# Merges the training and the test sets to create one data set.
humanActivity <- rbind(
  
                        cbind(TrainSubject,TrainValues,TrainActivity),
                        cbind(TestSubjects,TestValues,Testactivity)
)


# Remove single data tables to save memory
rm(TrainSubject,TrainValues,TrainActivity,TestSubjects,TestValues,Testactivity)

# Create Column names
colnames(humanactivity) <- c("Subject",features[,2],"Activity")

# Determine which columns to keep
columnsToKeep <- grepl("Subject|Activity|Mean|StD",colnames(humanActivity))
# Keep these columns only
humanActivity <- humanActivity[,columnsToKeep]

# replace activity values with name factor levels
humanActivity$Activity <- factor(humanActivity$Activity,levels = Activities[,1],labels = Activities[,2])

# Get column names
humanActivityCols <- colnames(humanActivity)

# Remove special characters
humanActivityCols <- gsub("[\\(\\)-]","",humanActivityCols)
# Expand abbreviations and clean up mames
humanActivityCols <- gsub("^f","frequencyDomain",humanActivityCols)
humanActivityCols <- gsub("^t","timeDomain",humanActivityCols)
humanActivityCols <- gsub("Acc]","Accelerometer",humanActivityCols)
humanActivityCols <- gsub("Gyro","Gyroscope",humanActivityCols)
humanActivityCols <- gsub("Mag","Magnitude",humanActivityCols)
humanActivityCols <- gsub("Freq","Frequency",humanActivityCols)
humanActivityCols <- gsub("mean","Mean",humanActivityCols)
humanActivityCols <- gsub("std","StandardDeviation",humanActivityCols)

# #  Correct typing mistake
humanActivityCols <- gsub("BodyBody","Body",humanActivityCols)

# # Use new labels as column names
colnames(humanActivity) <- humanActivityCols

# # Group by subject and activity and summarize by using mean
humanActivityMeans <- humanActivity %>%
  group_by(Subject,Activity)  %>%
  summarise_each(funs(mean))

  # #  Output file to " tidy_data.txt"
write.table(humanActivityMeans,"tidy_data.txt",row.names = FALSE,quote = FALSE)

