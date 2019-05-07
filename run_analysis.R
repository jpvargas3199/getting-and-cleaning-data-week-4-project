library(plyr)

# Download the dataset
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileurl, destfile = "/Users/user/week4assigment.zip")

# Unzip the dataset
  unzip(zipfile = "/Users/user/week4assigment.zip", exdir = "/Users/user/")

# Read sets

  # Train data
    set_train <- read.table("/Users/user/UCI HAR Dataset/train/X_train.txt")
    labels_train <- read.table("/Users/user/UCI HAR Dataset/train/y_train.txt")
    subject_train<-read.table("/Users/user/UCI HAR Dataset/train/subject_train.txt")
  
  # Test data
    set_test<-read.table("/Users/user/UCI HAR Dataset/test/X_test.txt")
    labels_test<-read.table("/Users/user/UCI HAR Dataset/test/y_test.txt")
    subjects_test<-read.table("/Users/user/UCI HAR Dataset/test/subject_test.txt")
  
  # Activity Labels & Features
    activity_labels<-read.table("/Users/user/UCI HAR Dataset/activity_labels.txt")
    features<-read.table("/Users/user/UCI HAR Dataset/features.txt")
  
#Assign Variables
  
  #Train
    colnames(labels_train)<-"activityID"
    colnames(set_train)<-features[,2]
    colnames(subject_train)<-"subjectID"
    
  #Test
    colnames(labels_test)<-"activityID"
    colnames(set_test)<-features[,2]
    colnames(subjects_test)<-"subjectID"
    
  #Activity Labels
    colnames(activity_labels) <- c("activityID", "activityType")
    
# Merges the training and the test sets to create one data set
  train<-cbind(labels_train,subject_train,set_train)
  test<-cbind(labels_test,subjects_test,set_test)
  finaldataset<-rbind(train,test)
    
# Extracts only the measurements on the mean and standard deviation for each measurement
  Mean_std<-grepl("subjectID|activityID|mean|std",colnames(finaldataset))
  finaldataset<-finaldataset[,Mean_std==TRUE]
    
# Replace activity values with named factor levels
  finaldataset$activityID <- factor(finaldataset$activityID, 
  levels = activity_labels[, 1], labels = activity_labels[, 2])
    
# Appropriately labels the data set with descriptive variable names
    
  #remove special characters
    dataCol<-colnames(finaldataset)
    dataCol<-gsub("[\\(\\)-]", "", dataCol)
    
  #clean and set up expanded names
    dataCol <- gsub("^f", "frequencyDomain", dataCol)
    dataCol <- gsub("^t", "timeDomain", dataCol)
    dataCol <- gsub("Acc", "Accelerometer", dataCol)
    dataCol <- gsub("Gyro", "Gyroscope", dataCol)
    dataCol <- gsub("Mag", "Magnitude", dataCol)
    dataCol <- gsub("Freq", "Frequency", dataCol)
    dataCol <- gsub("mean", "Mean", dataCol)
    dataCol <- gsub("std", "StandardDeviation", dataCol)
    dataCol <- gsub("BodyBody","Body",dataCol)
      
    #set new column names
    colnames(finaldataset)<-dataCol
      
# Creating a second,  independent tidy data set with the avg of each variable for each activity and subject   
    #Group by subject and activity and summarise using mean
    tidySet <- aggregate(. ~subjectID + activityID, finaldataset, mean)
    tidySet <- tidySet[order(tidySet$subjectID, tidySet$activityID), ]
    #Create new data set
    write.table(tidySet,"tidySet.txt",row.names = FALSE)