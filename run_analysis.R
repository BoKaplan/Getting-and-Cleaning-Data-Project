# Check to see if the user has the "reshape2" package installed (this script relies on it)
if(!library(reshape2, logical.return = TRUE)) {
  # It didn't exist, so install the package, and then load it
  install.packages('reshape2')
  library(reshape2)
}
library(plyr)
library(dplyr)
library(data.table)

#Download dataset
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

#Unzip file
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

#Read Data
  # Read training data
  trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
  trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
  trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

  #Read test data
  testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
  testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
  testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# Step 1 - Merge the training and the test sets to create one data set

  # Bind the rows for each of the data sets together
  data.data <- rbind(trainingValues, testValues)
  data.activities <- rbind(trainingActivity, testActivity)
  data.subjects <- rbind(trainingSubjects, testSubjects)

  # Now combine all of of the different columns together into one table
  full_data <- cbind(data.subjects, data.activities, data.data)


# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement

  #Complete list of features
  features <- read.table(file.path(dataPath, "features.txt"))
  
  #Filter to the features we desire
  requiredFeatures <- features[grep('-(mean|std)\\(\\)', features[, 2 ]), 2]
  full_data <- full_data[,c(1, 2, requiredFeatures)]
  
# Step 3 - Uses descriptive activity names to name the activities in the data set
  
  #Read activity labels
  activities <- read.table(file.path(dataPath, "activity_labels.txt"))
  
  #Update the activity name
  full_data[,2] <- activities[full_data[,2], 2]
  
# Step 4 - Appropriately labels the data set with descriptive variable names
  
  colnames(full_data) <- c(
    'subject',
    'activity',
    #Remove the brackets from the features columns
    gsub('\\-|\\(|\\)', '', as.character(requiredFeatures))
  )
  
  #Coerce the data into strings
  full_data[,2] <- as.character(full_data[,2])
  
# Step 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
  
  #Melt the data to get an unique row of each combination of subject and activities
  final.melt <- melt(full_data, id = c('subject', 'activity'))
  
  #Get the mean value
  final.mean <- dcast(final.melt, subject + activity ~ variable, mean)
  
  #Emit data to file
  write.table(final.mean, file = file.path("tidy.txt"), row.names = FALSE, quote = FALSE)

  