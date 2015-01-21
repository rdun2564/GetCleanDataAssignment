setwd("~/RData4/GDProject/UCIHARDataset")
library(dplyr)
# Read in activity labels
activityLabels <- read.table("activity_labels.txt", header = FALSE)
activityLabels # check activity labels imported correctly

# Read in vector containing original Variable Names
y <- read.table("features.txt", header = FALSE)
str(y) # confirm 561 variable names
# Adjust Variable names by removing parentheses and commas
yAdjusted <- gsub("\\()", "", y$V2)
yAdjusted <- gsub("\\(", "", yAdjusted)
yAdjusted <- gsub("\\)", "", yAdjusted)
yAdjusted <- gsub("\\,", "", yAdjusted)
str(yAdjusted) # confirm 561 variable names
# Then create vector with variable names representing means
varWithMean <- grep("[Mm]ean", yAdjusted, value = TRUE)
# Then create vector with variable names representing stds
varWithStd <- grep(("std"), yAdjusted, value = TRUE)
# Combine vectors varWithMean and varWithStd
varNames <- c(varWithMean, varWithStd)
str(varNames) #variable names to be selected in the dataset (#86)

# Read in X_train.txt dataset
xTrain <- read.table("./train/X_train.txt", header = FALSE)
dim(xTrain) #confirm 561 columns
names(xTrain) <- yAdjusted # Rename V variables to names in yAdjust vector
# Select only those columns which have Mean or Std in their name
xTrainDataset <- xTrain[, varNames]

# Read in subject training Data
subjectTrain <- read.table("./train/subject_train.txt", header = FALSE)
dim(subjectTrain) # confirms number of rows as 7352
unique(subjectTrain$V1) # Confirm 30 subjects
names(subjectTrain) <- "subject"  # Rename variable name V1 to Subject 

# Read in y-training data
yTrain <- read.table("./train/y_train.txt", header = FALSE)
dim(yTrain)  # confirms number of rows as 7352
table(yTrain)  # Examine distribution of yTrain values
# Merge activity Labels and yTrain Datasets
activity <- merge(yTrain, activityLabels, by ="V1")
dim(activity) # Confirm number of rows
table(activity$V2) #Examine activity$V2 variable
names(activity) <- c("activityCode", "activity")  # Rename variables V1 and V2 to activityCode and activity

# Combine columns for subject, activity and XTrain dataframes for Training Data
trainDataset <- cbind(subjectTrain, activity, xTrainDataset)
trainDataset$datasource <- "train"
dim(trainDataset)

#
# Code to create testDataset
#
# Read in X_test.txt dataset
xTest <- read.table("./test/X_test.txt", header = FALSE)
dim(xTest) #confirm 561 columns
names(xTest) <- yAdjusted # Rename V variables to names in yAdjust vector
# Select only those columns which have Mean or Std in their name
xTestDataset <- xTest[, varNames]

# Read in subject test Data
subjectTest <- read.table("./test/subject_test.txt", header = FALSE)
dim(subjectTest) # confirms number of rows
unique(subjectTest$V1) # Confirm number of subjects
names(subjectTest) <- "subject"  # Rename variable name V1 to Subject 

# Read in y-test data
yTest <- read.table("./test/y_test.txt", header = FALSE)
dim(yTest)  # confirms number of rows as 7352
table(yTest)  # Examine distribution of yTrain values
# Merge activity Labels and yTest Datasets
activityTest <- merge(yTest, activityLabels, by ="V1")
dim(activityTest) # Confirm number of rows
table(activityTest$V2) #Examine activityTest$V2 variable
names(activityTest) <- c("activityCode", "activity")  # Rename variables V1 and V2 to activityCode and activity

# Combine columns for subject, activityTest and xTest dataframes for test Dataset
testDataset <- cbind(subjectTest, activityTest, xTestDataset)
testDataset$datasource <- "test"
dim(testDataset)

# Combine trainDataset and testDataset together using rbind function
samsungDataset <- rbind(trainDataset, testDataset)
dim(samsungDataset)

# new Variable List
newVariableNames <- c("subject","activityCode","activity","tBodyAcc.X.mean","tBodyAcc.Y.mean",
                      "tBodyAcc.Z.mean","tGravityAcc.X.mean","tGravityAcc.Y.mean","tGravityAcc.Z.mean",
                      "tBodyAccJerk.X.mean","tBodyAccJerk.Y.mean","tBodyAccJerk.Z.mean","tBodyGyro.X.mean",
                      "tBodyGyro.Y.mean","tBodyGyro.Z.mean","tBodyGyroJerk.X.mean","tBodyGyroJerk.Y.mean",
                      "tBodyGyroJerk.Z.mean","tBodyAccMag.mean","tGravityAccMag.mean","tBodyAccJerkMag.mean",
                      "tBodyGyroMag.mean","tBodyGyroJerkMag.mean","fBodyAcc.X.mean","fBodyAcc.Y.mean",
                      "fBodyAcc.Z.mean","fBodyAcc.FreqX.mean","fBodyAcc.FreqY.mean","fBodyAcc.FreqZ.mean",
                      "fBodyAccJerk.X.mean","fBodyAccJerk.Y.mean","fBodyAccJerk.Z.mean",
                      "fBodyAccJerk.FreqX.mean","fBodyAccJerk.FreqY.mean","fBodyAccJerk.FreqZ.mean",
                      "fBodyGyro.X.mean","fBodyGyro.Y.mean","fBodyGyro.Z.mean","fBodyGyro.FreqX.mean",
                      "fBodyGyro.FreqY.mean","fBodyGyro.FreqZ.mean","fBodyAccMag.mean",
                      "fBodyAccMag.Freq.mean","fBodyBodyAccJerkMag.mean","fBodyBodyAccJerkMag.Freq.mean",
                      "fBodyBodyGyroMag.mean","fBodyBodyGyroMag.Freq.mean","fBodyBodyGyroJerkMag.mean",
                      "fBodyBodyGyroJerkMag.Freq.mean","angletBodyAccMeangravity",
                      "angletBodyAccJerkMeangravity.mean","angletBodyGyroMeangravity.mean",
                      "angletBodyGyroJerkMeangravity.mean","angleXgravity.mean","angleYgravity.mean",
                      "angleZgravity.mean","tBodyAcc.X.std","tBodyAcc.Y.std","tBodyAcc.Z.std",
                      "tGravityAcc.X.std","tGravityAcc.Y.std","tGravityAcc.Z.std","tBodyAccJerk.X.std",
                      "tBodyAccJerk.Y.std","tBodyAccJerk.Z.std","tBodyGyro.X.std","tBodyGyro.Y.std",
                      "tBodyGyro.Z.std","tBodyGyroJerk.X.std","tBodyGyroJerk.Y.std","tBodyGyroJerk.Z.std",
                      "tBodyAccMag.std","tGravityAccMag.std","tBodyAccJerkMag.std","tBodyGyroMag.std",
                      "tBodyGyroJerkMag.std","fBodyAcc.X.std","fBodyAcc.Y.std","fBodyAcc.Z.std",
                      "fBodyAccJerk.X.std","fBodyAccJerk.Y.std","fBodyAccJerk.Z.std","fBodyGyro.X.std",
                      "fBodyGyro.Y.std","fBodyGyro.Z.std","fBodyAccMag.std","fBodyBodyAccJerkMag.std",
                      "fBodyBodyGyroMag.std","fBodyBodyGyroJerkMag.std","datasource")

# Replace old veriable list with new Variable List
names(samsungDataset) <- newVariableNames

# Remove datasource variable - it is a character vector so a mean cannot be determined
samsungDataset <- samsungDataset[, -c(90)]

# Create summarised samsung dataset where the mean of each variable 
# is calculated for each subject-activity level using group_by function
# followwed by using the summarise_each function from dplyr
samsungGroupBy <- group_by(samsungDataset, subject, activity)
samsungDatasetSummarised <- summarise_each(samsungGroupBy, funs(mean))
write.table(samsungDatasetSummarised, file = "samsungDatasetSummarised.txt", row.name=FALSE)
