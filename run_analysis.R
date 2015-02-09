library(data.table)
library(reshape2)

# 1. Merges the training and the test sets to create one data set.
bindDataSets <- function() 
{
    x_train <- read.table("train/X_train.txt")
    x_test <- read.table("test/X_test.txt")
    x <<- rbind(x_train, x_test)
    
    y_train <- read.table("train/y_train.txt")
    y_test <- read.table("test/y_test.txt")
    y <<- rbind(y_train, y_test)
    
    subject_train <- read.table("train/subject_train.txt")
    subject_test <- read.table("test/subject_test.txt")
    subject <<- rbind(subject_train, subject_test)
}

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
extractMeanAndStd <- function() 
{
    features <- read.table("features.txt")

    #find features with mean and std
    msFeatures <- grep("(mean\\(\\)|std\\(\\))", features[, 2])
    
    #extraction:
    x <<- x[, msFeatures]
    
    #fix column names
    newColNames <<- features[msFeatures, 2]
    newColNames <<- gsub("mean", "Mean", newColNames)
    newColNames <<- gsub("std", "Std", newColNames)
    newColNames <<- gsub("\\(\\)", "", newColNames)
    
    names(x) <<- newColNames
}

# 3. Uses descriptive activity names to name the activities in the data set
nameActivities <- function() 
{
    activities <- read.table("activity_labels.txt")

    # update values with correct activity names
    y[, 1] <<- activities[y[, 1], 2]
    
}

# 4. Appropriately labels the data set with descriptive variable names. 
labelColumnsAndBind <- function() 
{
    #change column name
    names(y) <- "activity"
    
    #change column name
    names(subject) <- "subject"
    
    #bind all the data in a single data set
    cbind(x, y, subject)
}

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
#for each activity and each subject.

calculateMeans <- function(allData) 
{
    idLabels <- c("activity", "subject")
    melted <- melt(allData, id = idLabels, measure.vars = newColNames)
    dcast(melted, subject + activity ~ variable, mean)    
}

#call all sub-steps to analyse data
analyseSamsungData <- function()
{
    bindDataSets()
    extractMeanAndStd()
    nameActivities()
    allData <- labelColumnsAndBind()
    result <- calculateMeans(allData)
    write.csv(result, "result.txt", row.names = FALSE)
}

#call main function
analyseSamsungData()
