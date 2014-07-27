#-----------------------------------------------------------------------  
# Goals of this script:
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation 
#       for each measurement. 
# 3. Use descriptive activity names to name the activities in the 
#       data set
# 4. Appropriately label the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of 
#       each variable for each activity and each subject.
#
# Project FAQ:
#   https://class.coursera.org/getdata-005/forum/thread?thread_id=23
#-----------------------------------------------------------------------   
run_analysis <- function()  {
       
    #-----------------------------------------------------------------------
    # TEST Data
    #-----------------------------------------------------------------------
    
    # read test subjects
    subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", quote="\"")
    
    # read test features
    X_test  <- read.table("./UCI HAR Dataset/test/X_test.txt", quote="\"")
    
    # read test labels
    y_test  <- read.table("./UCI HAR Dataset/test/y_test.txt", quote="\"")
    
    #-----------------------------------------------------------------------
    # TRAINING Data
    #-----------------------------------------------------------------------    
    
    # read training subjects
    subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", quote="\"")
    
    # read training features
    X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", quote="\"")
    
    #read training labels
    y_train  <- read.table("./UCI HAR Dataset/train/y_train.txt", quote="\"")
    
    #-----------------------------------------------------------------------   
    # MERGE
    #-----------------------------------------------------------------------   
    
    # merge subjects
    subject_all <- rbind(subject_train, subject_test)
    colnames(subject_all) <- "subject"
    
    # merge features
    X_all <- rbind(X_train, X_test)
    
    #merge labels
    y_all <- rbind(y_train, y_test)
    colnames(y_all) <- c("activity") # rename the column
    
    #-----------------------------------------------------------------------   
    # subset of features that are measurements on the mean and 
    #       standard deviation for each measurement. 
    #-----------------------------------------------------------------------   
    Xsub <- X_all[, c( stdANDmean()[,1] )]
    #  label the data set with descriptive variable names. 
    colnames(Xsub) <- gsub("[)()]", "", stdANDmean()[,2]) # remove brackets from names
    
    #----------------------------------------------------------------------- 
    # descriptive activity names to name the activities in the data set
    #----------------------------------------------------------------------- 
    activity_labels <- read.csv("./UCI HAR Dataset/activity_labels.txt", header=FALSE, sep=" ")    
    y_all$activityDesc <- factor(y_all$activity, labels=activity_labels[,2])
    
    
    theDataSet <- cbind(y_all$activityDesc, subject_all, Xsub)
    colnames(theDataSet) <- c("activitydesc", colnames(theDataSet)[2:68] )
    
    #return( theDataSet )
    
    
    #gsub("[)()]", "", stdANDmean()[,2]) # remove brackets from names
    
    #tidyData <- data.frame(activitydesc = theDataSet$activitydesc, subject = theDataSet$subject)
    
    tidyData <- aggregate(theDataSet[,3], 
                        list(activitydesc=theDataSet$activitydesc, 
                             subject = theDataSet$subject), 
                        mean)
    colnames(tidyData) <- c(colnames(theDataSet)[c(1,2)], colnames(theDataSet)[3])
    
    for (c in 4:68) {
        tmpData <- aggregate(theDataSet[,c], 
                              list(activitydesc=theDataSet$activitydesc, 
                                   subject = theDataSet$subject), 
                              mean)
        colnames(tmpData) <- c(colnames(theDataSet)[c(1,2)], colnames(theDataSet)[c])
        
        tidyData <- merge(x=tidyData, y=tmpData, by.x=c("activitydesc", "subject"), 
                          by.y=c("activitydesc", "subject"))
    }
    
    tidyData <- data.frame( tidyData[with(tidyData, order(subject, activitydesc)), ] )
    
    # write as .txt file as the project submission page would not accept .csv
    write.csv(tidyData, "tidydata.txt", row.names=FALSE)
    #return(tidyData)

}

#-----------------------------------------------------------------------   
# list of features that are either a mean or standard deviaiton value
# returns: a data frame with two columns. First column is the feature 
#       number. Second column feature desciption.
#-----------------------------------------------------------------------   
stdANDmean <- function() {
    
    library(sqldf)
    
    features <- read.table("./UCI HAR Dataset/features.txt", quote="\"")
    colnames(features) <- c("featureNumber", "featureName")
    
    selectedFeatures <- sqldf("select * from features where featureName like '%std()%' OR featureName like '%mean()%' ")
    
    #print(duplicated(selectedFeatures$featureName))
    
    return(selectedFeatures)
}