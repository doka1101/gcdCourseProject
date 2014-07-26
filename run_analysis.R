# Coursera Getting And Cleaning Data Project
# Merge two data sets (training and test), and extract the
# average and standard deviation columns.
# Also, replace the column names by descriptive variable names.

run_analysis <- function() {
    #setting up working directory
    setwd("C:/Coursera/Data Science Track/Getting And Cleaning Data/Project")
    
    #reading test and training data
    test_data <- as.data.frame(read.table("./test/X_test.txt"))
    train_data <- as.data.frame(read.table("./train/X_train.txt"))
    
    #merging two data sets
    tt_data <- merge(test_data, train_data, all = TRUE)
    
    #obtaining column names from seperate text file.
    name_file <- as.data.frame(read.table("features.txt"))
    cNames <- name_file$V2
    
    #naming the columns
    colnames(tt_data) <- cNames
    
    #now we need to extract the mean and std columns from the merge data sets.
    #find the column indices of mean and std.
    ind_lst <- grep(pattern = "mean\\(|std\\(", names(tt_data))
    
    final_data <- tt_data[, ind_lst]
    
    #renaming column names.
    new_col_name <- gsub("tBodyAcc-","body-acceleration-time-", 
                         names(final_data))
    new_col_name <- gsub("fBodyAcc-","body-acceleration-frequency-", 
                         new_col_name)
    new_col_name <- gsub("tGravityAcc-", "gravity-acceleration-time-", 
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("fGravityAcc-", "gravity-acceleration-frequency-", 
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("tBodyAccJerk-", "body-acceleration-jerk-time-",
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("fBodyAccJerk-", "body-acceleration-jerk-frequency-",
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("tBodyGyro-", "body-gyroscope-time-",
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("fBodyGyro-", "body-gyroscope-frequency-",
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("tBodyGyroJerk-", "body-gyroscope-jerk-time-",
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("fBodyGyroJerk-", "body-gyroscope-jerk-frequency-",
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("tBodyAccMag-", "body-acceleration-magnitude-time-",
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("fBodyAccMag-", "body-acceleration-magnitude-frequency-",
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("tGravityAccMag-", "gravity-acceleration-magnitude-time-",
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("fGravityAccMag-", 
                         "gravity-acceleration-magnitude-frequency-",
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("fBodyBodyAccJerkMag-", 
                         "body-acceleration-jerk-magnitude-frequency-",
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("fBodyBodyGyroMag-", 
                         "body-gyroscope-magnitude-frequency-",
                         new_col_name, fixed = TRUE)
    new_col_name <- gsub("fBodyBodyGyroJerkMag-", 
                         "body-gyroscope-jerk-magnitude-frequency-",
                         new_col_name, fixed = TRUE)
    
    #remove "()"
    new_col_name <- gsub("()", "", new_col_name, fixed = TRUE)
    
    colnames(final_data) <- new_col_name
    
    #reading test and train activity label data
    test_act_data <- as.data.frame(read.table("./test/y_test.txt"))
    train_act_data <- as.data.frame(read.table("./train/y_train.txt"))
    
    #merging two acitivity label data
    tt_act_data <- rbind(test_act_data, train_act_data)
    
    
    #replace label number with corresponding activity
    tt_act_data <- as.data.frame(gsub("1", "WALKING", tt_act_data[,1]))
    tt_act_data <- as.data.frame(gsub("2", "WALKING_UPSTAIRS", tt_act_data[,1]))
    tt_act_data <- as.data.frame(gsub("3", "WALKING_DOWNSTAIRS", tt_act_data[,1]))
    tt_act_data <- as.data.frame(gsub("4", "SITTING", tt_act_data[,1]))
    tt_act_data <- as.data.frame(gsub("5", "STANDING", tt_act_data[,1]))
    tt_act_data <- as.data.frame(gsub("6", "LAYING", tt_act_data[,1]))
        
    #appending activity to the mean and st.dev dataset.
    colnames(tt_act_data) <- "activity"
    
    #reading test and train subject data
    test_subj_data <- as.data.frame(read.table("./test/subject_test.txt"))
    train_subj_data <- as.data.frame(read.table("./train/subject_train.txt"))
    
    #merging two acitivity label data
    tt_subj_data <- rbind(test_subj_data, train_subj_data)
    colnames(tt_subj_data) <- "subject"
        
    #merging all the data.
    final_data <- cbind(final_data, tt_act_data)
    final_data <- cbind(final_data, tt_subj_data)
    
    #generate tidy data set.
    tidy_set <- aggregate(final_data[,1:66], 
            by= list(final_data$activity, final_data$subject),FUN = mean)
    
    cNames <- names(tidy_set)
    cNames[1] = "activity"
    cNames[2] = "subject"
    
    colnames(tidy_set) <- cNames
    
    #saving data set to a text file.
    write.table(tidy_set, "tidy_data.txt", sep=",", row.names = FALSE)
}
