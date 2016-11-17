require(readr)
require(dplyr)

if(!exists("load_dataset", mode = "function")) source("load.R")
    

# 0) Load data

data_folder <- file.path(getwd(), "UCI HAR Dataset")

if(!dir.exists(data_folder)){
    stop(paste0(data_folder, " doen't exist !"))
}


data_description <- load_data_description(data_folder)

test_data_list <- load_test_dataset(data_folder)

train_data_list <- load_train_dataset(data_folder)


# 1) Merges the training and the test sets to create one data set.

test_data <- cbind(test_data_list$subject, test_data_list$y, test_data_list$X)
train_data <- cbind(train_data_list$subject, train_data_list$y, train_data_list$X)

data <- tbl_df(rbind(test_data, train_data))


# 2) Extracts only the measurements on the mean and standard deviation for each measurement.


# TODO




# 3) Uses descriptive activity names to name the activities in the data set

# 4) Appropriately labels the data set with descriptive variable names.

# 5) From the data set in step 4, creates a second, independent tidy data set with 
#    the average of each variable for each activity and each subject.















