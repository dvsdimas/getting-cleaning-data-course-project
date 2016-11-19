require(readr)
require(dplyr)
require(tidyr)
require(purrr)

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

features <- data_description$features %>%
                mutate(index = paste0("V", id)) %>%
                filter(grepl("mean\\(\\)|std\\(\\)", name))

keys <- c(1, 2, sapply(features$id, function(x) { x + 2 } ))

fdata <- select(data, keys)


# 3) Uses descriptive activity names to name the activities in the data set

fdata <- fdata %>%
    left_join(data_description$activity, by = c("activitynum" = "id")) %>%
    select(-activitynum)

fdata$activity <- factor(fdata$activity)

fdata$subject <- factor(fdata$subject)


# 4) Appropriately labels the data set with descriptive variable names.

tfdata <- fdata %>% 
    gather(feature, value, one_of(features$index), -subject, -activity) %>% 
    left_join(features, by = c("feature" = "index")) %>%
    select(-feature, -id)


# parse column 'name'

domain <- map_chr(tfdata$name, function(x) { ifelse(grepl("^t", x), "TIME", "FREQUENCY") })

tidy.data <- cbind(tfdata, domain = as.factor(domain))
    

device <- map_chr(tfdata$name, function(x) { ifelse(grepl("Gyro", x), "GYROSCOPE", "ACCELEROMETER") }) 
    
tidy.data <- cbind(tidy.data, device = as.factor(device))


instrument <- map_chr(tfdata$name, function(x) { ifelse(grepl("GravityAcc", x), "GRAVITY", "BODY") }) 

tidy.data <- cbind(tidy.data, instrument = as.factor(instrument))


calculation <- map_chr(tfdata$name, function(x) { ifelse(grepl("mean()", x), "MEAN", "SD") }) 

tidy.data <- cbind(tidy.data, calculation = as.factor(calculation))


jerk <- map_lgl(tfdata$name, function(x) { ifelse(grepl("Jerk", x), TRUE, FALSE) }) 

tidy.data <- cbind(tidy.data, jerk = jerk)


magnitude <- map_lgl(tfdata$name, function(x) { ifelse(grepl("Mag", x), TRUE, FALSE) }) 

tidy.data <- cbind(tidy.data, magnitude = magnitude)


axis <- map_chr(tfdata$name, function(x) { 
    
    if(grepl("-X$", x)) {
        "X" 
    } else if(grepl("-Y$", x)) {
        "Y"
    } else if(grepl("-Z$", x)) {
        "Z"
    } else {
        NA
    }  
    
    }) 

tidy.data$axis = factor(axis, c(NA, "X", "Y", "Z"), exclude = NULL)

tidy.data <- select(tidy.data, -name)


# 5) From the data set in step 4, creates a second, independent tidy data set with 
#    the average of each variable for each activity and each subject.





















