require(readr)
require(dplyr)
require(tidyr)
require(purrr)
require(knitr)

# set up your working directory

setwd(getwd())


# include functions for loading data in memory, it was made for simplicity

if(!exists("load_dataset", mode = "function")) source("load.R")
    

# 0) Load data

data_folder <- file.path(getwd(), "UCI HAR Dataset")

if(!dir.exists(data_folder)){
    
    data_file <- paste0(data_folder, ".zip")
    
    if(!file.exists(data_file)) {
        
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
                      destfile = data_file)
    
        if(!file.exists(data_file)){
            stop(paste0(data_file, " doen't exist !"))     
        }    
    }
    
    unzip(data_file)
    
    if(!dir.exists(data_folder)) {
        stop(paste0(data_folder, " doen't exist !"))     
    }
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

domain <- map_chr(tfdata$name, function(x) { ifelse(grepl("^t", x), "TIME", "FREQUENCY") })

device <- map_chr(tfdata$name, function(x) { ifelse(grepl("Gyro", x), "GYROSCOPE", "ACCELEROMETER") }) 
    
instrument <- map_chr(tfdata$name, function(x) { ifelse(grepl("GravityAcc", x), "GRAVITY", "BODY") }) 

calculation <- map_chr(tfdata$name, function(x) { ifelse(grepl("mean()", x), "MEAN", "SD") }) 

jerk <- map_lgl(tfdata$name, function(x) { ifelse(grepl("Jerk", x), TRUE, FALSE) }) 

magnitude <- map_lgl(tfdata$name, function(x) { ifelse(grepl("Mag", x), TRUE, FALSE) }) 

axis <- map_chr(tfdata$name, function(x) { 
    
    if(grepl("-X$", x))      "X"  
    else if(grepl("-Y$", x)) "Y"
    else if(grepl("-Z$", x)) "Z"
    else                     NA
        
    }) 

tidy.data <- tfdata %>% 
             select(-name) %>%
                cbind(domain = as.factor(domain),
                      device = as.factor(device),
                      instrument = as.factor(instrument),
                      calculation = as.factor(calculation),
                      jerk = jerk,
                      magnitude = magnitude,
                      axis = factor(axis, c(NA, "X", "Y", "Z"), exclude = NULL) )



# 5) From the data set in step 4, creates a second, independent tidy data set with 
#    the average of each variable for each activity and each subject.

tidy.data.avg <- tidy.data %>%
    group_by(subject, activity, domain, device, instrument, jerk, magnitude, calculation, axis) %>%
    summarise(count = n(), average = mean(value))


# 6) Make codebook.

knit("makeCodebook.Rmd", output="CodeBook.md", quiet=TRUE)



