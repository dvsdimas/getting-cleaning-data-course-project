require(readr)
require(dplyr)
require(tidyr)
require(purrr)

if(!exists("load_dataset", mode = "function")) source("load.R")
    

# 0) Load data

data_folder <- file.path(getwd(), "UCI HAR Dataset")

if(!dir.exists(data_folder)){
    stop(paste0(data_folder, " doen't exist !")) # !!!!!!!!!!!!!!!!!!!! TODO download zip
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

# str(tidy.data)

# 'data.frame':	679734 obs. of  10 variables:
# $ subject    : Factor w/ 30 levels "1","2","3","4",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ activity   : Factor w/ 6 levels "LAYING","SITTING",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ value      : num  0.257 0.286 0.275 0.27 0.275 ...
# $ domain     : Factor w/ 2 levels "FREQUENCY","TIME": 2 2 2 2 2 2 2 2 2 2 ...
# $ device     : Factor w/ 2 levels "ACCELEROMETER",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ instrument : Factor w/ 2 levels "BODY","GRAVITY": 1 1 1 1 1 1 1 1 1 1 ...
# $ calculation: Factor w/ 2 levels "MEAN","SD": 1 1 1 1 1 1 1 1 1 1 ...
# $ jerk       : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ magnitude  : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ axis       : Factor w/ 4 levels NA,"X","Y","Z": 2 2 2 2 2 2 2 2 2 2 ...

# head(tidy.data)

# subject activity     value domain        device instrument calculation  jerk magnitude axis
# 1       2 STANDING 0.2571778   TIME ACCELEROMETER       BODY        MEAN FALSE     FALSE    X
# 2       2 STANDING 0.2860267   TIME ACCELEROMETER       BODY        MEAN FALSE     FALSE    X
# 3       2 STANDING 0.2754848   TIME ACCELEROMETER       BODY        MEAN FALSE     FALSE    X
# 4       2 STANDING 0.2702982   TIME ACCELEROMETER       BODY        MEAN FALSE     FALSE    X
# 5       2 STANDING 0.2748330   TIME ACCELEROMETER       BODY        MEAN FALSE     FALSE    X
# 6       2 STANDING 0.2792199   TIME ACCELEROMETER       BODY        MEAN FALSE     FALSE    X



# 5) From the data set in step 4, creates a second, independent tidy data set with 
#    the average of each variable for each activity and each subject.

tidy.data.avg <- tidy.data %>%
    group_by(subject, activity, domain, device, instrument, jerk, magnitude, calculation, axis) %>%
    summarise(count = n(), average = mean(value))

# str(tidy.data.avg)

# Classes ‘grouped_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':	11880 obs. of  11 variables:
# $ subject    : Factor w/ 30 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ activity   : Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ domain     : Factor w/ 2 levels "FREQUENCY","TIME": 1 1 1 1 1 1 1 1 1 1 ...
# $ device     : Factor w/ 2 levels "ACCELEROMETER",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ instrument : Factor w/ 2 levels "BODY","GRAVITY": 1 1 1 1 1 1 1 1 1 1 ...
# $ jerk       : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ magnitude  : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ calculation: Factor w/ 2 levels "MEAN","SD": 1 1 1 2 2 2 1 2 1 1 ...
# $ axis       : Factor w/ 4 levels NA,"X","Y","Z": 2 3 4 2 3 4 1 1 2 3 ...
# $ count      : int  50 50 50 50 50 50 50 50 50 50 ...
# $ average    : num  -0.939 -0.867 -0.883 -0.924 -0.834 ...


# head(tidy.data.avg)

# Source: local data frame [6 x 11]
# Groups: subject, activity, domain, device, instrument, jerk, magnitude, calculation [2]

# subject activity    domain        device instrument  jerk magnitude calculation   axis count    average
# <fctr>   <fctr>    <fctr>        <fctr>     <fctr> <lgl>     <lgl>      <fctr> <fctr> <int>      <dbl>
# 1       1   LAYING FREQUENCY ACCELEROMETER       BODY FALSE     FALSE        MEAN      X    50 -0.9390991
# 2       1   LAYING FREQUENCY ACCELEROMETER       BODY FALSE     FALSE        MEAN      Y    50 -0.8670652
# 3       1   LAYING FREQUENCY ACCELEROMETER       BODY FALSE     FALSE        MEAN      Z    50 -0.8826669
# 4       1   LAYING FREQUENCY ACCELEROMETER       BODY FALSE     FALSE          SD      X    50 -0.9244374
# 5       1   LAYING FREQUENCY ACCELEROMETER       BODY FALSE     FALSE          SD      Y    50 -0.8336256
# 6       1   LAYING FREQUENCY ACCELEROMETER       BODY FALSE     FALSE          SD      Z    50 -0.8128916


