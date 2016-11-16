require(readr)


load_data_description <- function(data_folder) {
    
    data_folder <- as.character(data_folder)
    
    if( is.na(data_folder) || is.null(data_folder) ) {
        stop(paste0("data_folder cannot be NULL or NA, but is [", data_folder, "]"))
    }
    
    
    # ------------------------------- load activity_labels -------------------------------
    
    activity_path <- file.path(data_folder, "activity_labels.txt")
    
    if(!file.exists(activity_path)){
        stop(paste0(activity_path, " doen't exist !"))
    }
    
    activity <- read_delim(activity_path, delim = " ", col_names = FALSE)
    
    names(activity) <- c("id", "lable")
    
    
    # ------------------------------- load features -------------------------------
    
    features_path <- file.path(data_folder, "features.txt")
    
    if(!file.exists(features_path)){
        stop(paste0(features_path, " doen't exist !"))
    }
    
    features <- read_delim(features_path, delim = " ", col_names = FALSE)
    
    names(features) <- c("id", "name")
    
    
    list(activity = activity, features = features)
}

load_dataset <- function(data_folder, type){
    
    data_folder <- as.character(data_folder)
 
    if( is.na(data_folder) || is.null(data_folder) ) {
        stop(paste0("data_folder cannot be NULL or NA, but is [", data_folder, "]"))
    }
    
    type <- as.character(type)
    
    if( is.na(type) || is.null(type) ) {
        stop(paste0("type cannot be NULL or NA, but is [", type, "]"))
    }
    
    
    folder <- file.path(data_folder, type)
    
    if(!dir.exists(folder)){
        stop(paste0(folder, " doen't exist !"))
    }
    
    
    # ------------------------------- load subject -------------------------------
    
    subject_path <- file.path(folder, paste0("subject_", type, ".txt")) 
    
    if(!file.exists(subject_path)){
        stop(paste0(subject_path, " doen't exist !"))
    }
    
    subject <- read_csv(subject_path, col_names = FALSE)
    
    names(subject) <- c("subject")
    
    
    # ------------------------------- load X -------------------------------
    
    X_path <- file.path(folder, paste0("X_", type, ".txt")) 
    
    if(!file.exists(X_path)){
        stop(paste0(X_path, " doen't exist !"))
    }
    
    X <- read_csv(X_path, col_names = FALSE, col_types = "n")
    
    names(X) <- c("X")
    
    
    # ------------------------------- load y -------------------------------
    
    y_path <- file.path(folder, paste0("y_", type, ".txt") ) 
    
    if(!file.exists(y_path)){
        stop(paste0(y_path, " doen't exist !"))
    }
    
    y <- read_csv(y_path, col_names = FALSE, col_types = "i")
    
    names(y) <- c("y")
    
    
    # ------------------------------- load Inertial Signals -------------------------------
    
    IS_folder <- file.path(folder, "Inertial Signals")
    
    if(!dir.exists(IS_folder)){
        stop(paste0(IS_folder, " doen't exist !"))
    }
    
    load_signals_data <- function(file_name) {
        
        file_path <- file.path(IS_folder, paste0(file_name, "_", type, ".txt")) 
        
        if(!file.exists(file_path)){
            stop(paste0(file_path, " doen't exist !"))
        }
        
        file_data <- read_csv(file_path, col_names = FALSE, col_types = "n")
        
        names(file_data) <- c(file_name)
        
        file_data
    }
    
    cbind(subject,  
          X, 
          y,
         
          load_signals_data("body_acc_x"),
          load_signals_data("body_acc_y"),
          load_signals_data("body_acc_z"),
         
          load_signals_data("body_gyro_x"),
          load_signals_data("body_gyro_y"),
          load_signals_data("body_gyro_z"),
         
          load_signals_data("total_acc_x"),
          load_signals_data("total_acc_y"),
          load_signals_data("total_acc_z")
         )
}


load_test_dataset <- function(data_folder) {
    load_dataset(data_folder, "test")   
}

load_train_dataset <- function(data_folder) {
    load_dataset(data_folder, "train")   
}