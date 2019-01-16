merge_data <- function (){
  
  library(reshape2)
 ## Create Path Variables  
  src_base_dir <- "D:\\RData\\Assignment_Data_Course_3\\UCI HAR Dataset"
  src_test_dir <- "test"
  src_train_dir <- "train"
  
  
  
  ## ------------------------------------------ STEP 1 of the Assignment ----------------------------------------------------
  # ****           Merges the training and the test sets to create one data set.
  
  # Load Features File and Add Column Name
  features_file <- paste(src_base_dir, "features.txt" , sep="\\")
  features <- read.table(features_file , header = FALSE)
  names(features) <- c("Feature ID" , "Feature")
  
  # Load Activity Label File and Add Column Name
  act_lbl_file <- paste(src_base_dir, "activity_labels.txt" , sep="\\")
  act_lbl_tbl <- read.table(act_lbl_file , header = FALSE)
  names(act_lbl_tbl) <- c("Activity_ID" , "Activity")
  
  # -------------------------------------------- Loading Test File  -------------------------------
  
  # Load Subject Test File and Add Column Name 
  subject_test_file <- paste(src_base_dir,src_test_dir, "subject_test.txt" , sep="\\")
  subject_test_tbl <- read.table(subject_test_file , col.names = as.list(features[1]))
  names(subject_test_tbl) <- "Subject_ID"
  
  #Load X Test File and Assing Column Name Features File
  X_test_file <- paste(src_base_dir, src_test_dir ,"X_test.txt" , sep="\\")
  X_test_tbl <- read.table(X_test_file , header = FALSE)
  names(X_test_tbl) <- features$Feature
  
  
  #Load Y Test File and Assing Column Name 
  #1 WALKING
  #2 WALKING_UPSTAIRS
  #3 WALKING_DOWNSTAIRS
  #4 SITTING
  #5 STANDING
  #6 LAYING
  
  y_test_file <- paste(src_base_dir, src_test_dir ,"y_test.txt" , sep="\\")
  y_test_tbl <- read.table(y_test_file , header = FALSE)
  names(y_test_tbl) <- "Activity_ID"
  
 
  merged_test <- cbind(X_test_tbl,y_test_tbl,subject_test_tbl)
 
  # -------------------------------------------- Loading Train File  -------------------------------
  
  # Load Subject train File and Add Column Name 
  subject_train_file <- paste(src_base_dir,src_train_dir, "subject_train.txt" , sep="\\")
  subject_train_tbl <- read.table(subject_train_file , col.names = as.list(features[1]))
  names(subject_train_tbl) <- "Subject_ID"
  
  #Load X train File and Assing Column Name Features File
  X_train_file <- paste(src_base_dir, src_train_dir ,"X_train.txt" , sep="\\")
  X_train_tbl <- read.table(X_train_file , header = FALSE)
  names(X_train_tbl) <- features$Feature
  
  
  #Load Y train File and Assing Column Name 
  #1 WALKING
  #2 WALKING_UPSTAIRS
  #3 WALKING_DOWNSTAIRS
  #4 SITTING
  #5 STANDING
  #6 LAYING
  
  y_train_file <- paste(src_base_dir, src_train_dir ,"y_train.txt" , sep="\\")
  y_train_tbl <- read.table(y_train_file , header = FALSE)
  names(y_train_tbl) <- "Activity_ID"
  
  
  merged_train <- cbind(X_train_tbl,y_train_tbl,subject_train_tbl)
  
  # COMBINE The TEST and TRAIN Data
  
  combined_data <- rbind(merged_test , merged_train)
  
  ## ------------------------------------------ STEP 2 of the Assignment ----------------------------------------------------
  # ****           Extracts only the measurements on the mean and standard deviation for each measurement.
  
  # Extranct the Mean Columns of  Data and Store in Logical Vector
  
  desired_cols <- grepl ("mean\\(\\)" , names(combined_data)) | grepl ("std\\(\\)" , names(combined_data)) 
  
  # Add the Activity and Subject ID Columns to the COL List Logical Vector
  desired_cols[562:563] <- TRUE
  
  # Store the Desired COlumns and Activity and Subject ID to the Combined Data Table
  combined_data <- combined_data[,desired_cols]
  
  head(combined_data)
  
  
  ## ------------------------------------------ STEP 3 of the Assignment ----------------------------------------------------
  # ****            Uses descriptive activity names to name the activities in the data set
  
  # Descriptive COlumn Names: Column Name have already been assigned in STEP 1
  
  
  
  ## ------------------------------------------ STEP 4 of the Assignment ----------------------------------------------------
  # ****            Appropriately labels the data set with descriptive variable names.
  
  
  # Create Factor Variable for the List of Activities and Assign to the Combined Data Table Column 
  
   combined_data$Activity_ID <- factor(combined_data$Activity_ID , labels = c("Walking","Walking_Upstairs", "Walking_Downstairs", "Sitting", "Standing", "Laying"))
   
  
  
  
  ## ------------------------------------------ STEP 4 of the Assignment ----------------------------------------------------
  # ****            From the data set in step 4, creates a second, independent tidy data set with 
  # ****                  the average of each variable for each activity and each subject
  
  
  melted_combined_data <- melt(combined_data, id=c("Subject_ID","Activity_ID"))
  tidy_data <- dcast(melted_combined_data, Subject_ID+Activity_ID ~ variable, mean)
  
  # Write Data to Target File
  tgt_path <- paste (src_base_dir , "tidy_data.csv" , sep = "\\")
  write.csv(tidy_data,tgt_path, row.names=FALSE)
  
  

  
  
  }

