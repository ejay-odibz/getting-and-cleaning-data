
## Coursera Getting and Cleaning Data Course Project
# 1. Merge the training and the test sets to create one data set.

setwd('~/UCI HAR Dataset/');

# Read in the train data from files
features     <- read.table('./features.txt',header=FALSE); 
activity_type <-  read.table('./activity_labels.txt',header=FALSE); 
subject_train <-  read.table('./train/subject_train.txt',header=FALSE); 
x_train       <-  read.table('./train/x_train.txt',header=FALSE); #
y_train       <-  read.table('./train/y_train.txt',header=FALSE); #
# Assign column names to the  imported data
colnames(activity_type)  <- c('activityId','activityType');
colnames(subject_train)  <- "subjectId";
colnames(x_train)        <- features[,2]; 
colnames(y_train)        <- "activityId";
# merge training set 
training_data <-  cbind(y_train,subject_train,x_train);

# Read in the test data
subject_test <-  read.table('./test/subject_test.txt',header=FALSE); 
x_test       <-  read.table('./test/x_test.txt',header=FALSE); 
y_test       <-  read.table('./test/y_test.txt',header=FALSE); 
# Assign column names to the imported test data
colnames(subject_test) <-  "subjectId";
colnames(x_test)       <-  features[,2]; 
colnames(y_test)       <-  "activityId";
# merge the test set
test_data <-  cbind(y_test,subject_test,x_test);

# Combine training and test data to create a new data set
test_train_data <-  rbind(training_data,test_data);

# Create a vector for the column names from the test_train_data set to select the 
#desired mean() & stddev() columns
col_names  <- colnames(test_train_data); 

# 2. Extracting the mean and standard deviation for each measurement. 

# Create a logical Vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE
#for others
logical_vector_true <-  (grepl("activity..",col_names) | grepl("subject..",col_names) | grepl("-mean..",col_names) & !grepl("-meanFreq..",col_names) & !grepl("mean..-",col_names) | grepl("-std..",col_names) & !grepl("-std()..-",col_names));

# Subset test_train_data table to keep only desired columns
test_train_data <- test_train_data[logical_vector_true==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the test_train_data set with the acitivityType table to include descriptive activity names
test_train_data <-  merge(test_train_data,activity_type,by='activityId',all.x=TRUE);

# Updating the col_names vector to include the new column names after merge
col_names  <-  colnames(test_train_data); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(col_names)) 
{
  col_names[i] <-  gsub("\\()","",col_names[i])
  col_names[i] <-  gsub("-std$","StdDev",col_names[i])
  col_names[i] <-  gsub("-mean","Mean",col_names[i])
  col_names[i] <-  gsub("^(t)","time",col_names[i])
  col_names[i] <-  gsub("^(f)","freq",col_names[i])
  col_names[i] <-  gsub("([Gg]ravity)","Gravity",col_names[i])
  col_names[i] <-  gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_names[i])
  col_names[i] <-  gsub("[Gg]yro","Gyro",col_names[i])
  col_names[i] <-  gsub("AccMag","AccMagnitude",col_names[i])
  col_names[i] <-  gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",col_names[i])
  col_names[i] <-  gsub("JerkMag","JerkMagnitude",col_names[i])
  col_names[i] <-  gsub("GyroMag","GyroMagnitude",col_names[i])
};

# Reassigning the new descriptive column names to the test_train_data set
colnames(test_train_data) <-  col_names;

# 5. Create a second, independent tidy data set with the average of each variable for 
#each activity and each subject. 

# Create a new table without the activityType column
no_activity_type  <- test_train_data[,names(test_train_data) != 'activityType'];
# Summarizing the New table to include just the mean of each variable for each
#activity and each subject
tidy_data <-  aggregate(no_activity_type[,names(no_activity_type) != c('activityId','subjectId')],by=list(activityId=no_activity_type$activityId,subjectId = no_activity_type$subjectId),mean);
# Merging the tidyData with activityType to include descriptive acitvity names
tidy_data <-  merge(tidy_data,activity_type,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidy_data, './tidyData.txt',row.names=FALSE,sep='\t')