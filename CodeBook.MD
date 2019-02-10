## Description

This code book describes the data and the variables in the tidyData.txt file. The file is the output of  the assignement Week 4 for the Coursera Data Science Track course Getting & Cleaning Data 

## Study
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 


The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

For each record in the dataset it is provided: 
* Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
* Triaxial Angular velocity from the gyroscope. 
* A 561-feature vector with time and frequency domain variables. 
* Its activity label. 
* An identifier of the subject who carried out the experiment.



The data were downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 


The following tasks were completed

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### 1. Merges the training and the test sets to create one data set.

The following files are imported:

*'features.txt'
*'activity_labels.txt'
*'subject_train.txt'
*'x_train.txt'
*'y_train.txt'
*'subject_test.txt'
*'x_test.txt'
*'y_test.txt

Firstly, the features and activity labels are imported and column names are added.
Next, the TRAINING files set and the TEST files set are imported and merged into one dataset 

### 2. Extracts only the measurements on the mean and standard deviation for each measurement.

From the dataset, the ID, mean and standard deviations are extracted.

### 3. Uses descriptive activity names to name the activities in the data set
activity_labels.txt' is merged with the subsetted data to add descriptive activity names to merged and subsetted data set. Values in 'activityId' column are then replaced with the matching values from the 'activityType' column in order to make the data easier to read.

### 4. Appropriately labels the data set with descriptive variable names.

The gsub function is used to properly named the column names.
The 'activityType' column is removed because it is not relevant for the tidy dataset. 


### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
The data are aggregated by activity and subject and a mean is computed. The resulted  data set is stored in a file FinalTidyData.txt