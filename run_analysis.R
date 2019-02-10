library(dplyr)

#set the working directory
setwd("C:/Users/david/Desktop/datascience/src/ProgrammingAssignment4")

# reading of the data description
variable_names <- read.table("./features.txt",header=FALSE)

# reading of the activity labels & add a column name
activity_labels <- read.table("./activity_labels.txt",header=FALSE)
colnames(activity_labels)<- c("activityId","activityType")


#reading of the training data & add a column name
x_train<-read.table("./train/X_train.txt",header=FALSE)
colnames(x_train)<- variable_names[,2];

y_train <- read.table("./train/Y_train.txt",header=FALSE)
colnames(y_train)<- "activityId";


subject_train <- read.table("./train/subject_train.txt",header=FALSE)
colnames(subject_train)<- "subjectId";

# merging of the train Data into a complete training set
train_dataset = cbind(y_train,subject_train,x_train);


#reading of the testing data  & add a column name
x_test<-read.table("./test/X_test.txt",header=FALSE)
colnames(x_test)<- variable_names[,2];

y_test <- read.table("./test/Y_test.txt",header=FALSE)
colnames(y_test)<- "activityId";


subject_test <- read.table("./test/subject_test.txt",header=FALSE)
colnames(subject_test)<- "subjectId";

# merging of the test Data into a complete test set
test_dataset = cbind(y_test,subject_test,x_test);

# merging of the train Data set and the test Data set into a complete test set
full_dataset = rbind(train_dataset,test_dataset);

column_names<-colnames(full_dataset)


# 2 Extracts only the measurements on the mean and standard deviation for each measurement

selected_column_names<- column_names[grep("subjectId|activityId|mean\\(\\)|std\\(\\)",column_names)]

selected_dataset <- full_dataset[,selected_column_names]

#3 Uses descriptive activity names to name the activities in the data set

selected_dataset <- merge(selected_dataset,activity_labels,by='activityId',all.x=TRUE)

selected_dataset$activityId <-activity_labels[,2][match(selected_dataset$activityId, activity_labels[,1])]

selected_column_names<-colnames(selected_dataset)

# 4.Appropriately label the data set with descriptive activity names.
for (i in 1:length(selected_column_names)) 
{
  selected_column_names[i] <- gsub("\\()","",selected_column_names[i])
  selected_column_names[i] <- gsub("-std","StdDev",selected_column_names[i])
  selected_column_names[i] <- gsub("-mean","Mean",selected_column_names[i])
  selected_column_names[i] <- gsub("^(t)","time",selected_column_names[i])
  selected_column_names[i] <- gsub("^(f)","freq",selected_column_names[i])
  selected_column_names[i] <- gsub("([Gg]ravity)","Gravity",selected_column_names[i])
  selected_column_names[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",selected_column_names[i])
  selected_column_names[i] <- gsub("[Gg]yro","Gyro",selected_column_names[i])
  selected_column_names[i] <- gsub("AccMag","AccMagnitude",selected_column_names[i])
  selected_column_names[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",selected_column_names[i])
  selected_column_names[i] <- gsub("JerkMag","JerkMagnitude",selected_column_names[i])
  selected_column_names[i] <- gsub("GyroMag","GyroMagnitude",selected_column_names[i])
}

colnames(selected_dataset)<-selected_column_names

# Remove activityType column
selected_dataset <- selected_dataset[,names(selected_dataset) != 'activityType'];

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Averaging each activity and each subject as Tidy Data
aggregated_dataset <- aggregate(selected_dataset[,colnames(selected_dataset) != c('activityId','subjectId')],by=list(activityId=selected_dataset$activityId,subjectId=selected_dataset$subjectId),FUN=mean);

# Export tidyData set 
write.table(aggregated_dataset, './FinalTidyData.txt',row.names=FALSE,sep='\t')
