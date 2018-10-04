library(data.table)
library(dplyr)

# Read in feature names

features = read.table('UCI_HAR_Dataset/features.txt')
#Remove Feature Numbers
features = features[2]

# Find indexes of the features that contain either "mean" or "std"
meanindexlist<- grep("mean", tolower(features[,1]))
stdindexlist<- grep("std", tolower(features[,1]))
indexlist <- sort(c(meanindexlist,stdindexlist))

subject_test = read.table('UCI_HAR_Dataset/test/subject_test.txt')
y_test = read.table('UCI_HAR_Dataset/test/y_test.txt')
x_test = read.table('UCI_HAR_Dataset/test/X_test.txt')
x_test = x_test[,indexlist]


# Cannot have common column names to merge, so rename to unique names
colnames(subject_test) <- "S1"
colnames(y_test) <- "Y1"
colnames(x_test) <- features[indexlist,1]

testdf = tbl_df(c(subject_test, y_test, x_test))

subject_train = read.table('UCI_HAR_Dataset/train/subject_train.txt')
y_train = read.table('UCI_HAR_Dataset/train/y_train.txt')
x_train = read.table('UCI_HAR_Dataset/train/X_train.txt')
x_train = x_train[, indexlist]


# Again, we cannot have common column names to merge, so rename to unique names

colnames(subject_train) <- "S1"
colnames(y_train) <- "Y1"
colnames(x_train) <- features[indexlist,1]

traindf = tbl_df(c(subject_train, y_train, x_train))

dim(traindf)

merged = rbind(testdf, traindf)

## Part 3:Uses descriptive activity names to name the activities in the data 

#Rename the Y1 row to "Activity", and the S1 row to "Subject"
setnames(merged, "Y1","Activity")
setnames(merged, "S1","Subject")

merged$Activity = factor(merged$Activity, levels = c(1,2,3,4,5,6), labels = c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"))

## Part 4: Appropriately labels the data set with descriptive variable names.

descfeatures -> names(merged)
descfeatures = gsub("-mean", " Mean ", descfeatures)
descfeatures = gsub("-std", " STD ", descfeatures)
descfeatures = gsub('[-()]',"", descfeatures)

names(merged) <- descfeatures

## Part 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidytable = arrange(merged, Subject, Activity)
tidytable = group_by(merged, Subject, Activity)
tidytable = tidytable %>% summarise_all(funs(mean))




