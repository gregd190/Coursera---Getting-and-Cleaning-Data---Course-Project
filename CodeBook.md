## Tidy Dataset Description

###Original Dataset

The original dataset can be obtained from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


###Transformation

The original dataset was reduced to only include variables which contained 'mean' or 'std' (case insensitive). 

The testing and training datasets were merged to form a single dataframe.

The variable referring to the individual has been renamed to 'Subject', and the variable related to the type of activity to 'Activity'.

The numeric code for the activity types in the 'Activity' variable has been converted into descriptive text levels. 

###Variables

The Tidy dataset ontains the mean and standard deviation of each measurement type. 

Variables are as per the original dataset, except for the following:

* Only variables referring to either the mean or standard deviation of each measurement was kept, 
* The variable referring to the individual has been renamed to 'Subject', and the variable related to the type of activity to 'Activity'.
* The previously numeric code for the 'Activity' variable has been transformed into descriptive text levels. 

Each record contains 88 variables, corresponding to the subject, the activity type ('Activity') and the data recorded from the various sensors and subsequent analysis:


 [1] "Subject"                              "Activity"                            
 [3] "tBodyAcc-mean()-X"                    "tBodyAcc-mean()-Y"                   
 [5] "tBodyAcc-mean()-Z"                    "tBodyAcc-std()-X"                    
 [7] "tBodyAcc-std()-Y"                     "tBodyAcc-std()-Z"                    
 [9] "tGravityAcc-mean()-X"                 "tGravityAcc-mean()-Y"                
[11] "tGravityAcc-mean()-Z"                 "tGravityAcc-std()-X"                 
[13] "tGravityAcc-std()-Y"                  "tGravityAcc-std()-Z"                 
[15] "tBodyAccJerk-mean()-X"                "tBodyAccJerk-mean()-Y"               
[17] "tBodyAccJerk-mean()-Z"                "tBodyAccJerk-std()-X"                
[19] "tBodyAccJerk-std()-Y"                 "tBodyAccJerk-std()-Z"                
[21] "tBodyGyro-mean()-X"                   "tBodyGyro-mean()-Y"                  
[23] "tBodyGyro-mean()-Z"                   "tBodyGyro-std()-X"                   
[25] "tBodyGyro-std()-Y"                    "tBodyGyro-std()-Z"                   
[27] "tBodyGyroJerk-mean()-X"               "tBodyGyroJerk-mean()-Y"              
[29] "tBodyGyroJerk-mean()-Z"               "tBodyGyroJerk-std()-X"               
[31] "tBodyGyroJerk-std()-Y"                "tBodyGyroJerk-std()-Z"               
[33] "tBodyAccMag-mean()"                   "tBodyAccMag-std()"                   
[35] "tGravityAccMag-mean()"                "tGravityAccMag-std()"                
[37] "tBodyAccJerkMag-mean()"               "tBodyAccJerkMag-std()"               
[39] "tBodyGyroMag-mean()"                  "tBodyGyroMag-std()"                  
[41] "tBodyGyroJerkMag-mean()"              "tBodyGyroJerkMag-std()"              
[43] "fBodyAcc-mean()-X"                    "fBodyAcc-mean()-Y"                   
[45] "fBodyAcc-mean()-Z"                    "fBodyAcc-std()-X"                    
[47] "fBodyAcc-std()-Y"                     "fBodyAcc-std()-Z"                    
[49] "fBodyAcc-meanFreq()-X"                "fBodyAcc-meanFreq()-Y"               
[51] "fBodyAcc-meanFreq()-Z"                "fBodyAccJerk-mean()-X"               
[53] "fBodyAccJerk-mean()-Y"                "fBodyAccJerk-mean()-Z"               
[55] "fBodyAccJerk-std()-X"                 "fBodyAccJerk-std()-Y"                
[57] "fBodyAccJerk-std()-Z"                 "fBodyAccJerk-meanFreq()-X"           
[59] "fBodyAccJerk-meanFreq()-Y"            "fBodyAccJerk-meanFreq()-Z"           
[61] "fBodyGyro-mean()-X"                   "fBodyGyro-mean()-Y"                  
[63] "fBodyGyro-mean()-Z"                   "fBodyGyro-std()-X"                   
[65] "fBodyGyro-std()-Y"                    "fBodyGyro-std()-Z"                   
[67] "fBodyGyro-meanFreq()-X"               "fBodyGyro-meanFreq()-Y"              
[69] "fBodyGyro-meanFreq()-Z"               "fBodyAccMag-mean()"                  
[71] "fBodyAccMag-std()"                    "fBodyAccMag-meanFreq()"              
[73] "fBodyBodyAccJerkMag-mean()"           "fBodyBodyAccJerkMag-std()"           
[75] "fBodyBodyAccJerkMag-meanFreq()"       "fBodyBodyGyroMag-mean()"             
[77] "fBodyBodyGyroMag-std()"               "fBodyBodyGyroMag-meanFreq()"         
[79] "fBodyBodyGyroJerkMag-mean()"          "fBodyBodyGyroJerkMag-std()"          
[81] "fBodyBodyGyroJerkMag-meanFreq()"      "angle(tBodyAccMean,gravity)"         
[83] "angle(tBodyAccJerkMean),gravityMean)" "angle(tBodyGyroMean,gravityMean)"    
[85] "angle(tBodyGyroJerkMean,gravityMean)" "angle(X,gravityMean)"                
[87] "angle(Y,gravityMean)"                 "angle(Z,gravityMean)"