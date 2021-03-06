---
title: "Code_Book"
author: "jlg"
date: "01/01/2022"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

DATA DICTIONARY – ACTIVITY EXPERIMENT



		[1] subject_number
			subject_number 				code : numeric 1:30

		[2] activity
			activity 					code : character string 
								1 WALKING
								2 WALKING_UPSTAIRS
								3 WALKING_DOWNSTAIRS
								4 SITTING
								5 STANDING
								6 LAYING

		[3] tBodyAcc-mean()-X
			tBodyAcc-mean()-X 				code : numeric -1: 1
		[4] tBodyAcc-mean()-Y
			tBodyAcc-mean()-Y 				code : numeric -1: 1
		[5] tBodyAcc-mean()-Z
			tBodyAcc-mean()-Z 				code : numeric -1: 1

		[6] tGravityAcc-mean()-X
			tGravityAcc-mean()-X 			code : numeric -1: 1
		[7] tGravityAcc-mean()-Y
			tGravityAcc-mean()-Y 			code : numeric -1: 1
		[8] tGravityAcc-mean()-Z
			tGravityAcc-mean()-Z 			code : numeric -1: 1

		[9] tBodyAccJerk-mean()-X
			tBodyAccJerk-mean()-X 			code : numeric -1: 1
		[10] tBodyAccJerk-mean()-Y
			tBodyAccJerk-mean()-Y 			code : numeric -1: 1
		[11] tBodyAccJerk-mean()-Z
			tBodyAccJerk-mean()-Z 			code : numeric -1: 1

		[12] tBodyGyro-mean()-X
			tBodyGyro-mean()-X 			code : numeric -1: 1
		[13] tBodyGyro-mean()-Y
			tBodyGyro-mean()-Y 			code : numeric -1: 1
		[14] tBodyGyro-mean()-Z
			tBodyGyro-mean()-Z 				code : numeric -1: 1

		[15] tBodyGyroJerk-mean()-X
			tBodyGyroJerk-mean()-X 			code : numeric -1: 1
		[16] tBodyGyroJerk-mean()-Y
			tBodyGyroJerk-mean()-Y 			code : numeric -1: 1
		[17] tBodyGyroJerk-mean()-Z
			tBodyGyroJerk-mean()-Z 			code : numeric -1: 1

		[18] tBodyAccMag-mean()
			tBodyAccMag-mean() 			code : numeric -1: 1
		[19] tGravityAccMag-mean()
			tGravityAccMag-mean() 			code : numeric -1: 1
		[20] tBodyAccJerkMag-mean()
			tBodyAccJerkMag-mean() 			code : numeric -1: 1
		[21] tBodyGyroMag-mean()
			tBodyGyroJerk-mean()			code : numeric -1: 1
		[22] tBodyGyroJerkMag-mean()
			tBodyGyroJerkMag-mean() 			code : numeric -1: 1

		[23] fBodyAcc-mean()-X
			fBodyAcc-mean()-X 				code : numeric -1: 1
		[24] fBodyAcc-mean()-Y
			fBodyAcc-mean()-Y 				code : numeric -1: 1
		[25] fBodyAcc-mean()-Z
			fBodyAcc-mean()-Z 				code : numeric -1: 1

		[26] fBodyAcc-meanFreq()-X
			fBodyAcc-meanFreq()-X 			code : numeric -1: 1
		[27] fBodyAcc-meanFreq()-Y
			fBodyAcc-meanFreq()-Y 			code : numeric -1: 1
		[28] fBodyAcc-meanFreq()-Z
			fBodyAcc-meanFreq()-Z 			code : numeric -1: 1

		[29] fBodyAccJerk-mean()-X
			fBodyAccJerk-mean()-X 			code : numeric -1: 1
		[30] fBodyAccJerk-mean()-Y
			fBodyAccJerk-mean()-Y 			code : numeric -1: 1
		[31] fBodyAccJerk-mean()-Z
			fBodyAccJerk-mean()-Z 			code : numeric -1: 1

		[32] fBodyAccJerk-meanFreq()-X
			fBodyAccJerk-meanFreq()-X 		code : numeric -1: 1
		[33] fBodyAccJerk-meanFreq()-Y
			fBodyAccJerk-meanFreq()-Y 		code : numeric -1: 1
		[34] fBodyAccJerk-meanFreq()-Z
			fBodyAccJerk-meanFreq()-Z 		code : numeric -1: 1

		[35] fBodyGyro-mean()-X
			fBodyGyro-mean()-X 			code : numeric -1: 1
		[36] fBodyGyro-mean()-Y
			fBodyGyro-mean()-Y 			code : numeric -1: 1
		[37] fBodyGyro-mean()-Z
			fBodyGyro-mean()-Z 			code : numeric -1: 1

		[38] fBodyGyro-meanFreq()-X
			fBodyGyro-meanFreq()-X 			code : numeric -1: 1
		[39] fBodyGyro-meanFreq()-Y
			fBodyGyro-meanFreq()-Y 			code : numeric -1: 1
		[40] fBodyGyro-meanFreq()-Z 
			fBodyGyro-meanFreq()-Z 			code : numeric -1: 1

		[41] fBodyAccMag-mean()
			fBodyAccMag-mean() 			code : numeric -1: 1
		[42] fBodyAccMag-meanFreq()
			fBodyAccMag-meanFreq() 			code : numeric -1: 1
		[43] fBodyBodyAccJerkMag-mean()
			fBodyBodyAccJerkMag-mean() 		code : numeric -1: 1
		[44] fBodyBodyAccJerkMag-meanFreq() 
			fBodyBodyAccJerkMag-meanFreq() 	code : numeric -1: 1
		[45] fBodyBodyGyroMag-mean()
			fBodyBodyGyroMag-mean() 		code : numeric -1: 1
		[46] fBodyBodyGyroMag-meanFreq()
			fBodyBodyGyroMag-meanFreq() 		code : numeric -1: 1
		[47] fBodyBodyGyroJerkMag-mean()
			fBodyBodyGyroJerkMag-mean() 		code : numeric -1: 1
		[48] fBodyBodyGyroJerkMag-meanFreq()
			fBodyBodyGyroJerkMag-meanFreq() 	code : numeric -1: 1

		[49] tBodyAcc-std()-X
			tBodyAcc-std()-X 				code : numeric -1: 1
		[50] tBodyAcc-std()-Y
			tBodyAcc-std()-Y 				code : numeric -1: 1
		[51] tBodyAcc-std()-Z
			tBodyAcc-std()-Z 				code : numeric -1: 1

		[52] tGravityAcc-std()-X
			tGravityAcc-std()-X 				code : numeric -1: 1
		[53] tGravityAcc-std()-Y
			tGravityAcc-std()-Y 				code : numeric -1: 1
		[54] tGravityAcc-std()-Z
			tGravityAcc-std()-Y 				code : numeric -1: 1

		[55] tBodyAccJerk-std()-X
			tBodyAccJerk-std()-X 			code : numeric -1: 1
		[56] tBodyAccJerk-std()-Y
			tBodyAccJerk-std()-Y 			code : numeric -1: 1
		[57] tBodyAccJerk-std()-Z
			tBodyAccJerk-std()-Z 			code : numeric -1: 1

		[58] tBodyGyro-std()-X
			tBodyGyro-std()-X 				code : numeric -1: 1
		[59] tBodyGyro-std()-Y
			tBodyGyro-std()-Y 				code : numeric -1: 1
		[60] tBodyGyro-std()-Z
			tBodyGyro-std()-Z 				code : numeric -1: 1

		[61] tBodyGyroJerk-std()-X
			tBodyGyroJerk-std()-X 			code : numeric -1: 1
		[62] tBodyGyroJerk-std()-Y
			tBodyGyroJerk-std()-Y 			code : numeric -1: 1
		[63] tBodyGyroJerk-std()-Z
			tBodyGyroJerk-std()-Z 			code : numeric -1: 1

		[64] tBodyAccMag-std()
			tBodyAccMag-std() 				code : numeric -1: 1
		[65] tGravityAccMag-std()
			tGravityAccMag-std() 			code : numeric -1: 1
		[66] tBodyAccJerkMag-std()
			tBodyAccJerkMag-std() 			code : numeric -1: 1
		[67] tBodyGyroMag-std()
			tBodyGyroMag-std() 				code : numeric -1: 1
		[68] tBodyGyroJerkMag-std()
			tBodyGyroJerkMag-std() 			code : numeric -1: 1

		[69] fBodyAcc-std()-X
			fBodyAcc-std()-X 				code : numeric -1: 1
		[70] fBodyAcc-std()-Y
			fBodyAcc-std()-Y 				code : numeric -1: 1
		[71] fBodyAcc-std()-Z
			fBodyAcc-std()-Z 				code : numeric -1: 1

		[72] fBodyAccJerk-std()-X
			fBodyAccJerk-std()-X 			code : numeric -1: 1
		[73] fBodyAccJerk-std()-Y
			fBodyAccJerk-std()-Y 			code : numeric -1: 1
		[74] fBodyAccJerk-std()-Z
			fBodyAccJerk-std()-Z 			code : numeric -1: 1

		[75] fBodyGyro-std()-X 
			fBodyGyro-std()-X 				code : numeric -1: 1
		[76] fBodyGyro-std()-Y
			fBodyGyro-std()-Y	 			code : numeric -1: 1
		[77] fBodyGyro-std()-Z
			fBodyGyro-std()-Z	 			code : numeric -1: 1

		[78] fBodyAccMag-std()
			fBodyAccMag-std() 	 			code : numeric -1: 1
		[79] fBodyBodyAccJerkMag-std()
			fBodyBodyAccJerkMag-std()  		code : numeric -1: 1
		[80] fBodyBodyGyroMag-std()
			fBodyBodyGyroMag-std() 			code : numeric -1: 1
		[81] fBodyBodyGyroJerkMag-std()  
			fBodyBodyGyroJerkMag-std()  		code : numeric -1: 1



The R script called run_analysis.R performing the cleaning and analysis and calculates average values for activity and subject (see details in README.md) 

