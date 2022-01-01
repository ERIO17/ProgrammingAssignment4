# project course3 week4 getting and sorting data
#
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.

# download zip cf https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-downloadingFiles.md

library("dplyr")
getwd()
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "2Fprojectfiles"
if(!file.exists("./2Fprojectfiles")) {
  download.file(fileUrl,
                destfile,
                mode="wb") # "wb" means "write binary," and is used for binary files
  
}
list.files()
unzip(zipfile = "2Fprojectfiles") # unpack the files into subdirectories 
list.files()
list.files("UCI_HAR_Dataset/test")

# -------------------------------------------------------------------------#
# -------------   prepare changing code to name activity     --------------#
# -------------------------------------------------------------------------#
myact <- data.frame(c(1,2,3,4,5,6), c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
colnames(myact) <- c("n", "ac")
myact

val1 <- function(x) {
  n <<- as.numeric(ytest[x,1])
  ytest[x,1]<<-myact[n, "ac"]
}

val2 <- function(x) {
  n <<- as.numeric(ytrain[x,1])
  ytrain[x,1]<<-myact[n, "ac"]
}

# -------------------------------------------------------------------------#
# -----------     loading experiment's names  = features.txt    -----------#
# -------------------------------------------------------------------------#


# read UCI_HAR_Dataset/features.txt
featurefile <- "UCI_HAR_Dataset/features.txt"
feat <- data.frame()
feat <- read.table(featurefile, header = FALSE)
head(feat)
View(feat)
summary(feat)
nrow(feat)
ncol(feat)
coln <- as.vector(feat[,2])
class(coln)
coln

# -------------------------------------------------------------------------#
# -------------------     load files test set      ------------------------#
# -------------------------------------------------------------------------#

# read UCI_HAR_Dataset/test/X_test.txt  + column name
xtestfile <- "UCI_HAR_Dataset/test/X_test.txt"
xtest <- data.frame()
xtest <- read.table(xtestfile, header = FALSE)
head(xtest)
summary(xtest)
ncol(xtest)
nrow(xtest)
colnames(xtest) <- coln
head(xtest, 1)


# read UCI_HAR_Dataset/test/y_test.txt  + column name + code to name activity
ytestfile <- "UCI_HAR_Dataset/test/y_test.txt"
ytest <- data.frame()
ytest <- read.table(ytestfile, header = FALSE)
head(ytest)
summary(ytest)
ncol(ytest)
nrow(ytest)
unique(ytest)
colnames(ytest) <- c("activity")
head(ytest)
val1(1:nrow(ytest))

# read UCI_HAR_Dataset/test/subject_test.txt
subtestfile <- "UCI_HAR_Dataset/test/subject_test.txt"
subtest <- data.frame()
subtest <- read.table(subtestfile, header = FALSE)
head(subtest)
summary(subtest)
ncol(subtest)
nrow(subtest)
unique(subtest)
colnames(subtest) <- c("subject_number")
head(subtest)

# -----     Filter columns and construct dataframe part test set     -----#

xtest_filt1 = select(xtest, grep("mean", names(xtest), value=TRUE) | grep("std", names(xtest), value=TRUE))
head(xtest_filt1)
ncol(xtest_filt1)
nrow(xtest_filt1)

datafram1 <- cbind(subtest, ytest, xtest_filt1)
head(datafram1)
ncol(datafram1)
nrow(datafram1)

# -------------------------------------------------------------------------#
# ----------------------     load files train set     --- -----------------#
# -------------------------------------------------------------------------#

# read UCI_HAR_Dataset/train/X_train.txt = measurements
xtrainfile <- "UCI_HAR_Dataset/train/X_train.txt"
xtrain <- data.frame()
xtrain <- read.table(xtrainfile, header = FALSE)
head(xtrain)
summary(xtrain)
ncol(xtrain)
nrow(xtrain)
colnames(xtrain) <- coln
head(xtrain,2)

# read UCI_HAR_Dataset/train/y_train.txt = activity
ytrainfile <- "UCI_HAR_Dataset/train/y_train.txt"
ytrain <- data.frame()
ytrain <- read.table(ytrainfile, header = FALSE)
head(ytrain)
summary(ytrain)
ncol(ytrain)
nrow(ytrain)
unique(ytrain)
colnames(ytrain) <- c("activity")
head(ytrain)
val2(1:nrow(ytrain))
head(ytrain); tail(ytrain)

# read UCI_HAR_Dataset/train/subject_train.txt
subtrainfile <- "UCI_HAR_Dataset/train/subject_train.txt"
subtrain <- data.frame()
subtrain <- read.table(subtrainfile, header = FALSE)
head(subtrain)
summary(subtrain)
ncol(subtrain)
nrow(subtrain)
unique(subtrain)
colnames(subtrain) <- c("subject_number")
head(subtrain)

# ------Filter columns and construct dataframe  part train set ---------#

xtrain_filt1 = select(xtrain, grep("mean", names(xtrain), value=TRUE) | grep("std", names(xtrain), value=TRUE))
head(xtrain_filt1)
ncol(xtrain_filt1)
nrow(xtrain_filt1)

datafram2 <- cbind(subtrain, ytrain, xtrain_filt1)
head(datafram2)
ncol(datafram2)
nrow(datafram2)

# -------------------------------------------------------------------------#
#  construct final data set datafram3 sorted by subject_number & activity  #
# -------------------------------------------------------------------------#

datafram3 <- rbind(datafram1, datafram2)
datafram3 <- arrange(datafram3, subject_number, activity)
head(datafram3, 2)
ncol(datafram3)
nrow(datafram3)
names(datafram3)
unique(datafram3$activity)
unique(datafram3$"subject_number")


# -------------------------------------------------------------------------#
#construct datafram4 with average meas.ments by  subject_number & activity #
# -------------------------------------------------------------------------#

listn <- names(datafram3)
listn
listn[3:length(listn)]

datafram4 <- datafram3 %>% 
  group_by(activity, subject_number) %>% 
  summarise_at(vars(listn[3:length(listn)]),
               list(name = mean))

head(datafram4)
View(datafram4)
