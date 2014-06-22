if( !file.exists("UCI\ HAR\ Dataset") ){
   print("Downloading original data")
   download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile="uci.zip",method="curl")
   unzip("uci.zip")
}

print("Loading labels")
actLabel <- read.delim(file="UCI\ HAR\ Dataset/activity_labels.txt",sep=" ",header=FALSE)
features <- read.delim(file="UCI\ HAR\ Dataset/features.txt",sep=" ",header=FALSE)

print("Loading test data")
subjTest <- read.delim(file="UCI\ HAR\ Dataset/test/subject_test.txt",header=FALSE,col.names=c("subject"))
actsTest <- read.delim(file="UCI\ HAR\ Dataset/test/y_test.txt",header=FALSE,col.names=c("activity"))
dataTest <- read.delim(file="UCI\ HAR\ Dataset/test/X_test.txt",header=FALSE,col.names=features$V2,sep="")
activity <- factor(actsTest$activity,labels=as.character(actLabel$V2))
completeTest  <- cbind(activity, cbind(subjTest, dataTest))

print("Loading train data")
subjTrain <- read.delim(file="UCI\ HAR\ Dataset/train/subject_train.txt",header=FALSE,col.names=c("subject"))
actsTrain <- read.delim(file="UCI\ HAR\ Dataset/train/y_train.txt",header=FALSE,col.names=c("activity"))
dataTrain <- read.delim(file="UCI\ HAR\ Dataset/train/X_train.txt",header=FALSE,col.names=features$V2,sep="")
activity  <- factor(actsTrain$activity,labels=as.character(actLabel$V2))
completeTrain <- cbind(activity,cbind(subjTrain,dataTrain))

print("Merging train and test data")
completedata <- rbind(completeTrain,completeTest)

print("Printing overal means and standard deviations:")
for(i in names(dataTrain)) print(c(i," = ",mean(completedata[[i]]),"+-",sd(completedata[[i]])))

print("Printing means and standard deviations for each of the activities:")
for(i in names(dataTrain)) print(c(i,tapply(completedata[[i]],completedata$activity,mean)))

print("Creating the tidy dataset")
tidyData <- data.frame( subject=integer(), activity=factor() )
for(subj in 1:30){
    for(act in actLabel$V2){
       tidyData <- rbind(tidyData,data.frame( subject=subj, activity=act))
    }
}

for(i in names(dataTrain)){
    m <- tapply(completedata[[i]],list("activity"=completedata$activity,"subject"=completedata$subject),mean)
    tidyData <- cbind(tidyData, as.vector(m) )
}

colnames(tidyData) <- c("subject","activity",names(dataTrain))

write.csv(tidyData,"tidyData.txt")
