
  ## Set the working Directory and load packages
        setwd("C:/Users/Dell/Documents/DataScientist/testdata/UCI HAR Dataset/train")
        library(dplyr)
        library(data.table)

  ## Read Train data into tables
       dttrainX<-read.table("X_train.txt")
       dtsubtrain <-read.table("subject_train.txt")
       dttrainy <- read.table("y_train.txt")

  ## Get dimension of the train tables
      dim(dttrainy)
      dim(dtsubtrain)
      dim(dttrainX)

  ## Get unique values y_train and subject_train
      untrain <-unique(dttrainy)
      unsubtrain<-unique(dtsubtrain)
   
  ## Transform the data of train_y from activity code to activity description
       dttrainy$V1<-gsub("1","WALKING",dttrainy$V1)
       dttrainy$V1<-gsub("2","WALKING_UPSTARS",dttrainy$V1)
       dttrainy$V1<-gsub("3","WALKING_DOWNSTAIRS",dttrainy$V1)
       dttrainy$V1<-gsub("4","SIITING",dttrainy$V1)
       dttrainy$V1<-gsub("5","STANDING",dttrainy$V1)
       newdesctrainy<-gsub("6","LAYING",dttrainy$V1)

  ## Rename column V1 as subject in dtsubtrain
       rnsubtrain <-rename(dtsubtrain,subject=V1)

  ## Combine subject and activity with trainX table
       colcombinetrain<-cbind(dttrainX,newdesctrainy,rnsubtrain)

  ## Set the working Directory to read the test files
      setwd("C:/Users/Dell/Documents/DataScientist/testdata/UCI HAR Dataset/test")

  ## Read test data into tables
      dttesty <- read.table("y_test.txt")
      dtsubtest <-read.table("subject_test.txt")
      dttestX<-read.table("X_test.txt")
      dim(dttesty)
      dim(dttestX)
      dim(dtsubtest)

  ## Get unique values of dttesty and dtsubtest
     untest <-unique(dttesty)
     unsubtest<-unique(dtsubtest)
 
  ## Transform the data of test_y from activity code to activity description   
     dttesty$V1<-gsub("1","WALKING",dttesty$V1)
     dttesty$V1<-gsub("2","WALKING_UPSTARS",dttesty$V1)
     dttesty$V1<-gsub("3","WALKING_DOWNSTAIRS",dttesty$V1)
     dttesty$V1<-gsub("4","SIITING",dttesty$V1)
     dttesty$V1<-gsub("5","STANDING",dttesty$V1)
     newdesctesty<-gsub("6","LAYING",dttesty$V1)
    
  ## Rename column V1 as subject in dtsubtrain
     rnsubtest<-rename(dtsubtest,subject=V1)

  ## combine subject and activity data with testX
     colcombinetest<-cbind(dttestX,newdesctesty,rnsubtest)

  ## Rename the colunn wuth activity label in both test and train combine table
     comtrain<-rename(colcombinetrain,activitylabel=newdesctrainy)
     comtest<-rename(colcombinetest,activitylabel=newdesctesty)

  ## Merge train and test data vertically
      mergedtesttrain<-rbind(comtrain,comtest)

  ## Read all the 561 col headers into a table
       setwd("C:/Users/Dell/Documents/DataScientist/testdata/UCI HAR Dataset")
       featurestab<-read.table("features.txt")
       head(featurestab)

  ## Extract column names having mean and std in their headers
       meanV<- grep("[mM]ean",featurestab$V2)
       meancols<-featurestab[meanV,2]
       stdV<- grep("[sS]td",featurestab$V2)
       stdcols<-featurestab[stdV,2]

  ## Extract the columns containing mean and std in their headers
       avgCols <- mergedtesttrain[,c(meancols,stdcols)]
 
  ## Rename measurement columns with descriptive names extracted from features table
       colnames(avgCols) <- c(levels(factor(meancols)),levels(factor(stdcols)))
  
  ## Recombine measurements,activity and subject dataset
       avgCols2 <- select(mergedtesttrain,activitylabel,subject)
       combinedAvg <-cbind(avgCols,avgCols2)

  ## Arrange data by subject and activity
       ordered<- arrange(combinedAvg,subject,activitylabel)

  ## Group data by subject and then by activity level
       avgPersAct <- ordered %>% group_by(subject,activitylabel,add=TRUE) %>% summarise_each(funs(mean))
  ## Output the tidy data set
       write.csv(avgPersAct,"avgPersonActivity.csv")




