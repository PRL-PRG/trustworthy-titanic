
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(dplyr)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")
comb<- bind_rows(train,test)

# Here we will plot the passenger survival by class
train$Survived <- factor(train$Survived, levels=c(1,0))
levels(train$Survived) <- c("Survived", "Died")
train$Pclass <- as.factor(train$Pclass)
levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

png("1_survival_by_class.png", width=800, height=600)
mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)
dev.off()

png("2_Survival_by_Gender.png",width=800,height=600)
mosaicplot(train$Sex ~ train$Survived, main = "Passenger Survival by Gender",
            color=c("#8dd3c7", "#fb8072"))
dev.off()

train$Child<-train$Age<18
train$Child<-factor(train$Child,levels=c(TRUE,FALSE))
levels(train$Child)<-c("Child","Adult")
barplot(table(train$Survived,train$Child), legend=TRUE,
           col=c("green","red"))
           
##Fetch title from Name column and create new Title column
comb$Title <- gsub("(.*, )|(\\..*)", "", comb$Name)

#Group titles
comb$Title[comb$Title=="Don"]<-"Mr"
comb$Title[comb$Title == "Mme"]<-"Mrs"
comb$Title[comb$Title == "Mlle"]<-"Miss"
comb$Title[comb$Title == "Ms"]<-"Miss"
comb$Title[comb$Title == "Jonkheer"]<-"Master"
comb$Title[comb$Title != "Mr" & comb$Title != "Mrs" &
        comb$Title != "Miss" & comb$Title != "Master"]<-"Special"  
table(comb$Title)

           
           









