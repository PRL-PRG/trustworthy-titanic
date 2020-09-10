
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
summary(train)

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
test$Survived <- NA 
library("dplyr")
combined <- bind_rows(train,data=test) 
nrow(train)
nrow(test)
nrow(combined)
str(combined, give.attr = FALSE)
colSums(is.na(combined))
colnames(combined)
combined=combined[,-c(11,12,4,9)]
View(combined)
str(combined)
combined$Survived <- factor(combined$Survived)
combined$Pclass <- factor(combined$Pclass)
combined$Sex <- factor(combined$Sex)
combined$Age=ifelse(is.na(combined$Age),median(na.omit(combined$Age)),combined$Age)
combined$Fare=ifelse(is.na(combined$Fare),median(na.omit(combined$Fare)),combined$Fare)
str(titanic_traindata)
length(unique(combined$Ticket))
chiSqStat=NULL
for (i in 2:(ncol(combined))) 
{ if(is.factor(combined[,i]))
{
  ChiSqTest=chisq.test(x=combined$Survived,
                       y=combined[,i])
  chiSqStat=rbind.data.frame(chiSqStat,
                             cbind.data.frame(
                               variable.names=colnames(combined)[i],
                               chi_sq_value=ChiSqTest$statistic,
                               p_value=ChiSqTest$p.value))
  cat("\n",colnames(combined)[i],"\n","chi-sq value:",
      ChiSqTest$statistic,"pvalue:",ChiSqTest$p.value,"\n")
  cat("*************************")
}
}
View(chiSqStat)
train<-combined[1:891,]
test<-combined[892:1309,]
Logistic_Model_1=glm(Survived~.,family = binomial,data=train,maxit=100)
predict_Probs=predict(Logistic_Model_1,test)
Predict_Class = ifelse(predict_Probs >= 0.5,1,0)
prediction=write.csv(Predict_Class, "titanictestfinaldeb output1.csv", row.names = FALSE)
solution <- data.frame(PassengerID = titanic_testdata$PassengerId, Survived = prediction)

