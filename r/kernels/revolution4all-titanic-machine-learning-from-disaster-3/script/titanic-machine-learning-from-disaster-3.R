#Read data into R enviorment 
titanicData<-read.csv(file="../input/train.csv",header = TRUE,sep = "," )
#explicitly declare atribute "survived" as factor, by default R treats this as numeric
titanicData$Survived<-factor(titanicData$Survived)
#set libary randomforest to access built in functions
library(randomForest)
#perform missing value treatment on the data
titanicData2<-na.roughfix(titanicData)
#slit the data into test and evaluation set
ind <- sample(2, nrow(titanicData2),replace = TRUE,prob = c(0.8,0.2))
#test data - 80% sample of the orginal data
tdata <-titanicData2[ind==1,]
#validation data - 20% sample of the orginal data
vdata <-titanicData2[ind==2,]
#declare prediction variable to train on the test data
#assumption has been made that pclass, age and sex are important attributes that helped survive
fcn=randomForest(Survived~Pclass+Sex+Age ,data=tdata,mtry=3,ntree=100)
#make the preiction on validation set based on learning from test data
predicted<- predict(fcn,newdata=vdata,type="class")
#convert predicted from atomic vector  into data frame 
predicted <- as.data.frame(predicted)
#add predicted column to vdata to compare the prediction with actuals
vdata <- cbind(vdata, predicted)
#compute accurcy as no. of coorect prediction to total number of predcitions
accuracy_rf <- sum(predicted$predicted == vdata$Survived)/NROW(vdata)
accuracy_rf
#identify the important factors that helped in survival
importance(fcn)

#the accuracy was 72.88% and the most important factors helping in survival
#was identified to be "sex" followed by "age" and "Pclass"




#Approach2 -since the accuracy was only 72.88% lets try adding 
#more attributes to the above to make the predictions more accuarte

#define  variable fmem as numebr of family members(siblings+parents) 
#add it to test and validation sets
tdata2<-cbind(tdata,fmem=tdata$SibSp+tdata$Parch)
vdata2<-cbind(vdata,fmem=vdata$SibSp+vdata$Parch)
#define prediction variable as 
fcn2=randomForest(Survived~Pclass+Sex+Age+fmem ,data=tdata2,mtry=4,ntree=100,importance=TRUE)

predicted2<- predict(fcn2,newdata=vdata2,type="class")
predicted2 <- as.data.frame(predicted2)
vdata2 <- cbind(vdata2, predicted2)
accuracy_rf2 <- sum(predicted2$predicted2 == vdata2$Survived)/NROW(vdata2)
accuracy_rf2
importance(fcn2)
#the accuracy improved slightly from 72.88% tp 74% by adding
#family member attribute to prediction 
#the most important factors helping in survival
#was identified to be "sex" followed by "age", "Pclass" and fmem

#make submission to titanic contest 
write.csv(vdata2, file = "random_forest_r_submission.csv", row.names=FALSE)

imp <- importance(fcn2, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
library(ggplot2) 
p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
     geom_bar(stat="identity", fill="#53cfff") +
     coord_flip() + 
     theme_light(base_size=20) +
     xlab("") +
     ylab("Importance") + 
     ggtitle("Random Forest Feature Importance\n") +
     theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p)

