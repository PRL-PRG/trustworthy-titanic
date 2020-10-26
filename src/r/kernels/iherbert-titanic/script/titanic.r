
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.



train <- read_csv("../input/train.csv")
test <-read_csv("../input/test.csv")
gender<-read_csv("../input/gender_submission.csv")

str(train)
head(train)
No_cabin<-train%>%filter(is.na(Cabin))%>%summarize(count = n())
No_cabin

useful<- train[,c("PassengerId", "Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]
head(useful)

#Generate counts for NA in each of the remaining fields
length(which(is.na(useful$Pclass)))
length(which(is.na(useful$Sex)))
length(which(is.na(useful$Age)))
length(which(is.na(useful$SibSp)))
length(which(is.na(useful$Parch)))
length(which(is.na(useful$Fare)))
length(which(is.na(useful$Embarked)))

#Find the most common embarkation point, in case it will be sensible to replace missing values with it. 
useful%>%group_by(Embarked)%>%summarize(n())

range(useful$Age, na.rm=TRUE) #it looks like there are a wide range of ages, including a baby of around 5 months
useful%>%filter(Age==0.42) #phew,the baby survived!

median(useful$Age, na.rm=TRUE)
mean(useful$Age, na.rm=TRUE)



ggplot(useful, aes(x=Age))+geom_histogram(binwidth=5)


#Replace NA is Embarked with Southampton
useful_no_na<-useful
useful_no_na$Embarked[which(is.na(useful_no_na$Embarked))]<-"S"

#Replace NA in Age with mean age of other passengers
useful_no_na$Age[which(is.na(useful_no_na$Age))]<-mean(useful_no_na$Age, na.rm=TRUE)


model1 <-glm(Survived ~ ., family = "binomial", data = useful_no_na)


length(which(is.na(test$Age))) #=86, which isn't great

mean(test$Age, na.rm=1) 
#but the mean is also around 30, so replacing the missing ages with the mean here doesn't make the test distribution too far from the training one

test_no_na <- test
test_no_na$Age[which(is.na(test$Age))]<-mean(test$Age, na.rm=1)


test_pred1<- predict(model1, newdata= test_no_na, type = "response")

#Check how many rows in the test data were missing the other fields we chose to use

length(which(is.na(test_pred1))) #looks like we're just missing data for 1 point. Let's see what it is. 


which(is.na(test_pred1))
test_no_na[153,]

class3fare<-test_no_na%>%filter(Pclass==3)%>%summarize(median(Fare, na.rm=1))
test_no_na[153,"Fare"]<- class3fare
test_no_na[153,]

test_pred1<- predict(model1, newdata= test_no_na, type = "response")
head(test_pred1)

submission<-test["PassengerId"]
submission<-submission%>%mutate(Survived = as.integer(test_pred1>=0.5))
head(submission)


submission%>%filter(Survived ==1)%>%summarize(Test_survive_pct = n()/nrow(submission))

train%>%filter(Survived==1)%>%summarize(Train_survive_pct = n()/nrow(train))



submission<-submission%>%mutate(PassengerId = as.integer(PassengerId))
write_csv(submission, "submission1.csv")

#Look at a summary of the model we started with

summary(model1)


model2<-glm(Survived~Pclass+Sex+Age+SibSp, family = "binomial", data = useful_no_na)
summary(model2)

test_pred2 = predict(model2, newdata= test_no_na, type = "response")
head(test_pred2)


submission2<-test["PassengerId"]
submission2<-submission2%>%mutate(PassengerId = as.integer(PassengerId))%>%mutate(Survived = as.integer(test_pred2>=0.5))
head(submission2)

submission2%>%filter(Survived==1)%>%summarize(Test_survive_pct = n()/nrow(submission2))

write_csv(submission2, "submission2.csv")

model3<-glm(Survived~Pclass+Sex+SibSp, family = "binomial", data = useful_no_na)
summary(model3)
test_pred3<-predict(model3, newdata = test_no_na, type= "response")
head(test_pred3)

submission3<-test["PassengerId"]%>%mutate(PassengerId = as.integer(PassengerId))%>%mutate(Survived = as.integer(test_pred3>=0.50))
submission3%>%filter(Survived ==1)%>%summarize(Test_survive_pct = n()/nrow(submission3))
write_csv(submission3, "submission3.csv")

# Create new data frames with "none" category for unknown ages. 
useful_na_age<-useful
useful_na_age[which(is.na(useful$Age)), "Age"]<-"none"
head(useful_na_age)

test_na_age<-test
test_na_age[which(is.na(test$Age)), "Age"]<-"none"


#Make a new model and get its predictions
model4<-glm(Survived~Pclass +Sex+Age+SibSp, family = "binomial", data= useful_na_age)
summary(model4)



#test_pred4<-predict(model4, newdata = test_na_age, type = "response")
#head(test_pred4)

#Write a function that, for a given bin size, assigns each Age value to a bin named by the range of the bin

bin_namer <-function(x, binsize){
    midpoint<-binsize/2+(binsize*floor(x/binsize))
    boxname<-paste(as.character(binsize*floor(midpoint/binsize)),"-", as.character(binsize*ceiling(midpoint/binsize)))
    return(boxname)
}
            
        
useful_age_bin<-useful%>%mutate(Age_bin = bin_namer(Age, 10))
       head(useful_age_bin)
test_age_bin<-test%>%mutate(Age_bin = bin_namer(Age, 10))
       head(test_age_bin)

#Check how the survival chances look by age bin

bin_groups<-useful_age_bin%>%group_by(Age_bin)%>%summarize(Percent_survived = length(which(Survived ==1))/n())
ggplot(bin_groups, aes(x=Age_bin, y=Percent_survived))+geom_col()+labs(x="Age bin", y="Percentage of passengers who survived")


#Construct the model
model4<-glm(Survived~Pclass +Sex+Age_bin+SibSp, family = "binomial", data= useful_age_bin)
summary(model4)

#Get the predictions
test_pred4<-predict(model4, newdata = test_age_bin, type = "response")
head(test_pred4)

#Prepare the submission
submission4<-test["PassengerId"]%>%mutate(PassengerId = as.integer(PassengerId))%>%mutate(Survived = as.integer(test_pred4>=0.50))
submission4%>%filter(Survived ==1)%>%summarize(Test_survive_pct = n()/nrow(submission4))
write_csv(submission4, "submission4.csv")


useful_age_bin5<-useful%>%mutate(Age_bin = bin_namer(Age, 5))
       head(useful_age_bin5)
test_age_bin5<-test%>%mutate(Age_bin = bin_namer(Age, 5))
       head(test_age_bin5)


#Treat our Age_bin variable as an ordered factor
factored_age_bin5<-useful_age_bin5
ages<- c("0 - 5","5 - 10","10 - 15", "15 - 20","20 - 25", "25 - 30","30 - 35", "35 - 40", "40 - 45", "45 - 50", "50 - 55", "55 - 60", "60 - 65", "65 - 70", "70 - 75", "75 - 80", "80 - 85", "NA - NA" )
factored_age_bin5$Age_bin<-factor(useful_age_bin5$Age_bin, ordered = TRUE, levels = ages)
factored_age_bin5$Survived<-factor(factored_age_bin5$Survived, levels = c("0", "1"))


test_age_bin5<-test%>%mutate(Age_bin = bin_namer(Age, 5))
       head(test_age_bin5)
test_age_bin5$Age_bin<-factor(test_age_bin5$Age_bin, ordered = TRUE, levels = ages)



bin_groups<-factored_age_bin5%>%group_by(Age_bin)%>%mutate(Percent_survived = length(which(Survived ==1))/n())%>%mutate(Bin_count = n())
ggplot(bin_groups, aes(x=Age_bin, y = Percent_survived, size = Bin_count))+geom_point()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

test_age_bin5%>%filter(Age_bin=="75 - 80") #See if there are such passengers

#Unfortunately, there is one. 
useful_age_bin5%>%filter(Age_bin=="70 - 75") #We've got 6 passengers in the training set in bin 70-75, so let's try just throwing Mrs. Cavendish in that bin

which(test_age_bin5$Age_bin == "75 - 80")
test_age_bin5[97, "Age_bin"]<-"70 - 75"

#Now build the model and predictions

model5<-glm(Survived~Pclass +Sex+Age_bin+SibSp, family = "binomial", data= useful_age_bin5)
summary(model5)

#Get predictions
test_pred5<-predict(model5, newdata = test_age_bin5, type = "response")

#Generate submission
submission5<-test["PassengerId"]%>%mutate(PassengerId = as.integer(PassengerId))%>%mutate(Survived = as.integer(test_pred5>=0.50))
submission5%>%filter(Survived ==1)%>%summarize(Test_survive_pct = n()/nrow(submission5))
write_csv(submission5, "submission5.csv")



#Selecting Cross Validation Sets
set.seed(100)
permutation<-sample(nrow(train))
permuted_train<-train[permutation,]

#Now we've put our training set into a random order. If we just separate it into thirds, each will be randomly selected relative to the other two. 
#Then we can use each two to train a model predicting the third, and average these models together for the test.

train1<-permuted_train[1:(nrow(train)/3),]
train2<-permuted_train[((nrow(train)/3)+1):(2*nrow(train)/3),]
train3<-permuted_train[((2*nrow(train)/3)+1):nrow(train), ]
head(train3)

train12<-bind_rows(train1, train2)
train13<-bind_rows(train1, train3)
train23<-bind_rows(train2, train3)

cvmodel12<-glm(Survived~Pclass+Sex+SibSp, family = "binomial", data = train12)
cvmodel13<-glm(Survived~Pclass+Sex+SibSp, family = "binomial", data = train13)
cvmodel23<-glm(Survived~Pclass+Sex+SibSp, family = "binomial", data = train23)

#Make the predictions for the missing data from each model.
pred12<- predict(cvmodel12, newdata = train3, type = "response")
pred13<-predict(cvmodel13, newdata = train2, type = "response")
pred23<- predict(cvmodel23, newdata = train1, type = "response")

threshold<-0.6
prediction12<-ifelse(pred12>=threshold, 1, 0)
prediction13<-ifelse(pred13>=threshold, 1, 0)
prediction23<-ifelse(pred23>=threshold, 1, 0)

#Let's see how the predictions are working out
conf1<-table(train1$Survived, prediction23)
conf2<-table(train2$Survived, prediction13)
conf3<-table(train3$Survived, prediction12)

conf1
conf2
conf3

accuracy12<-sum(diag(conf3))/sum(conf3)
accuracy13<-sum(diag(conf2))/sum(conf2)
accuracy23<-sum(diag(conf1))/sum(conf1)

c(accuracy12, accuracy13, accuracy23)

test_pred12<-predict(cvmodel12, newdata=test, type="response")
test_pred13<-predict(cvmodel13,, newdata=test, type="response")
test_pred23<-predict(cvmodel23, newdata=test, type="response")

test_pred6<- rowMeans(cbind(test_pred12, test_pred13, test_pred23))
head(test_pred6)

submission6<-test["PassengerId"]%>%mutate(Survived = as.integer(test_pred6>=0.5), PassengerId = as.integer(PassengerId))

submission6%>%filter(Survived ==1)%>%summarize(n()/nrow(submission6))
write_csv(submission6, "submission6.csv")

length(which(submission3$Survived !=submission6$Survived))

#Separate the set into pieces. We'll write actual functions to do it this time. 

separate<-function(df, n){
    pieces<-vector("list",n)
    rows<-nrow(df)
    width<-floor(nrow(df)/n) #if n does not evenly divide the number of rows in the dataframe, the last piece will be bigger
    for(i in 1:(n-1)){  
        pieces[[i]] <- df[(((i-1)*width)+1):(i*width),]
        }
    pieces[[n]]<-df[(((n-1)*width)+1):(nrow(df)),]

    return(pieces)   
    }

bind<-function(pieces){
    n<-length(pieces)
    clumps<-vector("list", n)
    for(i in (1:n)){
        clumps[[i]]<-bind_rows(pieces[c(-i)])
    }
    return(clumps)    
}



#Now we write functions to build the models from the clumps to predict the pieces.

build_models<-function(clumps){
    clump_models<-vector("list", length(clumps))
    for(i in (1:length(clumps))){
        clump_models[[i]]<-glm(Survived~Pclass+Sex+SibSp, family = "binomial", data = clumps[[i]])
    }
    return(clump_models)
}


get_predictions<-function(clump_models, pieces){
    n<- length(pieces)
    predictions<-vector("list", n)
    for(i in (1:n)){
        predictions[[i]]<-predict(clump_models[[i]], newdata = pieces[[i]], type = "response")
    
        
    }  
    return(predictions)
   
}
        
        
        

#Now we actually do the separating and modelling

#Get a random ordering of the training set
set.seed(101)
permutation<-sample(nrow(train))
permuted_train<-train[permutation,]


pieces<-separate(permuted_train,10)
clumps<-bind(pieces)
clump_models<-build_models(clumps)
predicted_chances<-get_predictions(clump_models, pieces)



threshold<- 0.5
predictions<-vector("list", length(predicted_chances))
for(i in (1:length(predicted_chances))){
    predictions[[i]]<-ifelse(predicted_chances[[i]]>=threshold, 1, 0)
}

conf<-vector("list", length(predictions))
for(i in (1:length(conf))){
    conf[[i]]<-table(pieces[[i]]$Survived, predictions[[i]])
}

accuracy<-(1:length(conf))
for(i in 1:length(conf)){
    accuracy[i]<-sum(diag(conf[[i]]))/sum(conf[[i]])
}

accuracy

#Make predictions on the test set and put them together
test_predict<-function(clump_models){
    test_pred<-vector("list", length(clump_models))
    for(i in 1:length(test_pred)){
        test_pred[[i]]<-predict(clump_models[[i]], newdata = test, type = "response")
    }
    return(test_pred)
}

test_pred<-test_predict(clump_models)
pred_matrix<-test_pred[[1]]

for(i in 2:length(test_pred)){
    pred_matrix<-cbind(pred_matrix, test_pred[[i]])
}

test_pred7<-rowMeans(pred_matrix)
head(test_pred7)

submission7<-test["PassengerId"]%>%mutate(Survived = as.integer(test_pred7>=0.5), PassengerId = as.integer(PassengerId))

submission7%>%filter(Survived ==1)%>%summarize(n()/nrow(submission7))
write_csv(submission7, "submission7.csv")

head(submission7)

length(which(submission7$Survived!=submission3$Survived))


library(caret)

trainf<-train
trainf$Survived<-as.factor(trainf$Survived)
str(trainf)
loomodel<-train(Survived ~ Pclass+Sex+SibSp, method = "glm", data = trainf, trControl = trainControl(method = "LOOCV"))

test_pred8<-predict(loomodel, newdata = test, type= "raw")
head(test_pred8)

test_pred8<-matrix(as.integer(test_pred8), ncol = 1)
test_pred8<-test_pred8 -1
head(test_pred8)
submission8<-test["PassengerId"]%>%mutate(PassengerId = as.integer(PassengerId), Survived = test_pred8)
head(submission8)
length(which(submission8$Survived!=submission3$Survived))

write_csv(submission8, "submission8.csv")

train_tree<-train%>%mutate(Survived = as.factor(ifelse(Survived==1,"Survived", "Died")))%>%mutate(Sex = as.factor(Sex))
train_tree$Embarked[which(is.na(train_tree$Embarked))]<-"S"
train_tree<-train_tree%>%mutate(Embarked = as.factor(Embarked))
head(train_tree)
str(train_tree)

#Let's make a box plot to see how fares are distributed in the 3 classes. 
ggplot(train%>%group_by(Pclass)%>%filter(Fare<500), aes(x=Pclass, y=Fare))+geom_boxplot()

#Build a scatterplot of fares, coloring each dot by whether that passenger survived
ggplot(train%>%filter(Fare<300), aes(x=PassengerId, y=Fare, color = Survived))+geom_point() 
#We have 3 outlier2 who paid 512-pound fares(and all survived), to keep the graph simple we'll exclude them

#First group passengers by fare, rounded to the nearest 5 and calculate the percentage chance of survival in each group
fare_groups<-train_tree%>%mutate(Rounded_fare = 5*round(Fare/5))%>%group_by(Rounded_fare)%>%mutate(Percent_survived = length(which(Survived =="Survived"))/n() )
ggplot(fare_groups%>%group_by(Rounded_fare)%>%mutate(Fare_count = n(), Average_class = mean(Pclass))%>%filter(Fare<300), aes(x=Rounded_fare, y=Percent_survived, size = Fare_count, color =Average_class))+
geom_point()+scale_x_log10()

fare_groups%>%filter(Rounded_fare<100 & Percent_survived == 0) #It looks like this was mainly a family (with 5 children) who all died together. 
fare_groups%>%filter(Rounded_fare>100 & Percent_survived == 0) #Just an outlier, no extra informations about him

library(tree)

tree_model1<- tree(Survived ~ Pclass+Age + Sex+ Fare+SibSp, data = train_tree)
summary(tree_model1)

plot(tree_model1)
text(tree_model1, pretty = 0)

#Now prepare the test set to fit the model assumptions, and get predictions on it. 
test_tree<-test%>%mutate(Sex = as.factor(Sex), Embarked = as.factor(Embarked))
tree_pred1<-predict(tree_model1, newdata = test_tree, type = "vector")
head(tree_pred1)

submission12<-cbind(test["PassengerId"], as.integer(tree_pred1[,"Survived"]>=0.5))
names(submission12)[2]<-"Survived"

submission12<-submission12%>%mutate(PassengerId = as.integer(PassengerId))
head(submission12)
write_csv(submission12, "submission12.csv")

#Replace missing ages with mean
train_tree_age_mean<-train_tree
train_tree_age_mean$Age[which(is.na(train_tree$Age))]<-mean(train_tree$Age, na.rm = TRUE)

#Build a new model with these data included
tree_model2<- tree(Survived ~ Pclass+Age + Sex+ Fare+SibSp, data = train_tree_age_mean)
summary(tree_model2)

plot(tree_model2)
text(tree_model2, pretty = 0)

#Tree looks good. Let's make the predictions and submit. 
tree_pred2<-predict(tree_model2, newdata = test_tree, type = "vector")

submission13<-cbind(test["PassengerId"], as.integer(tree_pred2[,"Survived"]>=0.5))
names(submission13)[2]<-"Survived"

submission13<-submission13%>%mutate(PassengerId = as.integer(PassengerId))
head(submission13)
write_csv(submission13, "submission13.csv")

#Create a new data frame, where Age is rounded off and treated as an ordered factor, with an extra 'none' category
train_tree_age_missing<-train_tree%>%mutate(Age_bin = bin_namer(Age, 5))

#Treat our Age_bin variable as an ordered factor
ages<- c("0 - 5","5 - 10","10 - 15", "15 - 20","20 - 25", "25 - 30","30 - 35", "35 - 40", "40 - 45", "45 - 50", "50 - 55", "55 - 60", "60 - 65", "65 - 70", "70 - 75", "75 - 80", "80 - 85", "NA - NA" )

train_tree_age_missing$Age_bin<-factor(train_tree_age_missing$Age_bin, ordered = TRUE, levels = ages)

test_tree_age_missing<-test_tree%>%mutate(Age_bin = bin_namer(Age, 5))
test_tree_age_missing$Age_bin<-factor(test_tree_age_missing$Age_bin, ordered = TRUE, levels = ages)

#Build the tree model on this training set, and see how it goes.

tree_model3<- tree(Survived ~ Pclass+Age_bin + Sex+ Fare+SibSp, data = train_tree_age_missing)
summary(tree_model3)
plot(tree_model3)
text(tree_model3, pretty = 1)

tree_pred3<-predict(tree_model3, newdata = test_tree_age_missing, type = "vector")

submission14<-cbind(test["PassengerId"], as.integer(tree_pred3[,"Survived"]>=0.5))
names(submission14)[2]<-"Survived"

submission14<-submission14%>%mutate(PassengerId = as.integer(PassengerId))
head(submission14)
write_csv(submission14, "submission14.csv")

#Make a dataframe containing just the passengers with cabins
cabins<-train%>%filter(!is.na(Cabin))
#Percent chance of survival for passengers in cabins
cabins%>%summarize(Percent_Survived = sum(Survived)/n())

train_tree_cabins<-train_tree_age_mean%>%mutate(Has_cabin = as.integer(!is.na(Cabin)), Deck = Cabin)

train_tree_cabins$Deck[grepl("^A", train_tree_cabins$Cabin)]<-"A"
train_tree_cabins$Deck[grepl("^B", train_tree_cabins$Cabin)]<-"B"
train_tree_cabins$Deck[grepl("^C", train_tree_cabins$Cabin)]<-"C"
train_tree_cabins$Deck[grepl("^D", train_tree_cabins$Cabin)]<-"D"
train_tree_cabins$Deck[grepl("^E", train_tree_cabins$Cabin)]<-"E"
train_tree_cabins$Deck[grepl("^F", train_tree_cabins$Cabin)]<-"F"
train_tree_cabins$Deck[grepl("^G", train_tree_cabins$Cabin)]<-"G"
train_tree_cabins$Deck[is.na(train_tree_cabins$Deck)]<-"none"
head(train_tree_cabins)

train_tree_cabins%>%group_by(Deck)%>%summarize(percent_survived = sum(Survived=="Survived")/n())

#Turn the Deck and Has_cabin variables into factors
train_tree_cabins$Deck<-factor(train_tree_cabins$Deck, ordered = TRUE, levels = c("none", "T", "G", "F", "E", "D", "C", "B", "A"))
train_tree_cabins$Has_cabin<-factor(train_tree_cabins$Has_cabin, ordered = TRUE, levels = c("0", "1"))

#Build the model
tree_model4<- tree(Survived ~ Pclass+Age + Sex+ Fare+SibSp+Has_cabin+Deck, data = train_tree_cabins)
summary(tree_model4)
plot(tree_model4)
text(tree_model4, pretty = 0)

#Prepare the Has_cabin and Deck fields for the test set
test_tree_cabins<-test_tree%>%mutate(Has_cabin = as.integer(!is.na(Cabin)), Deck = Cabin)

test_tree_cabins$Deck[grepl("^A", test_tree_cabins$Cabin)]<-"A"
test_tree_cabins$Deck[grepl("^B", test_tree_cabins$Cabin)]<-"B"
test_tree_cabins$Deck[grepl("^C", test_tree_cabins$Cabin)]<-"C"
test_tree_cabins$Deck[grepl("^D", test_tree_cabins$Cabin)]<-"D"
test_tree_cabins$Deck[grepl("^E", test_tree_cabins$Cabin)]<-"E"
test_tree_cabins$Deck[grepl("^F", test_tree_cabins$Cabin)]<-"F"
test_tree_cabins$Deck[grepl("^G", test_tree_cabins$Cabin)]<-"G"
test_tree_cabins$Deck[is.na(test_tree_cabins$Deck)]<-"none"

test_tree_cabins$Deck<-factor(test_tree_cabins$Deck, ordered = TRUE, levels = c("none", "T", "G", "F", "E", "D", "C", "B", "A"))
test_tree_cabins$Has_cabin<-factor(test_tree_cabins$Has_cabin, ordered = TRUE, levels = c("0", "1"))


#Get the predictions
tree_pred4<-predict(tree_model4, newdata = test_tree_cabins, type = "vector")

submission15<-cbind(test["PassengerId"], as.integer(tree_pred4[,"Survived"]>=0.5))
names(submission15)[2]<-"Survived"

submission15<-submission15%>%mutate(PassengerId = as.integer(PassengerId))
head(submission15)
write_csv(submission15, "submission15.csv")

library(randomForest)

#Prepare the training set by removing extraneous variables
train_tree_cabins_nona<-train_tree_cabins[,c("PassengerId","Survived", "Pclass","Age","Sex", "Fare", "SibSp","Has_cabin","Deck")]

#Build a random forest
rf_model<-randomForest(Survived~.-PassengerId, data=train_tree_cabins_nona)

rf_model
plot(rf_model)


#Make predictions on the test data.
#First, fill in the missing Fare and Age values in the test data
test_tree_cabins_nona<-test_tree_cabins
test_tree_cabins_nona$Fare[is.na(test_tree_cabins$Fare)]<-median(filter(test_tree_cabins, Pclass ==  3)$Fare, na.rm=TRUE)
test_tree_cabins_nona$Age[is.na(test_tree_cabins$Age)]<-mean(test_tree_cabins$Age, na.rm=TRUE)
test_tree_cabins_nona<-test_tree_cabins_nona%>%mutate(Survived = as.factor("unknown"))
test_tree_cabins_nona<-test_tree_cabins_nona[,c("PassengerId", "Survived", "Pclass","Age","Sex", "Fare", "SibSp","Has_cabin","Deck")]
#Now, get the predictions
test_pred_rf<-predict(rf_model, newdata = test_tree_cabins_nona, type = "prob")

head(test_pred_rf)

#Prepare the submission
submission16<-cbind(test["PassengerId"], as.integer(test_pred_rf[,"Survived"]>=0.5))
names(submission16)[2]<-"Survived"

submission16<-submission16%>%mutate(PassengerId = as.integer(PassengerId))
head(submission16)
write_csv(submission16, "submission16.csv")
length(which(submission15$Survived!=submission16$Survived))

#XGBOOST
library(xgboost)

#Now, prepare our data into a Dmatrix, so we can build an xgboost model on it. All the data needs to be numeric,
#and we need to remove any fields we're not planning on using. 

xgb_train<-train_tree_cabins%>%select("Pclass","Age","Sex", "Fare", "SibSp", "Has_cabin", "Deck")
xgb_train_labels<-train[,"Survived"]
#deck<-model.matrix(~Deck-1,xgb_train)

#Right now, Sex, Has_cabin, and Deck are all factors instead of numbers, but we can easily convert them
xgb_train<-xgb_train%>%mutate(Is_female = as.numeric(Sex=="female"), Has_cabin = as.numeric(Has_cabin)-1, Deck_number = as.numeric(Deck)-1)%>%select(-c(Sex,Deck))

#xgb_train<-cbind(xgb_train, deck)

str(xgb_train)
str(xgb_train_labels)


#Convert our dataframes to matrices, and then to a DMatrix
xgb_train_matrix <- data.matrix(xgb_train)
xgb_train_labels<-data.matrix(xgb_train_labels)
dtrain<-xgb.DMatrix(data = xgb_train_matrix, label= xgb_train_labels)

#Now build the model
modelxgb <- xgboost(data = dtrain, nround = 5, objective = "binary:logistic")


#Now we prepare the test data the same way, and then make predictions. 
xgb_test<-test_tree_cabins%>%select("Pclass","Age","Sex", "Fare", "SibSp","Has_cabin","Deck")
#test_deck<-model.matrix(~Deck-1,xgb_test)
xgb_test<-xgb_test%>%mutate(Is_female = as.numeric(Sex=="female"), Has_cabin = as.numeric(Has_cabin)-1, Deck_number = as.numeric(Deck)-1)%>%select(-c(Sex, Deck))
xgb_test$Age[which(is.na(xgb_test$Age))]<-mean(xgb_test$Age, na.rm = TRUE)
#xgb_test<-cbind(xgb_test, test_deck)
xgb_test_matrix <- data.matrix(xgb_test)
dtest<-xgb.DMatrix(data = xgb_test_matrix)

#Make predictions
test_pred_xgb<-predict(modelxgb, newdata = dtest)

#Prepare the submission
submission17<-cbind(test["PassengerId"], as.integer(test_pred_xgb>=0.5))
names(submission17)[2]<-"Survived"

submission17<-submission17%>%mutate(PassengerId = as.integer(PassengerId))
head(submission17)
write_csv(submission17, "submission17.csv")


#Randomize a split into a training and crossval set
set.seed(111)
permutation<-sample(nrow(train))
xgb_train_permuted<-xgb_train_matrix[permutation,]
xgb_train_labels_permuted<-xgb_train_labels[permutation,]
head(xgb_train_permuted)

dtrain<-xgb_train_permuted[1:624,]
dcross<-xgb_train_permuted[625:891,]
dtrain_labels<-xgb_train_labels_permuted[1:624]
dcross_labels<-xgb_train_labels_permuted[625:891]

dtrain<-xgb.DMatrix(data = dtrain, label= dtrain_labels)
dcross<-xgb.DMatrix(data = dcross, label = dcross_labels)

#Now build some models
negatives<- sum(dtrain_labels == FALSE)
positives <- sum(dtrain_labels == TRUE)

modelxgb_tune1 <- xgboost(data = dtrain, nround = 11, max.depth = 6, early_stopping_rounds = 3,
                    objective = "binary:logistic", 
                    scale_pos_weight = negatives/positives
                         )


#Predict against our crossval set, and see how it does
cross_pred_tune1<-predict(modelxgb_tune1, newdata = dcross)
err <- mean(as.numeric(cross_pred_tune1 > 0.5) != dcross_labels)
print(paste("test-error=", err))
print(positives)
print(negatives)

xgb.plot.multi.trees(feature_names = names(xgb_train_matrix), model = modelxgb_tune1)
importance_matrix <- xgb.importance(names(xgb_train_matrix), model = modelxgb_tune1)

xgb.plot.importance(importance_matrix)

#Make predictions
test_pred_xgb_tune1<-predict(modelxgb_tune1, newdata = dtest)
submission18<-cbind(test["PassengerId"], as.integer(test_pred_xgb_tune1>=0.5))
names(submission18)[2]<-"Survived"

submission18<-submission18%>%mutate(PassengerId = as.integer(PassengerId))
head(submission18)
write_csv(submission18, "submission18.csv")

which(submission15$Survived!=submission18$Survived)
