## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 100)


## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)
library(dplyr)
library(forcats)
library(Hmisc)
library(randomForest)
library(caret)
library(corrplot)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(train)
head(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(test)
head(test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test$Survived <- NA
all <- rbind(train, test)
dim(all)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sort(colSums(is.na(all)), decreasing = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Survived <- as.factor(all$Survived)
all$Pclass <- as.ordered(all$Pclass) #Pclass has ordered values, so converting it into ordinal factor
all$Sex <- as.factor(all$Sex)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Embarked[is.na(all$Embarked)] <- 'S'
all$Embarked <- as.factor(all$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Fare[is.na(all$Fare)] <- 8.0500


## ----fig.align="center"----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data=all[!is.na(all$Survived),], aes(x=Survived)) + 
  geom_bar(stat='count', aes(fill=Survived)) +
  geom_label(stat='count', aes(label=..count..)) + 
  labs(x="not survived and survived in train data") + 
  theme_grey(base_size = 15)


## ----fig.align="center", fig.width=10--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(data=all, aes(x=Sex)) + 
        geom_bar(stat='count', aes(fill=Sex)) + 
        geom_label(stat='count', aes(label=..count..)) + 
        labs(x="Gender ratio (total data)")

p2 <- ggplot(data=all[!is.na(all$Survived),], aes(x=Sex, group=Survived)) + 
        geom_bar(stat='count', aes(fill=Survived), position = "dodge") + 
        geom_label(stat='count', aes(label=..count..), size = 3, position = position_dodge(0.9)) + 
        labs(x="Survived ratio by Gender") 

p3 <- ggplot(data=all[!is.na(all$Survived),], aes(x=Sex, fill=Survived)) + 
        geom_bar(stat='count', position = "fill") + 
        labs(x="Survived ratio by Gender", y="percent")

grid.arrange(p1, p2, p3, ncol=3)


## ----fig.width=10, fig.align="center"--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p4 <- ggplot(data=all, aes(x=Pclass)) + 
        geom_bar(stat='count', aes(fill=Pclass), position="dodge") + 
        geom_label(stat="count", aes(label=..count..)) + 
        labs(x="Passenger class ratio (total data)")

p5 <- ggplot(data=all[!is.na(all$Survived),], aes(x=Pclass, group = Survived)) + 
        geom_bar(stat = "count", aes(fill=Survived), position="dodge") + 
        geom_label(stat = "count", aes(label=..count..), size=3, position = position_dodge(0.9)) + 
        labs(x="Survived ratio by Pclass")

p6 <- ggplot(data=all[!is.na(all$Survived),], aes(x=Pclass, fill=Survived)) + 
        geom_bar(stat="count", position="fill") + 
        labs(x="Survived ratio by Pclass", y="percent")

grid.arrange(p4, p5, p6, ncol=3)

p7 <- ggplot(data=all[!is.na(all$Survived),], aes(x=Pclass, group = Survived)) + 
        geom_bar(stat = "count", aes(fill=Survived), position="dodge") + 
        geom_label(stat = "count", aes(label=..count..), size=3, position = position_dodge(0.9)) + 
        labs(x="Survived ratio by Pclass and Gender") + 
        facet_grid(.~Sex)

p8 <- ggplot(data=all[!is.na(all$Survived),], aes(x=Pclass, fill=Survived)) + 
        geom_bar(stat = "count", position="fill") + 
        labs(x="Survived ratio by Pclass and Gender", y="Percent") + 
        facet_grid(.~Sex)

grid.arrange(p7, p8, ncol=2)


## ----fig.align="center", fig.width=10--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p9 <- ggplot(data=all[!is.na(all$Survived),], aes(x=fct_infreq(Embarked))) + 
        geom_bar(stat='count', aes(fill=Embarked)) + 
        geom_label(stat='count', aes(label=..count..)) + 
        labs(x="Passengers by Embarked")

p10 <- ggplot(data=all[!is.na(all$Survived),], aes(x=fct_infreq(Embarked), group=Survived)) + 
        geom_bar(stat='count', aes(fill=Survived), position="dodge") + 
        geom_label(stat='count', aes(label=..count..), position = position_dodge(0.9)) + 
       labs(x="Survived ratio by Embarked")

grid.arrange(p9, p10, ncol=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$GenderClass <- paste0(all$Sex, "P", all$Pclass)
all$GenderClass <- as.factor(all$GenderClass)


## ----fig.align="center", fig.width=10--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p9 <- ggplot(data = all[!is.na(all$Survived),], aes(x=GenderClass, group=Survived)) + 
  geom_bar(stat='count', aes(fill=Survived), position = "dodge") + 
  geom_label(stat='count', aes(label=..count..), size=3, position = position_dodge(0.9)) + 
  labs(x="Survived ratio by GenderClass")  

p10 <- ggplot(data = all[!is.na(all$Survived), ], aes(x=GenderClass)) + 
  geom_bar(stat = 'count', aes(fill=Survived), position = "fill") + 
  labs(x="Survived ratio by GenderClass", y="percent")

grid.arrange(p9, p10, ncol=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#760 "Rothes, Mrs. Norman Leslie (Lucy Noel Martha Dyer-Edwards)"
#711 "Mayne, Mlle. Berthe Antonine"
#797 "Leader, Dr. Alice (Farnham)"

all$Title <- sapply(all$Name, function(x){trimws(unlist(strsplit(x, split = '[,.]'))[2])})
table(all$Sex, all$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Title[all$Title %in% c("Don", "Sir", "Jonkheer", "Rev", "Major", "Col", "Capt")] <- "Mr"
all$Title[all$Title %in% c("Lady", "the Countess")] <- "Mrs"
all$Title[all$Title %in% c("Ms", "Dona", "Mlle", "Mme")] <- "Miss"
all$Title[all$Title == 'Dr' & all$Sex == 'male'] <- "Mr"
all$Title[all$Title == 'Dr' & all$Sex == 'female'] <- "Mrs"

table(all$Sex, all$Title)


## ----fig.align="center", fig.width=10, fig.height=10-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p11 <- ggplot(data=all[!is.na(all$Survived),], aes(x=Pclass, group=Survived)) + 
        geom_bar(stat='count', aes(fill=Survived), position="dodge") + 
        geom_label(stat='count', aes(label=..count..), size=3, position = position_dodge(0.9)) + 
        labs(x="Survived ratio by Pclass and Title") + 
        facet_grid(.~Title)

p12 <- ggplot(data=all[!is.na(all$Survived),], aes(x=Pclass)) + 
        geom_bar(stat='count', aes(fill=Survived), position="fill") + 
        labs(x="Survived ratio by Pclass and Title", y="percent") + 
        facet_grid(.~Title)

grid.arrange(p11, p12, nrow=2)


## ----fig.align="center", fig.width=10--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$TitleClass <- paste0(all$Title, "P", all$Pclass)
all$TitleClass <- as.factor(all$TitleClass)

p13 <-ggplot(data=all[!is.na(all$Survived),], aes(x=TitleClass)) + 
        geom_bar(stat='count', aes(fill=Survived), position="fill") + 
        labs(x="Survived ratio by TitleClass", y="percent")

grid.arrange(p13)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$TicketPrefix <- sapply(all$Ticket, function(x){trimws(unlist(strsplit(x, split=' '))[1])})
HasTicketPrefix <- sapply(all$Ticket, function(x){grepl("\\D", x)})
all$TicketPrefix[!HasTicketPrefix] <- "No Prefix"
all$TicketPrefix[all$TicketPrefix %in% c("PC")] <- "PC"
all$TicketPrefix[!all$TicketPrefix %in% c("PC")] <- "Other"
rm(HasTicketPrefix)


## ----fig.width=10, fig.align="center"--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p14 <- ggplot(data=all[!is.na(all$Survived),], aes(x=TicketPrefix, group=Survived)) + 
  geom_bar(stat='count', aes(fill=Survived), position = 'dodge') + 
  geom_label(stat='count', aes(label=..count..), position = position_dodge(0.9)) + 
  facet_grid(.~Sex)

p15 <- ggplot(data=all[!is.na(all$Survived),], aes(x=TicketPrefix)) + 
  geom_bar(stat='count', aes(fill=Survived), position = 'fill') + 
  labs(y="percent")

grid.arrange(p14, p15, ncol=2)


## ----fig.align="center", fig.width=10, fig.height=10-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$FSize <- all$SibSp + all$Parch + 1

p16 <- ggplot(data=all[!is.na(all$Survived),], aes(x=factor(FSize), group=Survived)) + 
          geom_bar(stat='count', aes(fill=Survived), position="dodge") +
          geom_label(stat='count', aes(label=..count..), position = position_dodge(0.9)) + 
          labs(x="Survived ratio by Family Size") 

p17 <- ggplot(data=all[!is.na(all$Survived),], aes(x=factor(FSize))) + 
          geom_bar(stat='count', aes(fill=Survived), position="fill") + 
          labs(x="Survived ratio by Family Size", y="percent") 

grid.arrange(p16, p17, nrow=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
GroupFunction <- function(x){
  if(x==1){
    return("Solo")
  }else if(x==2){
    return("Duo")
  }else if(x>=3 & x<=4){
    return("SmallFamily")
  }else{
    return("LargeFamily")
  }
}

all$FSizeGroup <- sapply(all$FSize, GroupFunction)
rm(GroupFunction)


## ----fig.align="center", fig.width=10--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p18 <- ggplot(data=all[!is.na(all$Survived),], aes(x=FSizeGroup, group=Survived)) + 
          geom_bar(stat='count', aes(fill=Survived), position = "dodge") + 
          geom_label(stat='count', aes(label=..count..), position = position_dodge(0.9)) + 
          labs(x="Survived ratio by Family Group")

p19 <- ggplot(data=all[!is.na(all$Survived),], aes(x=FSizeGroup)) + 
          geom_bar(stat='count', aes(fill=Survived), position = "fill") + 
          labs(x="Survived ratio by Family Group", y="percent")

grid.arrange(p18, p19, ncol=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
PPTicket <- all %>% group_by(Ticket=all$Ticket) %>% summarise(PPTicket=n())
all$PPTicket <- PPTicket$PPTicket[match(all$Ticket, PPTicket$Ticket)]
rm(PPTicket)

all$TicketShared <- sapply(all$PPTicket, function(x){ifelse(x==1, "Not Shared", "Shared")})


## ----fig.align="center", fig.width=10, fig.height=10-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p20 <- ggplot(data=all[!is.na(all$Survived),], aes(x=factor(PPTicket), group=Survived)) + 
          geom_bar(stat='count', aes(fill=Survived), position = "dodge") + 
          geom_label(stat='count', aes(label=..count..), position = position_dodge(0.9)) + 
          labs(x="Survived ratio by Passengers per Ticket")

p21 <- ggplot(data=all[!is.na(all$Survived),], aes(x=factor(PPTicket))) + 
          geom_bar(stat='count', aes(fill=Survived), position = "fill") + 
          labs(x="Survived ratio by Passengers per Ticket", y="percent") 

grid.arrange(p20, p21, nrow=2)


## ----fig.align="center", fig.width=10--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p22 <- ggplot(data=all[!is.na(all$Survived),], aes(x=TicketShared, group=Survived)) + 
          geom_bar(stat='count', aes(fill=Survived), position = "dodge") + 
          geom_label(stat='count', aes(label=..count..), position = position_dodge(0.9)) + 
          labs(x="Survived ratio by Sharing Ticket")

p23 <- ggplot(data=all[!is.na(all$Survived),], aes(x=TicketShared)) + 
          geom_bar(stat='count', aes(fill=Survived), position = "fill") + 
          labs(x="Survived ratio by Sharing Ticket", y="percent") 

grid.arrange(p22, p23, ncol=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$PGroupSize <- mapply(function(a, b){max(a, b)}, all$FSize, all$PPTicket)
all$PGroupSize <- as.numeric(all$PGroupSize)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
GroupFunction <- function(x){
  if(x==1){
    return("Single")
  }else if(x==2){
    return("Duo")
  }else if(x>=3 & x<=4){
    return("Group")
  }else{
    return("Large Group")
  }
}

all$PGroup <- sapply(all$PGroupSize, GroupFunction)
rm(GroupFunction)


## ----fig.align="center", fig.width=10--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p24 <- ggplot(data=all[!is.na(all$Survived),], aes(x=factor(PGroup), group=Survived)) + 
          geom_bar(stat='count', aes(fill=Survived), position = "dodge") + 
          geom_label(stat='count', aes(label=..count..), position = position_dodge(0.9)) + 
          labs(x="Survived ratio by Passenger Group")

p25 <- ggplot(data=all[!is.na(all$Survived),], aes(x=factor(PGroup))) + 
          geom_bar(stat='count', aes(fill=Survived), position = "fill") + 
          labs(x="Survived ratio by Passenger Group", y="percent")

grid.arrange(p24, p25, ncol=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
CabinDetails <- all[!is.na(all$Cabin), c("Ticket", "Cabin")]
all$CabinModified <- CabinDetails$Cabin[match(all$Ticket, CabinDetails$Ticket)]
all$CabinModified[is.na(all$CabinModified)] <- "N"
all$CabinPrefix <- substr(all$CabinModified, 1, 1)

all$CabinModified <- NULL
rm(CabinDetails)


## ----fig.align="center", fig.width=10, fig.height=10-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p26 <- ggplot(data=all[!is.na(all$Survived),], aes(x=CabinPrefix, group=Survived)) + 
          geom_bar(stat='count', aes(fill=Survived), position = "dodge") + 
          geom_label(stat='count', aes(label=..count..), position = position_dodge(0.9)) + 
          labs(x="Survived ration by Cabin Class")

p27 <- ggplot(data=all[!is.na(all$Survived),], aes(x=CabinPrefix)) + 
          geom_bar(stat='count', aes(fill=Survived), position = "fill") + 
          labs(x="Survived ration by Cabin Class", y="percent") 
grid.arrange(p26, p27, nrow=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$FarePP <- all$Fare/all$PPTicket


## ----fig.width=10, fig.align=10--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data=all[all$CabinPrefix!="N",], aes(x=CabinPrefix, group=Pclass)) + 
          geom_bar(stat='count', aes(fill=Pclass), position = "dodge") + 
          geom_label(stat='count', aes(label=..count..), position = position_dodge(0.9))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data=all, aes(x=FarePP)) + 
          geom_histogram(binwidth = 5, fill='blue', boundary=0) + 
          scale_x_continuous(breaks = seq(0, 150, by=5))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$FareBins <- cut2(all$FarePP, g=5)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data=all[!is.na(all$Survived) & !is.na(all$Age),], aes(x=Age, group=Survived)) + 
  geom_density(alpha=0.5, aes(fill=Survived)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
AgeLM <- lm(Age ~ Pclass + Title + SibSp + Parch, data=all[!is.na(all$Age),])
summary(AgeLM)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$AgeLM <- predict(AgeLM, newdata = all)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Title <- as.factor(all$Title)
all$TicketPrefix <- as.factor(all$TicketPrefix)
all$FSizeGroup <- as.factor(all$FSizeGroup)
all$TicketShared <- as.factor(all$TicketShared)
all$PGroup <- as.factor(all$PGroup)
all$CabinPrefix <- as.factor(all$CabinPrefix)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#quick_RF <- randomForest(x=all[!is.na(all$Survived),-c(1, 2, 4, 6, 9, 11)], y=all[!is.na(all$Survived),2], ntree=100, importance = TRUE)
#quick_RF <- randomForest(Survived ~ ., data=all[!is.na(all$Survived),-c(1, 4, 6, 9, 11)], ntree=1000, importance = TRUE)
#varImpPlot(quick_RF, main = "Variable Importance")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainClean <- all[!is.na(all$Survived), c("Survived", "TitleClass", "FarePP", "PGroupSize")]
testClean <- all[is.na(all$Survived), c("Survived", "TitleClass", "FarePP", "PGroupSize")]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2018)
model_RF <- train(x=trainClean[,-1], y=trainClean$Survived, data=trainClean, method='rf', trControl=trainControl(method = 'cv', number=10))
model_RF
model_RF$results


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
varImpPlot(model_RF$finalModel)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prediction_RF <- predict(model_RF, testClean)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2018)
model_SVM <- train(Survived ~ TitleClass + FarePP + PGroupSize, data=trainClean, method='svmRadial', preProcess= c('center', 'scale'), trControl=trainControl(method="cv", number=10))
model_SVM
model_SVM$results


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prediction_SVM <- predict(model_SVM, testClean)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2018)
model_GBM <- train(Survived~ TitleClass + FarePP + PGroupSize, data=trainClean, method='gbm', preProcess= c('center', 'scale'), trControl=trainControl(method="cv", number=10), verbose=FALSE)
model_GBM
model_GBM$results


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prediction_GBM <- predict(model_GBM, testClean)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

testClean$RF <- as.numeric(prediction_RF)-1
testClean$SVM <- as.numeric(prediction_SVM)-1
testClean$GBM <- as.numeric(prediction_GBM)-1


corrplot.mixed(cor(testClean[, c('RF', 'SVM', 'GBM')]), order="hclust", tl.col="black")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testClean$Sum <- testClean$RF + testClean$SVM + testClean$GBM
testClean$Majority <- ifelse(testClean$Sum<=1, 0, 1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
submission_select <- data.frame(PassengerId = test$PassengerId, Survived = testClean$Majority)
write.csv(submission_select, file='Titanic_Submission.csv', row.names = FALSE)

