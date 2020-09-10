## ---- message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret)
library(ggplot2)
library(randomForest)# Random forest tree
library(mice)        # md.pattern looking for missing values
library(VIM)         # Visualize the missing value
library(party)       # Cforest
library(rpart)       # Decision tree
library(rpart.plot)  # Plot decision tree
library(doSNOW)      # For multi-core training
library(infotheo)    # The definition of mutual information.

# Load raw data
train<-read.csv("../input/train.csv",na.strings ="")
test<-read.csv("../input/test.csv",na.strings = "")

# Add "Survived" variable to the test dataset
test$Survived<-NA

# Combine train and test dataset into cb
cb <- rbind(train, test)

# Take a look about the dataset
str(cb)

# Change data type of some variables 
cb$Survived <- as.factor(cb$Survived)
cb$Pclass <- as.factor(cb$Pclass)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
colSums(is.na(cb))
md.pattern(cb)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Prob.PS<-prop.table(table(cb$Survived,cb$Pclass),2)
Prob.PS
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extract the title and familyname from "Name" variable
cb$FamilyName<-gsub(',(.*?)\\.(.*)', '', cb$Name)
cb$Title<-gsub('(.*?), |(\\..*)', '', cb$Name)

table(cb$Title)
rare_title <- c('Mme','Ms','Mlle','Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
cb$Title[cb$Title %in% rare_title]  <- "other"
cb$Title <- as.factor(cb$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = Sex, fill = factor(Survived))) +
  geom_bar() +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived") 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table((table(cb[1:891,]$Survived,cb[1:891,]$Pclass,cb[1:891,]$Title)),c(2,3))
ggplot(cb[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(cb[1:891,]$Survived,cb[1:891,]$Sex,cb[1:891,]$Pclass),c(2,3))
ggplot(cb[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(cb$Age)
summary(cb[1:891,"Age"])
summary(cb[891:1309,"Age"])
ggplot(cb[1:891,], aes(x = Age, fill = Survived)) +
    geom_histogram(binwidth = 10) +
    xlab("Age") +
    ylab("Total Count")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(cb[which(cb$Title == "Master"), ]$Age)

Miss <- cb[which(cb$Title == "Miss"), ]
summary(Miss$Age)
MissAlone <- Miss[which(Miss$SibSp == 0 & Miss$Parch == 0),]
summary(MissAlone$Age)
length(which(MissAlone$Age <= 14.5))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(cb$SibSp)
length(table(cb$SibSp))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(cb[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass & Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(cb$Parch)
length(table(cb$Parch))
ggplot(cb[1:891,], aes(x = Parch, fill = Survived)) +
 geom_histogram(binwidth = 1)  +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass & Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
length(unique(cb$Ticket))
cb$Ticket <- as.character(cb$Ticket)
head(cb$Ticket,10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# We'll start with taking a look at the first character for each ticket
# The numbers of levels become 16 levels.
TicketFirstCharacter<-substr(cb$Ticket, 1, 1)
unique(TicketFirstCharacter)
length(unique(TicketFirstCharacter))


# we can make TicketFirstCharacter as factor for analysis and visualize
cb$TicketFirstCharacter <- as.factor(TicketFirstCharacter)

# First, see the surface of the TicketFirstCharacter with survived
ggplot(cb[1:891,], aes(x = TicketFirstCharacter, fill = Survived)) +
  geom_bar() +
  xlab("TicketFirstCharacter") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# TicketFirstCharacter might be predictive, drill down a little bit
ggplot(cb[1:891,], aes(x = TicketFirstCharacter, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("TicketFirstCharacter") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Plus title to see whether it have pattern or not.
ggplot(cb[1:891,], aes(x = TicketFirstCharacter, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("TicketFirstCharacter") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check the fare
class(cb$Fare)
summary(cb$Fare)
length(unique(cb$Fare))

# Fare is continuous variable, it can't be factor. so it needs to use histogram.
ggplot(cb, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)


# plus Pclass and Title to see whether fare has predictive power or not
ggplot(cb[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass ) + 
  ggtitle("Pclass, Title") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's take a look about cabin variable
str(cb$Cabin)
sum(is.na(cb$Cabin))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Turn factor cabin into character cabin 
cb$Cabin <- as.character(cb$Cabin)

# Replace empty cabins by "U"
cb[which(is.na(cb$Cabin)), "Cabin"] <- "U"

# Assign cabin first character into CabinFirstCharacter and take a look at it
cb$CabinFirstCharacter <- as.factor(substr(cb$Cabin, 1, 1))
str(cb$CabinFirstCharacter)

# CabinFirstCharacter vs Survived
ggplot(cb[1:891,], aes(x = CabinFirstCharacter, fill = Survived)) +
  geom_bar() +
  xlab("CabinFirstCharacter") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

# Dig more...
ggplot(cb[1:891,], aes(x = CabinFirstCharacter, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# What if plus Title?
ggplot(cb[1:891,], aes(x = CabinFirstCharacter, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass & Title") +
  xlab("CabinFirstCharacter") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# What about the multiple cabins?
cb$MultipleCabin <- as.factor(ifelse((nchar(cb$Cabin)>4),"Y","N"))
ggplot(cb[1:891,], aes(x = MultipleCabin, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Does Survived rate depend on the port of embarkation?
str(cb$Embarked)
sum(is.na(cb$Embarked))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot data for analysis
ggplot(cb[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass ) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Assign sibsp plus parch to "FamilySize" variable.
cb$FamilySize <- as.factor(cb$SibSp + cb$Parch + 1)

# Let us see the predict power of FamilSize
ggplot(cb[1:891,], aes(x = FamilySize, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Title +Pclass) + 
  ggtitle("Pclass, Title") +
  xlab("familysize") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extract Title from "Name" variable
cb$NewTitle<-gsub('(.*, )|(\\..*)', '', cb$Name)

# classify the titles
cb$NewTitle[cb$NewTitle %in% c("Ms", "Mlle")] <- "Miss"
cb$NewTitle[cb$NewTitle %in% c("Mme","Dona", "the Countess","Lady")] <- "Mrs"
cb$NewTitle[cb$NewTitle %in% c("Col", "Capt", "Major","Dr","Rev","Sir","Jonkheer", "Don")] <- "Mr"
cb$NewTitle <- as.factor(cb$NewTitle)

#  Visualize new version of title
ggplot(cb[1:891,], aes(x = NewTitle, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) + 
  ggtitle("by pclass")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(cb[cb$NewTitle=="Mr",])
cb[cb$NewTitle=="Mr" & cb$Sex=="female",]
cb$NewTitle[which(cb$NewTitle=="Mr" & cb$Sex=="female")]<-"Mrs"

# Make sure the "Newtitle" variable without any mistake.
length(which(cb$Sex == "female" & 
             (cb$NewTitle== "Master"|
              cb$NewTitle == "Mr")))
length(which(cb$Sex == "male" & 
             (cb$NewTitle== "Miss"|
              cb$NewTitle == "Mrs")))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# let's show some example about ticket with fare 
cb[cb$Ticket %in% c("PC 17755","PC 17611","17421"),c("Ticket","Fare")]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# let's show some example about ticket with fare 
 
head(cb[which(cb$Ticket == "PC 17755" |
                 cb$Ticket == "PC 17611" |
                 cb$Ticket == "113760"   | 
                 cb$Ticket=="17421"),],100)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Classify the ticket
TicketTypes<-as.data.frame(table(cb$Ticket))
names(TicketTypes)<-c("Ticket","TicketTypeSize")
cb<-merge(x=cb,y=TicketTypes,by.x="Ticket",sort=TRUE)[, union(names(cb), names(TicketTypes))]
cb<-cb[order(cb$PassengerId),]
rownames(cb)<-1:nrow(cb)
cb$AveFare<-cb$Fare/cb$TicketTypeSize
cb$AveFare<-as.numeric(cb$AveFare)

# List who lost fare value
cb[is.na(cb$AveFare), ]

# Get records for similar conditional passengers and get the median of AveFare
summary(cb$AveFare[which(cb$Pclass == "3" & cb$NewTitle == "Mr" & cb$FamilySize == 1 & cb$Ticket != "3701")])
cb[is.na(cb$AveFare), "AveFare"] <- 7.840


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#10-fold CV repeated 10 times
set.seed(2348)
cv.3.folds <- createMultiFolds(cb[1:891,"Survived"], k = 10, times = 10)
ctrl.3 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.3.folds)

# Run CV and check out results
set.seed(34324)
rpart.10cv.2<-train(x=cb[1:891,c("Pclass", "NewTitle", "FamilySize","TicketTypeSize","AveFare")],y=cb[1:891,"Survived"] ,method="rpart",tuneLength = 30, trControl= ctrl.3)
rpart.10cv.2
# Plot trees
prp(rpart.10cv.2$finalModel, type = 0, extra = 1, under = TRUE)
rpart.plot(rpart.10cv.2$finalModel)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Prediction <- predict(rpart.10cv.2, cb[892:1309,]) 
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "rpart_10CV_1.csv", row.names = FALSE)

