
# Load library related
library('ggplot2') #visualization
library('ggthemes') #visualization
library('scales') #visualization
library('dplyr') #data manipulation
library('mice') #imputation
library('randomForest') #classification algorithm : Random Forest
library("e1071") #classification algorithm : Support Vector Machine (SVM)
library(class) #classification algorithm : KNN
library(C50) #classification algorithm : C5.0 Binary Tree
library(rpart) #Classification algorithm : Rpart Binary Tree

# Read dataset
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
full <- bind_rows(train,test)
head(full)

# See possible feature might has correlation with Survival Rate
ggplot(full, aes(x= factor(Sex), fill= factor(Survived))) + geom_bar(position = 'dodge');
ggplot(full, aes(x= factor(Parch), fill= factor(Survived))) + geom_bar(position = 'dodge', stat = 'count');
ggplot(full, aes(x= factor(SibSp), fill= factor(Survived))) + geom_bar();
ggplot(full, aes(x= (Embarked), fill = factor(Survived))) + geom_bar(position = 'dodge');

# Check if there is incomplete value in dataset

sum(is.na(full$Pclass)); #Complete data
sum(is.na(full$Name)); #Complete data
sum(is.na(full$Sex)); #Complete data
sum(is.na(full$Age)); #Incomplete data -> there are 263 missing data
sum(is.na(full$SibSp)); #Complete data
sum(is.na(full$Parch)); #Complete data
sum(is.na(full$Ticket)); #Complete data
sum(is.na(full$Fare)); #Incomplete data -> there is 1 missing data
sum(is.na(full$Cabin)); #Complete data
sum(is.na(full$Embarked)); #Complete data

full$ParchCat[full$Parch == 0] <- 0
full$ParchCat[full$Parch == 1] <- 1
full$ParchCat[full$Parch == 2] <- 2
full$ParchCat[full$Parch >2 ] <- 3

ggplot(data = full, aes(x = ParchCat , fill = factor(Survived))) + geom_bar(position = 'dodge')

full$SibSpCat[full$SibSp == 0] <- 0
full$SibSpCat[full$SibSp == 1] <- 1
full$SibSpCat[full$SibSp == 2] <- 2
full$SibSpCat[full$SibSp == 3] <- 3
full$SibSpCat[full$SibSp == 4] <- 4
full$SibSpCat[full$SibSp > 4] <- 5

ggplot(full, aes(x = factor(SibSpCat) , fill = factor(Survived))) + geom_bar()

str(full$Name)
sample(full$Name, size = 5)

# Title might give additional information of gender and marital status
# Title is basically between first comma(,) and first dot (.)
# I will extract Title using Regex gsub()

full$Title <- gsub('(.*, )|(\\..*)','', full$Name)
table(full$Title)

# We have majority Title i.e Mr, Mrs, Miss, Master
# And also we have smaller group which is rare_title
# We will separate the big group, and predict what the rare_title is actually belong to

rare_title <- c('Capt', 'Col', 'Don', 
                'Dona', 'Dr', 'Jonkheer', 
                'Lady', 'Major', 'Rev', 'Sir', 
                'the Countess', 'Mlle', 'Mme', 'Ms')


full$TitleCat[full$Title == "Mr"] <- 1
full$TitleCat[full$Title == "Miss"] <- 2
full$TitleCat[full$Title == "Master"] <- 3
full$TitleCat[full$Title == "Mrs"] <- 4
full$TitleCat[full$Title %in% rare_title] <- 0 # We will group this into 1/2/3/4

table(full$TitleCat)

# Based on available information, we might predict Title in group 0
# Rational metrics are Sex, number of parents (Parch), and number of sibling (Sibsp)

# Temporal dataset
temp_train <- full[full$TitleCat != 0, c('PassengerId','Pclass','Sex','SibSpCat','TitleCat', 'ParchCat')]
temp_test <- full[full$TitleCat == 0, c('PassengerId','Pclass','Sex','SibSpCat','TitleCat', 'ParchCat')]

# Title prediction
temp_model <- randomForest(factor(TitleCat) ~ Sex + ParchCat + SibSpCat, data = temp_train)

# Prediction result
temp_result <- predict(temp_model,temp_test)
temp_test$TitleCat <- temp_result

# Replace Title Cat 0 with prediction result
full[full$TitleCat == 0,]$TitleCat <- temp_test$TitleCat
# See the result of this prediction
table(full$TitleCat)
# Now we have 4 gruops of Title which represent Gender and Marital Status!

# to see data that have no Fare
full[full$PassengerId[is.na(full$Fare)],]

# Based on this, the passanger is from Pclass = 3 and Embarkment = S
# Fare on the same class and embarkment might have the same range of range
# So we will use median value of fare in Pclass = 3 and Embarkment = S
full$Fare[1044] <- median(full[full$Pclass == 3 & full$Embarked == 'S',]$Fare , na.rm = TRUE)

sum(is.na(full$Fare)) # Now we dont have null value

table(full$Embarked)
# Turn out we have two missing data in Embarked. Let see the correlation between information we have about these two.

full[full$Embarked == "",]
#They both have the same ticket "1135712", fare 80 and Cabin B28, Pclass 1
#Let see the likely port of the majority passenger who have the same treat

ggplot(full, aes(x = factor(Embarked), y = Fare)) + geom_boxplot()
# Based on Fare boxplot, their fare are likely belong to C embarkment. (See boxplot!)

# Fill Embarked
full$Embarked[62] <- "C"
full$Embarked[830] <- "C"

table(full$Embarked) # Now we don't have null value in Embarked


str(full$Cabin)
head(full$Cabin)
sample(table(full$Cabin), size = 7)

# As we see,
# Cabin contain variative value, some of them are:
# 1. Initially with first character A, B, C, D, etc
# 2. Space character ""

# We will categorized this based on first character of cabin
sum(full$Cabin == "") # The dataset has 1014 Cabin which has "" value

#Get first character of Cabin and save it to CabinF
full$CabinF <- substring(full$Cabin,1,1)
table(full$CabinF)

# Distribution survivor in Cabin Category
ggplot(full, aes(x = factor(CabinF), fill = factor(Survived))) + geom_bar(position = 'dodge')

#Make smaller category in Cabin
full$CabinCat[full$CabinF == "A"] <- "A"
full$CabinCat[full$CabinF == "B"] <- "B"
full$CabinCat[full$CabinF == "C"] <- "C"
full$CabinCat[full$CabinF == "D"] <- "D"
full$CabinCat[full$CabinF == "E"] <- "E"
full$CabinCat[full$CabinF == "F"] <- "F"

full$CabinCat[full$CabinF == "G"] <- "Y" # Will categorize this cabin to category Y
full$CabinCat[full$CabinF == "T"] <- "Y" # Will categorize this cabin to category Y
full$CabinCat[full$CabinF == ""] <- "Y" # Will categorize this cabin to category Y

table(full$CabinCat)


# Check if Age is null
sum(is.na(full$Age))

# Prepare to complete these 263 missing data
# We will use second level prediciton to predict Age based on several feature we already have
# First we should pick the significant variable to define Age :
# Based on the writer code, we will use : Pclass, SibSp, CabinCat, and TitleCat as the most correlated

# Temporal dataset train and test
temp_train <- full[!(is.na(full$Age)), c('PassengerId','Pclass','Age','SibSp','CabinCat','TitleCat')]
temp_test <- full[is.na(full$Age), c('PassengerId','Pclass','Age','SibSp','CabinCat','TitleCat')]

# Age Prediction
temp_model <- rpart(Age ~ Pclass + SibSp + CabinCat + TitleCat, data = temp_train)

# Prediction result
temp_result <- predict(temp_model, temp_test)

# Use the result
temp_test$Age <- temp_result
full[is.na(full$Age),]$Age <- temp_test$Age

# Check if there still null value in Age
sum(is.na(full$Age))

# Yeah we finally completed the missing Age!

# It is time to dig deeper in Age.
# I want to see how many data which have particular value in this data

head(table(full$Age, full$Survived))
# It turns out we have too many values of Age within.
#I want to reduce categorical value of Age here. Were is the point I could separate the range of this Age.

# Distribution of Age and Survival
ggplot(full, aes(x = Age)) + geom_bar(position = 'dodge')

# After seeing distribution of Age, we could split it into two groups which is young and old (17 years)
full$AgeCat[full$Age <= 17] <- 'young'
full$AgeCat[full$Age > 17] <- 'old'
full$AgeCat <- as.factor(full$AgeCat)

# New Distribution using AgeCat and Survival
ggplot(full, aes(x = AgeCat, fill = Survived)) + geom_bar(position = 'dodge')



# First see what is the composition of this field
str(full$Ticket)
summary(full$Ticket)
sample(table(full$Ticket), size = 7)

# Based on this, we know that one ticket may belong to many passenger which means the ticket is for
# family use. We will categorize this ticket to Single or notSingle ticket.

full[full$Ticket == "2668",]
sum(full$Ticket == "2668")

#full$numTicket <- sum(as.factor(full$Ticket), na.rm = TRUE)

#Single Ticket
# full$TicketCat[count((as.factor(full$Ticket)), na.rm = TRUE) == 1] <- 'Single'

#Family Ticket
# full$TicketCat[count((as.factor(full$Ticket)), na.rm = TRUE) > 1] <- 'Family'

# table(full$TicketCat)


# Generate family size
full$FSize <- full$Parch + full$SibSp + 1

# See the relation to Survival rate
ggplot(full[1:891,], aes(x = as.factor(FSize) , fill = factor(Survived))) + geom_bar(position = 'dodge')

# Categorize family size into 4 groups i.e. single, small, large, and huge
full$FsizeCat[full$FSize == 1] <- 'single'
full$FsizeCat[full$FSize == 2] <- 'small'
full$FsizeCat[full$FSize == 3] <- 'small'
full$FsizeCat[full$FSize == 4] <- 'small'
full$FsizeCat[full$FSize > 4 & full$FSize < 8] <- 'large'
full$FsizeCat[full$FSize > 7] <- 'huge'

# See the relation to Survival rate
ggplot(full[1:891,], aes(x = as.factor(FsizeCat) , fill = factor(Survived))) + geom_bar(position = 'dodge')


#First make sure Embarked, CabinCat, FsizeCat in factor type
full$Embarked <- as.factor(full$Embarked)
full$CabinCat <- as.factor(full$CabinCat)
full$FsizeCat <- as.factor(full$FsizeCat)

# Segregate two dataset (Test & Train)
train <- full[1:891, ]
test <- full[892:1309, ]

#Random Forest Model
rf_model <- randomForest(factor(Survived)  ~ Pclass + Age + Fare + Embarked + ParchCat 
                         + SibSpCat + TitleCat + CabinCat + FsizeCat, data = train )

#Binary Tree Model
bt_model <- rpart(factor(Survived) ~ Pclass + Sex + Age + Fare + Embarked + ParchCat 
                  + SibSpCat + TitleCat + CabinCat + FsizeCat, data = train )

#SVM
svm_model <- svm(factor(Survived) ~ Pclass + Sex + Age + Fare + Embarked + ParchCat 
                 + SibSpCat + TitleCat + CabinCat + FsizeCat, data = train)

#Result Prediction

#Predict Random Forest Model
rf_result <- predict(rf_model, test)

#Binary Tree Model
bt_result <- predict(bt_model, test)

#SVM
svm_result <- predict(svm_model, test[,-2]) #Exclude the field "Survived"

#Solution Random Forest Model
rf_solution <- data.frame(PassengerId = test$PassengerId, Survived = rf_result)

#Solution Binary Tree Model
bt_solution <- data.frame(PassengerId = test$PassengerId, Survived = round(bt_result[,2], digits = 0))

#Solution SVM Model
svm_solution <- data.frame(PassengerId = test$PassengerId, Survived = svm_result)

# WRITE SOLUTION
#Write the RF_Solution
write.csv(rf_solution, file = "rf_solution.csv", row.names = F)

#Write the Binary Tree Solution
write.csv(bt_solution, file = "bt_solution.csv", row.names = F)

#Write the Binary Tree Solution
write.csv(svm_solution, file = "svm_solution.csv", row.names = F)
