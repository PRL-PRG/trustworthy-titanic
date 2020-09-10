
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

list.files("../input")

# Any results you write to the current directory are saved as output.

train_data <- read.csv('../input/train.csv')
test_data <- read.csv('../input/test.csv')

dim(train_data)
dim(test_data)

train_data$Cabin <- NULL
test_data$Cabin <- NULL
train_data$Ticket <- NULL
test_data$Ticket <- NULL

summary(train_data)
summary(test_data)

train_data$Title <- gsub('(.*, )|(\\..*)', '', train_data$Name)
test_data$Title <- gsub('(.*, )|(\\..*)', '', test_data$Name)

table(train_data$Sex, train_data$Title)
table(test_data$Sex, test_data$Title)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
train_data$Title[train_data$Title == 'Mlle']        <- 'Miss' 
train_data$Title[train_data$Title == 'Ms']          <- 'Miss'
train_data$Title[train_data$Title == 'Mme']         <- 'Mrs' 
train_data$Title[train_data$Title %in% rare_title]  <- 'Rare Title'
test_data$Title[test_data$Title == 'Mlle']        <- 'Miss' 
test_data$Title[test_data$Title == 'Ms']          <- 'Miss'
test_data$Title[test_data$Title == 'Mme']         <- 'Mrs' 
test_data$Title[test_data$Title %in% rare_title]  <- 'Rare Title'

summary(train_data)

train_data

train_data$Name <- NULL
test_data$Name <- NULL

summary(train_data)
summary(test_data)

title_list <- c('Mr', 'Miss', 'Mrs', 'Master', 'Rare Title')

print(title_list)

for (i in title_list){
    print(i)
    
}
print(i)
remove(i)
print(title_list)

print(train_data[which(is.na(train_data$Age) & train_data$Title=='Mr'),])

 master_train <- mean(train_data$Age[train_data$Title=="Master"], na.rm = TRUE)
 mr_train <- mean(train_data$Age[train_data$Title=="Mr"], na.rm = TRUE)
 mrs_train <- mean(train_data$Age[train_data$Title=="Mrs"], na.rm = TRUE)
 miss_train <- mean(train_data$Age[train_data$Title=="Miss"], na.rm = TRUE)
rare_train <- mean(train_data$Age[train_data$Title=="Rare Title"], na.rm = TRUE)
 master_test <- mean(test_data$Age[test_data$Title=="Master"], na.rm = TRUE)
 mr_test <- mean(test_data$Age[test_data$Title=="Mr"], na.rm = TRUE)
 mrs_test <- mean(test_data$Age[test_data$Title=="Mrs"], na.rm = TRUE)
 miss_test <- mean(test_data$Age[test_data$Title=="Miss"], na.rm = TRUE)
rare_test <- mean(test_data$Age[test_data$Title=="Rare Title"], na.rm = TRUE)

print(master_train)

train_data$Age[which(is.na(train_data$Age) & train_data$Title=='Mr')] <- mr_train

train_data$Age

train_data$Age[which(is.na(train_data$Age) & train_data$Title=='Mrs')] <- mrs_train
train_data$Age[which(is.na(train_data$Age) & train_data$Title=='Miss')] <- miss_train
train_data$Age[which(is.na(train_data$Age) & train_data$Title=='Master')] <- master_train
train_data$Age[which(is.na(train_data$Age) & train_data$Title=='Rare Title')] <- rare_train

test_data$Age[which(is.na(test_data$Age) & test_data$Title=='Mr')] <- ((mr_train+mr_test)/2)
test_data$Age[which(is.na(test_data$Age) & test_data$Title=='Mrs')] <- ((mrs_train+mrs_test)/2)
test_data$Age[which(is.na(test_data$Age) & test_data$Title=='Miss')] <- ((miss_train+miss_test)/2)
test_data$Age[which(is.na(test_data$Age) & test_data$Title=='Master')] <- ((master_train+master_test)/2)
test_data$Age[which(is.na(test_data$Age) & test_data$Title=='Rare Title')] <- ((rare_train+rare_test)/2)

summary(train_data)
summary(test_data)

which(train_data$Embarked=='')

train_data$Embarked[c(62, 830)] <- 'C'

train_data$PassengerId <- NULL
test_data$PassengerId <- NULL

log_fit <- glm(Survived ~ Pclass + Fare + Parch +SibSp + Title + Embarked + Sex + Age, data = train_data, family = binomial)

summary(log_fit)

prediction = predict(log_fit,test_data,type="response")

prediction

prediction_round.5 <- ifelse(prediction >0.6,1,0)

test <- read.csv('../input/test.csv')

solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction_round.5)

print(solution)

write.csv(solution, file ='/log_solution.csv', row.names = F)


