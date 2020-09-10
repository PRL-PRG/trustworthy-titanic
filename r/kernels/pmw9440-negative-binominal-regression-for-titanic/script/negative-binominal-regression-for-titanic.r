
###Importing the necesscary libraries.
library(ggplot2)
library(plyr)
library(MASS)

## Reading in files
list.files(path = "../input")
trainset <- read.csv("../input/train.csv")
testset <- read.csv("../input/test.csv")

## colnames 
print("The colnames of trainset")
colnames(trainset)
head(trainset)
print("There are 12 columns in trainset.\n")
print("____________________________________")

print("The colnames of testset")
colnames(testset)
head(testset)
print("It has the same same columns to trainset except for Survived")

##Clearning train data
#1. Survived(Nominal)
Hmisc::describe(trainset$Survived)
print("______________________________________________")

#2. Pclass(Ordinal)
Hmisc::describe(trainset$Pclass)
print("______________________________________________")

#3. Name(Nominal)
Hmisc::describe(trainset$Name)
print("______________________________________________")

#4. Sex(Nominal)
Hmisc::describe(trainset$Sex)
print("______________________________________________")

#5. Age(Discrete)
Hmisc::describe(trainset$Age)
ggplot(data = trainset , na.rm = TRUE) +
  geom_boxplot(color = "blue",aes(x= 1, y=Age), outlier.colour = "red", outlier.shape = 1)
print("Missing Value : 177")
print("______________________________________________")

#6. SibSp(Discrete)
Hmisc::describe(trainset$SibSp)
ggplot(data = trainset , na.rm = TRUE) +
  geom_boxplot(color = "blue",aes(x= 1, y=SibSp), outlier.colour = "red", outlier.shape = 1)
print("______________________________________________")

#7. Parch(Discrete)
Hmisc::describe(trainset$Parch)
ggplot(data = trainset , na.rm = TRUE) +
  geom_boxplot(color = "blue",aes(x= 1, y= Parch), outlier.colour = "red", outlier.shape = 1)
print("______________________________________________")

#8. Ticket(Nominal) (Useless)
Hmisc::describe(trainset$Ticket)
print("______________________________________________")

#9. Fare(Continous)
Hmisc::describe(trainset$Fare)
ggplot(data = trainset , na.rm = TRUE) +
  geom_boxplot(color = "blue",aes(x= 1, y= Fare), outlier.colour = "red", outlier.shape = 1)
print("______________________________________________")

#10. Cabin(Nominal)
Hmisc::describe(trainset$Cabin)
print("______________________________________________")

#11. Embarked(Nominal)
Hmisc::describe(trainset$Embarked)
print("______________________________________________")


### Complete the data
#Age
trainset[is.na(trainset$Age), 6] = median(trainset$Age, na.rm = TRUE)

##convert the Survived column as facotr variable
trainset$Survived <- as.factor(trainset$Survived)

###EDA(Explanatory Data analysis)
##distribution of variables(charastics of people who board the Titanic)

##convert the Survived column as facotr variable
trainset$Survived <- as.factor(trainset$Survived)

#1. Pclass
ggplot(data = trainset, aes(x =Pclass, fill = Survived))+
  geom_histogram(stat = "count" )

#2. Sex
ggplot(data = trainset, aes(x = Sex, fill = Survived))+
  geom_histogram(stat = "count")

#3. Age
ggplot(data = trainset, aes(x =Age, fill = Survived))+
  geom_density(binwidth =5, na.rm = TRUE, alpha = 0.3)

#4. SibSp
ggplot(data = trainset, aes(x =SibSp, fill = Survived))+
  geom_density(binwidth =5, na.rm = TRUE, alpha = 0.3)

#5. Parch
ggplot(data = trainset, aes(x = Parch, fill = Survived))+
  geom_density(binwidth =5, na.rm = TRUE, alpha = 0.3)

#6. Fare
ggplot(data = trainset, aes(x = Fare, fill = Survived))+
  geom_density(binwidth =5, na.rm = TRUE, alpha = 0.3)

#7. Embarked
ggplot(data = trainset, aes(x = Embarked, fill = Survived))+
  geom_histogram(stat = "count")


##Select Inputs and turn the vairiables into count data
#Kids (over 10 years old and under 10 years old)
trainset[trainset$Age <= 10, "Kids"] <- 1
trainset[trainset$Age > 10, "Kids"] <- 0

##converting data to count data
#trainset -> traincount
trainset$one <- 1
trainset$Survived <- as.numeric(as.character(trainset$Survived))
trainset$Survived <- as.numeric(trainset$Survived)
GroupColumns <- c("Pclass", "Sex", "Embarked", "Kids")
DataColumns <- c("Survived" ,"one")
traincounts <- ddply(trainset, GroupColumns, function(x) colSums(x[DataColumns]))
colnames(traincounts)[6] <- "Total"
traincounts$SurvivalRate <- traincounts$Survived / traincounts$Total
                                        
###Building the Negative Binorminal Regression
Nb_model <- glm.nb(formula =  SurvivalRate ~ Pclass + Sex + Embarked  +  Kids, data = traincounts)
Nb_model
confint(Nb_model)
