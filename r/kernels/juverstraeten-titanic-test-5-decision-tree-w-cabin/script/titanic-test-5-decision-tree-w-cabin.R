# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

# The titanic and test data is stored in the ../input directory
titanic <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the titanic data. The results of this are printed in the log tab below
str(titanic)

# Add variable Survived with NA Values to test
test$Survived <- NA

# Combine the 2 datasets
all_data <- rbind(titanic,test)

# Convert Name to a character variable and take out the interesting data
all_data$Name <- as.character(all_data$Name)

# First: the title
all_data$Title <- sapply(all_data$Name,FUN=function(x){strsplit(x,split="[,.]")[[1]][2]})
all_data$Title <- sub(" ","",all_data$Title)
all_data$Title[all_data$Title %in% c('Capt', 'Col', 'Don', 'Dr', 'Jonkheer', 'Major', 
                                    'Master', 'Rev', 'Sir')] <- 'Honorific_male'
all_data$Title[all_data$Title %in% c('Dona', 'Lady', 'the Countess')]  <- 'Honorific_female'
all_data$Title[all_data$Title %in% c('Miss', 'Mlle', 'Mme', 'Mrs', 'Ms')] <- 'Non_Honorific_female'
all_data$Title[all_data$Title %in% c('Mr')] <- 'Non_Honorific_male'
all_data$Title <- factor(all_data$Title)

# New variable for Cabin Group
all_data$CabinGroup <- sapply(all_data$Cabin, FUN= function(x){substr(x,1,1)})
all_data$CabinGroup[all_data$CabinGroup==''] <- 'no_data'
all_data$CabinGroup <- factor(all_data$CabinGroup)

titanic <- all_data[1:891,]
test <- all_data[892:1309,]

prop.table(table(titanic$CabinGroup, titanic$Survived),1)

tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + CabinGroup, 
            data=titanic, method = "class", control=rpart.control(minsplit = 5))

#plot(tree)
#text(tree)

# Visualisation of the tree
png("1_Visualisation_Tree_Titanic.png")
prp(tree)
dev.off()

# Use this tree to predict the outcome of the test data
my_prediction <- predict(tree, test, type="class")

# Create the csv file for submission
submission <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(submission, file="titanic_submission.csv", row.names=FALSE)