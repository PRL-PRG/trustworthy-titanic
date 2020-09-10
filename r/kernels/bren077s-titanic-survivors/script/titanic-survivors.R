
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library('dplyr')
#library(ggplot2)
library(rpart)
library(party)
#library(MASS)
#library(neuralnet)

#set.seed(111)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
shuffle <- train[sample(nrow(train)),]
test  <- read.csv("../input/test.csv")

all_data <- bind_rows(shuffle, test)
all_data$Embarked[c(62, 830)] <- "C"
all_data$Embarked <- factor(all_data$Embarked)
all_data$Fare[1044] <- median(all_data[all_data$Pclass == '3' & all_data$Embarked == 'S', ]$Fare, na.rm = TRUE)
all_data$Title <- gsub(".*\\ (.*)\\..*", "\\1", all_data$Name)
if(FALSE){
all_data$Title[all_data$Title == 'L'] <- 'Other'
all_data$Title[all_data$Title == 'Capt'] <- 'Other'
all_data$Title[all_data$Title == 'Countess'] <- 'Other'
all_data$Title[all_data$Title == 'Don'] <- 'Other'
all_data$Title[all_data$Title == 'Dona'] <- 'Other'
all_data$Title[all_data$Title == 'Mme'] <- 'Other'
all_data$Title[all_data$Title == 'Major'] <- 'Other'
all_data$Title[all_data$Title == 'Jonkheer'] <- 'Other'

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
all_data$Title[all_data$Title == 'Mlle']        <- 'Miss' 
all_data$Title[all_data$Title == 'Ms']          <- 'Miss'
all_data$Title[all_data$Title == 'Mme']         <- 'Mrs' 
all_data$Title[all_data$Title %in% rare_title]  <- 'Rare Title'
}
all_data$Title <- factor(all_data$Title)
all_data$Family_size <- all_data$Parch + all_data$SibSp + 1
all_data$FarePerPerson <- all_data$Fare / all_data$Family_size
all_data$Surname <- sapply(all_data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
all_data$FamilyID <- paste(as.character(all_data$Family_size), all_data$Surname, sep="")
all_data$FamilyID[all_data$Family_size < 2] <- 'Small'
famIDs <- data.frame(table(all_data$FamilyID))
famIDs <- famIDs[famIDs$Freq < 2,]
all_data$FamilyID[all_data$FamilyID %in% famIDs$Var1] <- 'Small'
length(unique(all_data$FamilyID))
all_data$FamilyID <- factor(all_data$FamilyID)

predicted_age <- cforest(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + Family_size + FarePerPerson + FamilyID,
                       data = all_data[!is.na(all_data$Age),],
                controls=cforest_unbiased(ntree=200, mtry=3))
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),], OOB=TRUE, type = "response")
all_data$Child[all_data$Age < 18] <- 'Child'
all_data$Child[all_data$Age >= 18] <- 'Adult'
all_data$Child <- factor(all_data$Child)
all_data$Mother <- 'Not Mother'
all_data$Mother[all_data$Sex == 'female' & all_data$Parch > 0 & all_data$Age > 18 & all_data$Title != 'Miss'] <- 'Mother'
all_data$Mother <- factor(all_data$Mother)
all_data$Deck<-substring(all_data$Cabin, 1, 1)
all_data$Deck <- factor(all_data$Deck)
#table(all_data$Deck)
predicted_deck <- cforest(Deck ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Child + Mother + Family_size + FarePerPerson + FamilyID,
                       data = all_data[all_data$Deck != '',],
                controls=cforest_unbiased(ntree=200, mtry=3))

all_data$Deck[all_data$Deck == ''] <- predict(predicted_deck, all_data[all_data$Deck == '',], OOB=TRUE, type = "response")
str(all_data)
train <- all_data[1:891,]
test <- all_data[892:1309,]
#my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Deck + Child + Mother + Family_size, data=train, importance = TRUE, ntree=2000)

my_forest <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title 
                + Deck + Child + Mother + Family_size + FarePerPerson + FamilyID, data = train,
                controls=cforest_unbiased(ntree=500, mtry=3))

if(FALSE){
    clusterdata <- all_data[, c('Pclass', 'Age', 'SibSp', 'Parch', 'Fare')]
    clusterdata$Sex <- as.numeric(all_data$Sex)
    #wss <- (nrow(clusterdata)-1)*sum(apply(clusterdata,2,var))
    #  for (i in 2:15) wss[i] <- sum(kmeans(clusterdata, centers=i)$withinss)
    #plot(1:15, wss, type="b", xlab="Number of Clusters",
    #     ylab="Within groups sum of squares")
    error <- vector()
    error[1] <- my_forest$err.rate[2000, c('OOB')]
    for(i in 2:15){
        cluster <- kmeans(clusterdata, i)
        all_data$Cluster <- as.factor(cluster$cluster)
        
        train <- all_data[1:891,]
        test <- all_data[892:1309,]
        
        my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Deck + Child + Mother + Cluster + Family_size, data=train, importance = TRUE, ntree=2000)
        error[i] <- my_forest$err.rate[2000, c('OOB')]
    }
    plot(error, type="b")
    cluster <- kmeans(clusterdata, 8)
    all_data$Cluster <- as.factor(cluster$cluster)
    
    train <- all_data[1:891,]
    test <- all_data[892:1309,]
    
    my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Deck + Child + Mother + Cluster + Family_size, data=train, importance = TRUE, ntree=2000)
}
#my_forest
#plot(my_forest)
#varImpPlot(my_forest)
#varimp(my_forest)
# Make your prediction using the test set
#my_prediction <- predict(my_forest, test, "class")

my_prediction <- predict(my_forest, test, OOB=TRUE, type = "response")
# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test["PassengerId"], Survived = my_prediction)

write.csv(my_solution, file="my_solution.csv",  row.names = FALSE)