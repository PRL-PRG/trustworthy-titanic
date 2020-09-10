# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

# The train and test data is stored in the ../input directory
set.seed(1)
train <- read.csv('train.csv', stringsAsFactors=FALSE)
test  <- read.csv('test.csv',  stringsAsFactors=FALSE)

# We can inspect the train data. The results of this are printed in the log tab below
summary(train)
# Here we will plot the passenger survival by class
train$Survived <- factor(train$Survived, levels=c(0, 1))
levels(train$Survived) <- c("Died", "Survived")
train$Pclass <- as.factor(train$Pclass)
levels(train$Pclass) <- c("1", "2", "3")

train$Sex <- factor(train$Sex)
levels(train$Sex) <- c("Female","Male")

train$AgeGroup <- ifelse(train$Age < 15, "0-15",
                         ifelse(train$Age >= 15 & train$Age < 29, "15-29",
                         ifelse(train$Age >= 29 & train$Age < 45, "29-45","45+")))

train$FamilySize <- train$SibSp + train$Parch + 1

#Family Size
g <- ggplot(train, aes(FamilySize, fill = Survived)) +geom_bar()
print(g)

# Embarked
h <- ggplot(train, aes(Embarked, fill=Survived)) + geom_bar()
print(h)

# Class vs Fare
i <- ggplot(train, aes(Pclass, Fare, fill=Pclass)) + geom_boxplot()
print(i)

#Fare vs Survival
train$FareBracket <- ifelse(train$Fare <8, "1. 0-7",
                            ifelse(train$Fare >=8 & train$Fare < 14, "2. 8-14",
                            ifelse(train$Fare >=14 & train$Fare <31, "3. 15-31", "4. 31+")))
j <-ggplot(train, aes(FareBracket, fill=Survived)) + geom_bar()
print(j)


#par(mfrow = c(1,2))
#png("1_survival_by_class.png", width=800, height=600)
#mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
#               color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
#               off=c(0), cex.axis=1.4)

#mosaicplot(train$AgeGroup ~ train$Survived, main="Passenger Survival by Age group",
#                      color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
#                      off=c(0), cex.axis=1.4, na.rm=TRUE)

#mosaicplot(train$AgeGroup ~ train$Pclass, main="Passenger Survival by class",
#           color=c("red", "blue","yellow"), shade=FALSE,  xlab="", ylab="",
#           off=c(0), cex.axis=1.4, na.rm=TRUE)


#library(grid)
#pushViewport(viewport(layout = grid.layout(1, 2)))
#print(myplot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
#print(myplot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))


#dev.off()

extractFeatures <- function(data) {
  features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked",
                "AgeGroup",
                "FamilySize",
                "FareBracket")
  fea <- data[,features]
  fea$Age[is.na(fea$Age)] <- -1
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
  fea$Embarked[fea$Embarked==""] = "S"
  fea$FamilySize[is.na(fea$FamilySize)] = 0
  fea$AgeGroup[is.na(fea$AgeGroup)] <-  "15-29"
  fea$Sex      <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  fea$AgeGroup <- as.factor(fea$AgeGroup)
  fea$FareBracket <- as.factor(fea$FareBracket)
  return(fea)
}

rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree=1000, importance=TRUE)
plot(rf, ylim=c(0,0.50))
legend('topright', colnames(rf$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(rf)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
 mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 3, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + theme_minimal()


