Train <- read.csv("../input/train.csv")
Test <- read.csv("../input/test.csv")

Train$TYPE <- 1
Test$TYPE <- 0

Survive <- as.data.frame(Train[,2])
colnames(Survive)[1] <- "Survived"

Train <- Train[-2]

Data <- rbind(Train,Test)

levels(Data$Embarked)[1] <- "S"
Data[Data$Embarked=='',"Embarked"] <- "S"

Data$Name <- as.character(Data$Name)

Data$Title <- gsub('(.*, )|(\\..*)', '', Data$Name)

officer <- c("Capt", "Col", "Don", "Dr", "Major", "Rev")
royalty <- c("Dona", "Lady", "the Countess","Sir", "Jonkheer")

Data$Title[Data$Title == "Mlle"]        <- "Miss" 
Data$Title[Data$Title == "Ms"]          <- "Miss"
Data$Title[Data$Title == "Mme"]         <- "Mrs" 
Data$Title[Data$Title %in% royalty]  <- "Royalty"
Data$Title[Data$Title %in% officer]  <- "Officer"

Data$Surname <- sapply(Data$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])

Data$FamilySize <- Data$SibSp + Data$Parch + 1

Data$Family <- paste(Data$Surname, Data$FamilySize, sep='_')

Data$FamilySizeD[Data$FamilySize == 1] <- "Singleton"
Data$FamilySizeD[Data$FamilySize < 5 & Data$FamilySize > 1] <- "Small"
Data$FamilySizeD[Data$FamilySize > 4] <- "Large"

library(mice)

F_Data_M <- mice(Data, m = 1)
Data_F <- complete(F_Data_M, 1, include = FALSE)

Data_F$Child[Data_F$Age < 18] <- "Child"
Data_F$Child[Data_F$Age >= 18] <- "Adult"

Data_F$Mother <- "Not Mother"
Data$Mother[Data$Sex == "female" & Data_F$Parch > 0 & Data$Age > 18 & Data$Title != "Miss"] <- "Mother"

Data_F$Child  <- factor(Data_F$Child)
Data_F$Title  <- factor(Data_F$Title)
Data_F$Pclass  <- factor(Data_F$Pclass)
Data_F$FamilySize  <- factor(Data_F$FamilySize)
Data_F$Mother <- factor(Data_F$Mother)

Data_F <- Data_F[,-c(8,10)]

S_Train <- subset(Data_F, TYPE== 1)
F_Test <- subset(Data_F, TYPE== 0)

F_Train <- cbind(S_Train,Survive)

library("randomForest")

set.seed(123)

MOD <- randomForest(factor(Survived) ~ Pclass + Sex + Fare + Embarked + Title + FamilySizeD + Child, data = F_Train)

PREDICTION <- predict(MOD, F_Test)

SOLUTION <- data.frame(PassengerId = F_Test$PassengerId, Survived = PREDICTION)

write.csv(SOLUTION, file = "RN_Submission.csv", row.names = F)