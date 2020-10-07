library(rpart)
library(dplyr)
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
full <- bind_rows(train, test)
tree_model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
full$Title <- gsub("(.*, )|(\\..*)", "", full$Name)
table(full$Sex, full$Title)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
full$Title[full$Title == "Mlle"] <- "Miss"
full$Title[full$Title == "Ms"] <- "Miss"
full$Title[full$Title == "Mme"] <- "Mrs"
full$Title[full$Title %in% rare_title] <- "Rare Title"
table(full$Sex, full$Title)
full01 <- full
full01$Sex <- as.factor(full01$Sex)
full01$Embarked <- as.factor(full01$Embarked)
full01$Title <- as.factor(full01$Title)
train_new <- full01[full01$PassengerId %in% train$PassengerId, ]
str(train_new)
test_new <- full01[full01$PassengerId %in% test$PassengerId, ]
str(test_new)
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = train_new, method = "class")
my_prediction <- predict(tree_model, newdata = test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
nrow(my_solution)
ncol(my_solution)
write.csv(my_solution, file = "tree_basic_Solution.csv", row.names = FALSE)
my_prediction <- predict(my_tree_five, test_new, type = "class")
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "tree_title_Solution.csv", row.names = FALSE)
