library("rpart")
library("rpart.plot")
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
test$Survived <- 0
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
strsplit(combi$Name[1], split = "[,.]")
strsplit(combi$Name[1], split = "[,.]")[[1]]
strsplit(combi$Name[1], split = "[,.]")[[1]][2]
combi$Title <- sapply(combi$Name, FUN = function(x) {
    strsplit(x, split = "[,.]")[[1]][2]
})
combi$Title <- sub(" ", "", combi$Title)
combi$Title[combi$PassengerId == 797] <- "Mrs"
combi$Title[combi$Title %in% c("Lady", "the Countess", "Mlle", "Mee", "Ms")] <- "Miss"
combi$Title[combi$Title %in% c("Capt", "Don", "Major", "Sir", "Col", "Jonkheer", "Rev", "Dr", "Master")] <- "Mr"
combi$Title[combi$Title %in% c("Dona")] <- "Mrs"
combi$Title <- factor(combi$Title)
combi$Embarked[c(62, 830)] = "S"
combi$Embarked <- factor(combi$Embarked)
combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)
combi$family_size <- combi$SibSp + combi$Parch + 1
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size, data = combi[!is.na(combi$Age), ], method = "anova")
combi$Age[is.na(combi$Age)] <- predict(predicted_age, combi[is.na(combi$Age), ])
train_new <- combi[1:891, ]
test_new <- combi[892:1309, ]
test_new$Survived <- NULL
train_new$Cabin <- substr(train_new$Cabin, 1, 1)
train_new$Cabin[train_new$Cabin == ""] <- "H"
train_new$Cabin[train_new$Cabin == "T"] <- "H"
test_new$Cabin <- substr(test_new$Cabin, 1, 1)
test_new$Cabin[test_new$Cabin == ""] <- "H"
train_new$Cabin <- factor(train_new$Cabin)
test_new$Cabin <- factor(test_new$Cabin)
str(train_new)
str(test_new)
my_tree <- rpart(Survived ~ Age + Sex + Pclass + family_size, data = train_new, method = "class", control = rpart.control(cp = 1e-04))
summary(my_tree)
prp(my_tree, type = 4, extra = 100)
my_prediction <- predict(my_tree, test_new, type = "class")
head(my_prediction)
vector_passengerid <- test_new$PassengerId
my_solution <- data.frame(PassengerId = vector_passengerid, Survived = my_prediction)
head(my_solution)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
