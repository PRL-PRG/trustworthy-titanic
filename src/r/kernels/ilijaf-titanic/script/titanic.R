library("mice")
library("dplyr")
train = read.csv("../input/train.csv")
test = read.csv("../input/test.csv")
full <- bind_rows(train, test)
factor_vars <- c("PassengerId", "Pclass", "Sex")
full$Title <- gsub("(.*, )|(\\..*)", "", full$Name)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
full$Title[full$Title == "Mlle"] <- "Miss"
full$Title[full$Title == "Ms"] <- "Miss"
full$Title[full$Title == "Mme"] <- "Mrs"
full$Title[full$Title %in% rare_title] <- "Rare Title"
table(full$Sex, full$Title)
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod <- mice(full[, !names(full) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Title", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
full$Embarked[c(62, 830)] <- "C"
full$Age <- mice_output$Age
train <- full[1:891, ]
test <- full[892:1309, ]
glm.fit = glm(Survived ~ Age + Title + Pclass + SibSp, data = train, family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type = "response")
glm.pred = ifelse(glm.probs > 0.5, "Survived", "Dead")
attach(train)
(475 + 243)/(467 + 232 + 82 + 110)
table(glm.pred, Survived)
mean(glm.pred == Survived)
prediction <- predict(glm.fit, test)
prediction[1:11]
solution <- data.frame(PassengerID = test$PassengerId, Survived = ifelse(prediction > 0.5, 1, 0))
write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
