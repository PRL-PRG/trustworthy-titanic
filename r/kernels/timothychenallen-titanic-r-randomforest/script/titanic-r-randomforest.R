require(dplyr)
require(caret)
require(mice)
require(ggplot2)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
full <- bind_rows(train, test)
sapply(names(full), function(x) sum(is.na(full[[x]])))
sapply(names(full), function(x) sum(full[[x]] == ""))
full[is.na(full$Age), ]$Age <- -1
full %>% filter(Embarked == "") %>% select(Embarked, Fare)
ggplot(full %>% filter(Pclass == 1, Embarked != "")) + geom_boxplot(aes(Embarked, Fare, fill = Embarked)) + geom_hline(yintercept = 80, color = "red")
full[full$Embarked == "", ]$Embarked <- "C"
full %>% filter(is.na(Fare)) %>% select(Embarked, Pclass, Sex)
full %>% filter(Embarked == "S", Pclass == 3, Sex == "male") %>% select(Fare) %>% summarize(usual_fare = median(Fare, na.rm = TRUE))
full[is.na(full$Fare), ]$Fare <- 8.05
full <- full %>% mutate(Title = gsub(x = Name, pattern = "^[^,]+, ([^ .]+)[\\. ].*$", replacement = "\\1")) %>% mutate(Surname = gsub(x = full$Name, pattern = "^([^,]+),.*$", replacement = "\\1")) %>% mutate(Family_size = SibSp + Parch)
full[grep(x = full$Title, pattern = "Mr|Miss|Mrs|Master", invert = TRUE), ]$Title <- "Unusual"
full$Sex <- factor(full$Sex)
full$Embarked <- factor(full$Embarked)
full$Title <- factor(full$Title)
train.clean <- full[1:891, ]
test.clean <- full[892:1309, ]
set.seed(42)
require(randomForest)
rf <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Family_size, data = train.clean)
plot(rf)
imp <- importance(rf)
imp
test.clean$Survived <- predict(rf, test.clean)
submission <- test.clean %>% select(PassengerId, Survived)
write.csv(submission, "submission.csv", row.names = FALSE)
