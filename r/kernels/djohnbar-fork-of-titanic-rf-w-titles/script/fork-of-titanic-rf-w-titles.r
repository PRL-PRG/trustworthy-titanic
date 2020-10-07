library(tidyverse)
library(randomForest)
list.files(path = "../input")
t1 <- read.csv("../input/train.csv", stringsAsFactors = F)
t2 <- read.csv("../input/test.csv", stringsAsFactors = F)
all <- bind_rows(t1, t2)
colSums(is.na(all))
all$Fare[all$Fare == 0] <- NA
all$Fare <- as.numeric(all$Fare)
all$Embarked[all$Embarked == ""] <- "S"
all$Embarked[all$Embarked == "C"] <- 0
all$Embarked[all$Embarked == "Q"] <- 1
all$Embarked[all$Embarked == "S"] <- 2
all$Embarked <- as.numeric(all$Embarked)
all$Sex[all$Sex == "female"] <- 1
all$Sex[all$Sex == "male"] <- 0
all$Sex <- as.numeric(all$Sex)
str(all)
pat <- "\\,*+\\w+\\."
titles <- str_extract(all$Name, pattern = pat)
table(titles)
all$titles <- titles
ti <- data.frame(num = 1:length(unique(all$titles)), titel = unique(all$titles))
for (i in 1:nrow(ti)) {
    all$titles[all$titles == ti$titel[i]] <- ti$num[i]
}
all$titles <- as.numeric(all$titles)
cor(all[c("Fare", "Age", "SibSp", "Parch", "Pclass", "Sex", "Embarked", "Survived", "titles")], use = "c")
not.missing <- which(!is.na(all[, c("Fare", "Age")]))
missing.fare <- which(is.na(all[, c("Fare")]))
missing.fare
model <- glm(Fare ~ Pclass + Embarked, data = all[not.missing, ])
pred <- predict(model, all[missing.fare, ])
data.frame(pred)
all$Fare[missing.fare] <- pred
not.missing <- which(!is.na(all$Age))
missing <- which(is.na(all$Age))
model <- glm(Age ~ Pclass + Embarked, data = all[not.missing, ])
pred <- predict(model, all[missing, ])
data.frame(pred = head(pred))
all$Age[missing] <- pred
train.val <- as.integer(891 * 0.7)
test.val <- 891 - train.val
all$Survived <- as.factor(all$Survived)
train <- all[1:train.val, ]
test <- all[624:891, ]
model <- randomForest(Survived ~ Sex + Pclass + titles + Fare, data = train)
pred <- predict(model, test)
table(pred = pred, actual = test$Survived)
mean(pred == test$Survived)
model
options(repr.plot.width = 7, repr.plot.height = 5)
varImpPlot(model, sort = T)
pred <- predict(model, all[892:1309, ])
submit <- data.frame(PassengerId = 892:1309, Survived = pred)
write.csv(submit, file = "RF_submission_t.csv", row.names = F)
