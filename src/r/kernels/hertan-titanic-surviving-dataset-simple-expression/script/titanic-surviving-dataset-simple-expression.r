library("dplyr")
library("randomForest")
library("ggplot2")
library("mice")
path <- "../input/"
train.df <- read.csv(paste0(path, "train.csv"), na.strings = c(""), stringsAsFactors = FALSE)
test.df <- read.csv(paste0(path, "test.csv"), na.strings = c(""), stringsAsFactors = FALSE)
all.df <- bind_rows(train.df, test.df)
all.df$Survived <- as.factor(all.df$Survived)
all.df$Pclass <- as.factor(all.df$Pclass)
all.df$Sex <- as.factor(all.df$Sex)
all.df$Embarked <- as.factor(all.df$Embarked)
all.df$Cabin <- as.character(all.df$Cabin)
all.df$Name <- as.character(all.df$Name)
all.df$Ticket <- as.character(all.df$Ticket)
all.df$Set <- "Train"
all.df[892:1309, ]$Set <- "Test"
title <- sapply(all.df$Name, function(x) substr(x, grep(",", unlist(strsplit(x, ""))) + 2, which(strsplit(x, "")[[1]] == ".") - 1))
title[title == "Mlle"] <- "Miss"
title[title == "Ms"] <- "Miss"
title[title == "Mme"] <- "Mrs"
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
title[title %in% rare_title] <- "Rare"
title <- as.factor(title)
all.df$Title <- title
ggplot(all.df[all.df$Set == "Train", ], aes(Title, fill = Survived)) + geom_bar(stat = "count") + xlab("Title") + ylab("Count") + scale_fill_discrete(name = "Survived") + ggtitle("Title vs Survived")
all.df$RelativeNumber <- all.df$SibSp + all.df$Parch + 1
ggplot(all.df[all.df$Set == "Train", ], aes(RelativeNumber, fill = Survived)) + geom_bar(stat = "count") + xlab("RelativeNumber") + ylab("Count") + scale_fill_discrete(name = "Survived") + ggtitle("RelativeNumber vs Survived")
all.df$FamilySize[all.df$RelativeNumber == 1] <- "single"
all.df$FamilySize[all.df$RelativeNumber > 1 & all.df$RelativeNumber < 5] <- "small"
all.df$FamilySize[all.df$RelativeNumber > 4] <- "large"
all.df$FamilySize <- as.factor(all.df$FamilySize)
all.df$CabinInfo <- NA
all.df[is.na(all.df$Cabin), ]$CabinInfo <- "NA"
all.df[!is.na(all.df$Cabin), ]$CabinInfo <- substr(all.df[!is.na(all.df$Cabin), ]$Cabin, 1, 1)
all.df[all.df$CabinInfo == "T", ]$CabinInfo <- "NA"
all.df[all.df$CabinInfo == "G", ]$CabinInfo <- "NA"
ggplot(all.df[all.df$Set == "Train", ], aes(CabinInfo, fill = Survived)) + geom_bar(stat = "count") + xlab("CabinInfo") + ylab("Count") + scale_fill_discrete(name = "Survived") + ggtitle("CabinInfo vs Survived")
all.df[all.df$CabinInfo == "B", ]$CabinInfo <- "BDE"
all.df[all.df$CabinInfo == "D", ]$CabinInfo <- "BDE"
all.df[all.df$CabinInfo == "E", ]$CabinInfo <- "BDE"
all.df[all.df$CabinInfo == "A", ]$CabinInfo <- "ACF"
all.df[all.df$CabinInfo == "C", ]$CabinInfo <- "ACF"
all.df[all.df$CabinInfo == "F", ]$CabinInfo <- "ACF"
all.df$CabinInfo <- as.factor(all.df$CabinInfo)
mice_model <- mice(all.df[, c("Age", "Pclass", "Sex", "SibSp", "Parch", "Embarked", "Title", "RelativeNumber", "FamilySize")], method = "rf", printFlag = FALSE)
mice_output <- complete(mice_model)
all.df[is.na(all.df$Age), "Age"] <- mice_output[which(is.na(all.df$Age)), "Age"]
all.df$Age <- round(all.df$Age)
na.fare <- all.df[which(is.na(all.df$Fare)), ]
all.df$Fare[1044] <- median(all.df[all.df$Pclass == 3 & all.df$Embarked == "S" & all.df$Title == "Mr", ]$Fare, na.rm = TRUE)
na.embarked <- all.df[which(is.na(all.df$Embarked)), ]
C_Fare <- mean(all.df[all.df$Embarked == "C", ]$Fare, na.rm = T)
S_Fare <- mean(all.df[all.df$Embarked == "S", ]$Fare, na.rm = T)
Q_Fare <- mean(all.df[all.df$Embarked == "Q", ]$Fare, na.rm = T)
all.df$Embarked[62] <- as.factor("C")
all.df$Embarked[830] <- as.factor("C")
train.class3 <- all.df[all.df$Pclass == 3 & all.df$Set == "Train", ]
train.class2 <- all.df[all.df$Pclass == 2 & all.df$Set == "Train", ]
train.class1 <- all.df[all.df$Pclass == 1 & all.df$Set == "Train", ]
ggplot(train.class3, aes(y = train.class3$Pclass, x = train.class3$Fare)) + geom_point(aes(color = train.class3$Survived), position = "jitter") + xlab("Fare") + ylab("Pclass") + geom_vline(aes(xintercept = 10), colour = "black", linetype = "dashed", lwd = 1) + geom_vline(aes(xintercept = 20), colour = "black", linetype = "dashed", lwd = 1) + scale_color_discrete(name = "Survived") + ggtitle("Pclass1 vs Fare vs Survived")
ggplot(train.class2, aes(y = train.class2$Pclass, x = train.class2$Fare)) + xlab("Fare") + ylab("Pclass") + scale_color_discrete(name = "Survived") + geom_point(aes(color = train.class2$Survived), position = "jitter") + geom_vline(aes(xintercept = 20), colour = "black", linetype = "dashed", lwd = 1) + ggtitle("Pclass2 vs Fare vs Survived")
ggplot(train.class1, aes(y = train.class1$Pclass, x = train.class1$Fare)) + xlab("Fare") + ylab("Pclass") + scale_color_discrete(name = "Survived") + geom_point(aes(color = train.class1$Survived), position = "jitter") + geom_vline(aes(xintercept = 50), colour = "black", linetype = "dashed", lwd = 1) + geom_vline(aes(xintercept = 120), colour = "black", linetype = "dashed", lwd = 1) + ggtitle("Pclass3 vs Fare vs Survived")
all.df$Class.fare.level[all.df$Pclass == 3 & all.df$Fare < 10] <- "P3_1"
all.df$Class.fare.level[all.df$Pclass == 3 & all.df$Fare >= 10 & all.df$Fare < 20] <- "P3_2"
all.df$Class.fare.level[all.df$Pclass == 3 & all.df$Fare >= 20] <- "P3_3"
all.df$Class.fare.level[all.df$Pclass == 2 & all.df$Fare < 20] <- "P2_1"
all.df$Class.fare.level[all.df$Pclass == 2 & all.df$Fare >= 20] <- "P2_2"
all.df$Class.fare.level[all.df$Pclass == 1 & all.df$Fare < 50] <- "P1_1"
all.df$Class.fare.level[all.df$Pclass == 1 & all.df$Fare >= 50 & all.df$Fare < 120] <- "P1_2"
all.df$Class.fare.level[all.df$Pclass == 1 & all.df$Fare >= 120] <- "P1_3"
all.df$Class.fare.level <- as.factor(all.df$Class.fare.level)
train.female <- all.df[all.df$Sex == "female" & all.df$Set == "Train", ]
train.male <- all.df[all.df$Sex == "male" & all.df$Set == "Train", ]
age.survived.female <- table(train.female$Age, train.female$Survived)
age.survived.female <- cbind(age.survived.female, age.survived.female[, 1]/(age.survived.female[, 1] + age.survived.female[, 2]))
age.survived.female.df <- data.frame(age.survived.female)
age.survived.female.df <- cbind(age.survived.female.df, rownames(age.survived.female.df))
colnames(age.survived.female.df) <- c("not_survived", "survived", "death_rate", "age")
age.survived.female.df <- cbind(age.survived.female.df, total = age.survived.female.df$not_survived + age.survived.female.df$survived)
age.survived.male <- table(train.male$Age, train.male$Survived)
age.survived.male <- cbind(age.survived.male, age.survived.male[, 1]/(age.survived.male[, 1] + age.survived.male[, 2]))
age.survived.male.df <- data.frame(age.survived.male)
age.survived.male.df <- cbind(age.survived.male.df, rownames(age.survived.male.df))
colnames(age.survived.male.df) <- c("not_survived", "survived", "death_rate", "age")
age.survived.male.df <- cbind(age.survived.male.df, total = age.survived.male.df$not_survived + age.survived.male.df$survived)
glimpse(age.survived.female.df)
ggplot(data = age.survived.female.df, aes(y = age.survived.female.df$death_rate, x = age.survived.female.df$age)) + geom_point(size = age.survived.female.df$total, alpha = 0.3) + geom_hline(aes(yintercept = 0.5), colour = "red", linetype = "dashed", lwd = 1) + xlab("Age") + ylab("Death Rate")
ggplot(data = age.survived.male.df, aes(y = age.survived.male.df$death_rate, x = age.survived.male.df$age)) + geom_point(size = age.survived.male.df$total, alpha = 0.3) + geom_hline(aes(yintercept = 0.5), colour = "red", linetype = "dashed", lwd = 1) + xlab("Age") + ylab("Death Rate")
ggplot(all.df[all.df$Set == "Train", ], aes(y = all.df[all.df$Set == "Train", ]$Pclass, x = all.df[all.df$Set == "Train", ]$Age)) + facet_wrap(~Sex) + xlab("Age") + ylab("Pclass") + scale_color_discrete(name = "Survived") + geom_point(aes(color = all.df[all.df$Set == "Train", ]$Survived), position = "jitter") + geom_vline(aes(xintercept = 10), colour = "black", linetype = "dashed", lwd = 1) + geom_vline(aes(xintercept = 40), colour = "black", linetype = "dashed", lwd = 1) + ggtitle("Pclass vs Age vs Sex vs Survived")
all.df$Class.sex.age.level <- NA
all.df[all.df$Age <= 10 & all.df$Sex == "female" & all.df$Pclass == 1, ]$Class.sex.age.level <- "cF1"
all.df[all.df$Age <= 10 & all.df$Sex == "male" & all.df$Pclass == 1, ]$Class.sex.age.level <- "cM1"
all.df[all.df$Age <= 10 & all.df$Sex == "female" & all.df$Pclass == 2, ]$Class.sex.age.level <- "cF2"
all.df[all.df$Age <= 10 & all.df$Sex == "male" & all.df$Pclass == 2, ]$Class.sex.age.level <- "cM2"
all.df[all.df$Age <= 10 & all.df$Sex == "female" & all.df$Pclass == 3, ]$Class.sex.age.level <- "cF3"
all.df[all.df$Age <= 10 & all.df$Sex == "male" & all.df$Pclass == 3, ]$Class.sex.age.level <- "cM3"
all.df[all.df$Age > 10 & all.df$Age <= 40 & all.df$Sex == "female" & all.df$Pclass == 1, ]$Class.sex.age.level <- "aF1"
all.df[all.df$Age > 10 & all.df$Age <= 40 & all.df$Sex == "male" & all.df$Pclass == 1, ]$Class.sex.age.level <- "aM1"
all.df[all.df$Age > 10 & all.df$Age <= 40 & all.df$Sex == "female" & all.df$Pclass == 2, ]$Class.sex.age.level <- "aF2"
all.df[all.df$Age > 10 & all.df$Age <= 40 & all.df$Sex == "male" & all.df$Pclass == 2, ]$Class.sex.age.level <- "aM2"
all.df[all.df$Age > 10 & all.df$Age <= 40 & all.df$Sex == "female" & all.df$Pclass == 3, ]$Class.sex.age.level <- "aF3"
all.df[all.df$Age > 10 & all.df$Age <= 40 & all.df$Sex == "male" & all.df$Pclass == 3, ]$Class.sex.age.level <- "aM3"
all.df[all.df$Age > 40 & all.df$Sex == "female" & all.df$Pclass == 1, ]$Class.sex.age.level <- "oF1"
all.df[all.df$Age > 40 & all.df$Sex == "male" & all.df$Pclass == 1, ]$Class.sex.age.level <- "oM1"
all.df[all.df$Age > 40 & all.df$Sex == "female" & all.df$Pclass == 2, ]$Class.sex.age.level <- "oF2"
all.df[all.df$Age > 40 & all.df$Sex == "male" & all.df$Pclass == 2, ]$Class.sex.age.level <- "oM2"
all.df[all.df$Age > 40 & all.df$Sex == "female" & all.df$Pclass == 3, ]$Class.sex.age.level <- "oF3"
all.df[all.df$Age > 40 & all.df$Sex == "male" & all.df$Pclass == 3, ]$Class.sex.age.level <- "oM3"
all.df[all.df$Class.sex.age.level %in% c("aF3", "aM1", "cF1", "cF3", "cM1", "cM3"), ]$Class.sex.age.level <- "other"
all.df$Class.sex.age.level <- as.factor(all.df$Class.sex.age.level)
ggplot(all.df[all.df$Set == "Train", ], aes(Class.sex.age.level, fill = Survived)) + geom_bar(stat = "count") + xlab("Class.sex.age.level") + ylab("Count") + scale_fill_discrete(name = "Survived") + ggtitle("Class.sex.age.level vs Survived")
all.df$Surname <- sapply(all.df$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
passenger_stats <- all.df %>% group_by(Surname, RelativeNumber, Ticket, Cabin) %>% summarize(family.size = n(), unknown = sum(is.na(Survived)), survived.info = sum(as.numeric(Survived) - 1, na.rm = T), death.info = family.size - (unknown + survived.info))
all.df <- cbind(all.df, all.df %>% left_join(passenger_stats, c("Surname", "RelativeNumber", "Ticket", "Cabin")) %>% select("family.size", "unknown", "survived.info", "death.info"))
all.df$Travelling.together <- NA
all.df[all.df$family.size == 1, ]$Travelling.together <- "alone"
all.df[all.df$family.size > 1 & all.df$survived.info == 0 & all.df$death.info == 0, ]$Travelling.together <- "no_info"
for (i in 1:891) {
    if ((all.df$family.size[i] > 1) & (all.df$survived.info[i] > 0 | all.df$death.info[i] > 0)) {
        if (all.df$Survived[i] == 1) {
            all.df[i, ]$Travelling.together <- round((all.df$survived.info[i] - 1)/(all.df$family.size[i] - 1), 2)
        }
        else {
            all.df[i, ]$Travelling.together <- round(all.df$survived.info[i]/(all.df$family.size[i] - 1), 2)
        }
    }
}
for (i in 892:1309) {
    if ((all.df$family.size[i] > 1) & (all.df$survived.info[i] > 0 | all.df$death.info[i] > 0)) {
        all.df[i, ]$Travelling.together <- round((all.df$survived.info[i]/all.df$family.size[i]), 2)
    }
}
ggplot(all.df[all.df$Set == "Train", ], aes(Travelling.together, fill = Survived)) + geom_bar(stat = "count") + xlab("Travelling.together") + ylab("Count") + scale_fill_discrete(name = "Survived") + ggtitle("Train Travelling.together vs Survived")
ggplot(all.df[all.df$Set == "Test", ], aes(Travelling.together)) + geom_bar(stat = "count") + xlab("Travelling.together") + ylab("Count") + ggtitle("Test Travelling.together vs Survived")
all.df[!(all.df$Travelling.together %in% c("alone", "0", "no_info")), ]$Travelling.together <- "survived_in_group"
all.df$Travelling.together <- as.factor(all.df$Travelling.together)
ggplot(all.df[all.df$Set == "Train", ], aes(Travelling.together, fill = Survived)) + geom_bar(stat = "count") + xlab("Travelling.together") + ylab("Count") + scale_fill_discrete(name = "Survived") + ggtitle("Train Travelling.together vs Survived")
ggplot(all.df[all.df$Set == "Test", ], aes(Travelling.together)) + geom_bar(stat = "count") + xlab("Travelling.together") + ylab("Count") + ggtitle("Test Travelling.together vs Survived")
feature.set <- c("Survived", "Pclass", "Sex", "Age", "Fare", "Embarked", "Title", "FamilySize", "CabinInfo", "Class.fare.level", "Class.sex.age.level", "Travelling.together")
all.df.model.input <- all.df[, feature.set]
train.df <- all.df.model.input[1:891, ]
test.df <- all.df.model.input[892:1309, ]
fit.model.randomforest <- randomForest(Survived ~ ., data = train.df, importance = TRUE, keep.forest = TRUE)
pred <- predict(fit.model.randomforest, test.df[, -1], type = "class")
predictions <- data.frame(c(892:1309), pred)
names(predictions) <- c("PassengerId", "Survived")
write.csv(predictions, "rf_model_prediction.csv", row.names = FALSE)
importance <- importance(fit.model.randomforest)
Feature.importance <- data.frame(Features = row.names(importance), Importance = round(importance[, "MeanDecreaseAccuracy"], 2))
Feature.importance <- Feature.importance %>% mutate(Rank = dense_rank(desc(Importance)))
ggplot(Feature.importance, aes(x = reorder(Features, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + geom_text(aes(x = Features, label = Rank), size = 5, y = 0.5, colour = "red") + labs(x = "Features") + coord_flip()
