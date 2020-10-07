library("ggplot2")
library("ggthemes")
library("scales")
library("dplyr")
library("mice")
library("randomForest")
library("rpart")
library("rpart.plot")
library("RColorBrewer")
library("rattle")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
str(train)
str(test)
full <- bind_rows(train, test)
str(full)
slice(full, 886:896)
full$Title <- gsub("(.*, )|(\\..*)", "", full$Name)
table(full$Sex, full$Title)
table(full$Survived, full$Title)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Don", "Dr", "Major", "Sir", "Jonkheer")
full$Title[full$Title == "Mlle"] <- "Miss"
full$Title[full$Title == "Ms"] <- "Miss"
full$Title[full$Title == "Mme"] <- "Mrs"
full$Title[full$Title %in% rare_title] <- "Rare Title"
table(full$Sex, full$Title)
table(full$Survived, full$Title)
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
cat(paste("We have <b>", nlevels(factor(full$Surname)), "</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time."))
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep = "_")
head(full)
ggplot(full[1:891, ], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_few()
full$FsizeD[full$Fsize == 1] <- "singleton"
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- "small"
full$FsizeD[full$Fsize > 4] <- "large"
mosaicplot(table(full$FsizeD, full$Survived), main = "Family Size by Survival", shade = TRUE)
sum(is.na(full$Cabin))
full$Cabin[1:28]
strsplit(full$Cabin[2], NULL)[[1]]
full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
summary(full)
sample_n(full, 10)
sample_n(full, 10)
subset(full, full$Cabin == "" | full$Cabin == " ") %>% count()
subset(full, full$Embarked == "" | full$Embarked == " ") %>% count()
subset(full, full$Deck == "" | full$Deck == " ") %>% count()
subset(full, is.na(full$Deck)) %>% count()
subset(full, full$Embarked == "" | full$Embarked == " ")
full[c(62, 830), "Embarked"]
cat(paste("We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $", full[c(62, 830), "Fare"][[1]][1], "</b>and<b> $", full[c(62, 830), "Fare"][[1]][2], "</b>respectively and their classes are<b>", full[c(62, 830), "Pclass"][[1]][1], "</b>and<b>", full[c(62, 830), "Pclass"][[1]][2], "</b>. So from where did they embark?"))
subset(full, full$Surname == "Stone" | full$Surname == "Icard")
subset(full, full$Fare == 80)
embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + scale_y_continuous(labels = dollar_format()) + theme_few()
full$Embarked[c(62, 830)] <- "C"
full[1044, ]
ggplot(full[full$Pclass == "3" & full$Embarked == "S", ], aes(x = Fare)) + geom_density(fill = "#99d6ff", alpha = 0.4) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "red", linetype = "dashed", lwd = 1) + scale_x_continuous(labels = dollar_format()) + theme_few()
full$Fare[1044] <- median(full[full$Pclass == "3" & full$Embarked == "S", ]$Fare, na.rm = TRUE)
sum(is.na(full$Age))
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Surname", "Family", "FsizeD")
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod <- mice(full[, !names(full) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Family", "Surname", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
par(mfrow = c(1, 2))
hist(full$Age, freq = F, main = "Age: Original Data", col = "darkgreen", ylim = c(0, 0.04))
hist(mice_output$Age, freq = F, main = "Age: MICE Output", col = "lightgreen", ylim = c(0, 0.04))
full$Age <- mice_output$Age
sum(is.na(full$Age))
ggplot(full[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram(position = "fill") + facet_grid(. ~ Sex) + theme_few()
full$Child[full$Age < 6] <- "Baby"
full$Child[full$Age >= 6 & full$Age < 14] <- "Kid"
full$Child[full$Age >= 14 & full$Age < 18] <- "Young"
full$Child[full$Age >= 18 & full$Age < 60] <- "Adult"
full$Child[full$Age >= 60] <- "Old"
table(full[1:891, ]$Child, full[1:891, ]$Survived)
prop.table(table(full[1:891, ]$Child, full[1:891, ]$Survived), 1)
ggplot(full[1:891, ], aes(factor(Child), fill = factor(Survived))) + geom_histogram(stat = "count", position = "fill") + facet_grid(. ~ Sex) + theme_few()
full$Mother <- "Not Mother"
full$Mother[full$Sex == "female" & full$Parch > 0 & full$Age > 18 & full$Title != "Miss"] <- "Mother"
table(full$Mother, full$Survived)
full$Child <- factor(full$Child)
full$Mother <- factor(full$Mother)
md.pattern(full)
train <- full[1:891, ]
test <- full[892:1309, ]
set.seed(754)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child + Mother, data = train, ntree = 100, importance = TRUE)
plot(rf_model, ylim = c(0, 0.36))
legend("topright", colnames(rf_model$err.rate), col = 1:3, fill = 1:3)
varImpPlot(rf_model)
table(full[1:891, ]$Title, full[1:891, ]$Survived)
prop.table(table(full[1:891, ]$Title, full[1:891, ]$Survived), 1)
ggplot(full[1:891, ], aes(factor(Title), fill = factor(Survived))) + geom_histogram(stat = "count", position = "fill") + theme_few()
posn.jd <- position_jitterdodge(jitter.width = 0.4, dodge.width = 0.5)
ggplot(full[1:891, ], aes(y = Fare, x = Sex, col = factor(Survived))) + geom_point(size = 3, alpha = 0.4, position = posn.jd) + theme_few()
ggplot(full[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram(position = "fill") + facet_grid(. ~ Sex) + theme_few()
table(full[1:891, ]$Pclass, full[1:891, ]$Survived)
prop.table(table(full[1:891, ]$Pclass, full[1:891, ]$Survived), 1)
ggplot(full[1:891, ], aes(factor(Pclass), fill = factor(Survived))) + geom_histogram(stat = "count", position = "fill") + theme_few()
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
set.seed(754)
tree_model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child + Mother, data = train, method = "class", control = rpart.control(cp = 1e-04))
plot(tree_model)
text(tree_model)
prp(tree_model, type = 4, extra = 100)
fancyRpartPlot(tree_model)
my_prediction <- predict(tree_model, newdata = test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "tree_mod_Solution.csv", row.names = FALSE)
