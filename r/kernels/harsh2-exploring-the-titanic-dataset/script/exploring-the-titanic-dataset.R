library(Hmisc)
library(knitr)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(cowplot)
library(ROCR)
library(e1071)
train <- read.csv("../input/train.csv", stringsAsFactors = F, na.strings = c("NA", ""))
test <- read.csv("../input/test.csv", stringsAsFactors = F, na.strings = c("NA", ""))
str(train)
str(test)
test$Survived <- NA
all <- rbind(train, test)
sapply(all, function(x) {
    sum(is.na(x))
})
all$Sex <- as.factor(all$Sex)
all$Survived <- as.factor(all$Survived)
all$Pclass <- as.ordered(all$Pclass)
ggplot(all[!is.na(all$Survived), ], aes(x = Survived, fill = Survived)) + geom_bar(stat = "count") + labs(x = "How many people died and survived on the Titanic?") + geom_label(stat = "count", aes(label = ..count..))
p1 <- ggplot(all, aes(x = Sex, fill = Sex)) + geom_bar(stat = "count", position = "dodge") + labs(x = "All data") + geom_label(stat = "count", aes(label = ..count..))
p2 <- ggplot(all[!is.na(all$Survived), ], aes(x = Sex, fill = Survived)) + geom_bar(stat = "count", position = "dodge") + labs(x = "Training data only") + geom_label(stat = "count", aes(label = ..count..))
plot_grid(p1, p2)
p3 <- ggplot(all, aes(x = Pclass, fill = Pclass)) + geom_bar(stat = "count", position = "dodge") + labs(x = "Pclass, All data") + geom_label(stat = "count", aes(label = ..count..)) + theme(legend.position = "none")
p4 <- ggplot(all[!is.na(all$Survived), ], aes(x = Pclass, fill = Survived)) + geom_bar(stat = "count", position = "dodge") + labs(x = "Training data only") + theme(legend.position = "none")
p5 <- ggplot(all[!is.na(all$Survived), ], aes(x = Pclass, fill = Survived)) + geom_bar(stat = "count", position = "stack") + labs(x = "Training data only", y = "Count") + facet_grid(. ~ Sex) + theme(legend.position = "none")
p6 <- ggplot(all[!is.na(all$Survived), ], aes(x = Pclass, fill = Survived)) + geom_bar(stat = "count", position = "fill") + labs(x = "Training data only", y = "Percent") + facet_grid(. ~ Sex) + theme(legend.position = "none")
plot_grid(p3, p4, p5, p6, ncol = 2)
all$PclassSex[all$Pclass == "1" & all$Sex == "male"] <- "P1Male"
all$PclassSex[all$Pclass == "2" & all$Sex == "male"] <- "P2Male"
all$PclassSex[all$Pclass == "3" & all$Sex == "male"] <- "P3Male"
all$PclassSex[all$Pclass == "1" & all$Sex == "female"] <- "P1Female"
all$PclassSex[all$Pclass == "2" & all$Sex == "female"] <- "P2Female"
all$PclassSex[all$Pclass == "3" & all$Sex == "female"] <- "P3Female"
all$PclassSex <- as.factor(all$PclassSex)
all$Surname <- sapply(all$Name, function(x) {
    strsplit(x, split = "[,.]")[[1]][1]
})
all$Surname <- sapply(all$Surname, function(x) {
    strsplit(x, split = "[-]")[[1]][1]
})
all$Title <- sapply(all$Name, function(x) {
    strsplit(x, split = "[,.]")[[1]][2]
})
all$Title <- sub(" ", "", all$Title)
kable(table(all$Sex, all$Title))
all$Title[all$Title %in% c("Mlle", "Ms")] <- "Miss"
all$Title[all$Title == "Mme"] <- "Mrs"
all$Title[!(all$Title %in% c("Master", "Miss", "Mr", "Mrs"))] <- "Rare Title"
all$Title <- as.factor(all$Title)
kable(table(all$Sex, all$Title))
ggplot(all[!is.na(all$Survived), ], aes(x = Title, fill = Survived)) + geom_bar(stat = "count", position = "stack") + labs(x = "Title")
all$Fsize <- all$SibSp + all$Parch + 1
ggplot(all[!is.na(all$Survived), ], aes(x = Fsize, fill = Survived)) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size")
all$FsizeName <- paste(as.character(all$Fsize), all$Surname, sep = "")
SizeCheck <- all %>% group_by(FsizeName, Fsize) %>% summarise(NumObs = n())
SizeCheck$NumFam <- SizeCheck$NumObs/SizeCheck$Fsize
SizeCheck$modulo <- SizeCheck$NumObs%%SizeCheck$Fsize
SizeCheck <- SizeCheck[SizeCheck$modulo != 0, ]
sum(SizeCheck$NumObs)
kable(tail(SizeCheck))
kable(all[all$Ticket %in% c("29104", "29105", "29106"), c(2, 3, 4, 5, 6, 7, 8, 9, 15)])
NC <- all[all$FsizeName %in% SizeCheck$FsizeName, ]
NC$Name <- sub("\\s$", "", NC$Name)
NC$Maiden <- sub(".*[^\\)]$", "", NC$Name)
NC$Maiden <- sub(".*\\s(.*)\\)$", "\\1", NC$Maiden)
NC$Maiden[NC$Title != "Mrs"] <- ""
NC$Maiden <- sub("^\\(", "", NC$Maiden)
NC$Maiden[NC$Name == "Andersen-Jensen, Miss. Carla Christine Nielsine"] <- "Jensen"
NC$Maiden2[NC$Maiden %in% NC$Surname] <- NC$Maiden[NC$Maiden %in% NC$Surname]
NC$Combi[!is.na(NC$Maiden2)] <- paste(NC$Surname[!is.na(NC$Maiden2)], NC$Maiden[!is.na(NC$Maiden2)])
labels1 <- NC[!is.na(NC$Combi), c("Surname", "Combi")]
labels2 <- NC[!is.na(NC$Combi), c("Maiden", "Combi")]
colnames(labels2) <- c("Surname", "Combi")
labels1 <- rbind(labels1, labels2)
NC$Combi <- NULL
NC <- left_join(NC, labels1, by = "Surname")
CombiMaxF <- NC[!is.na(NC$Combi), ] %>% group_by(Combi) %>% summarise(MaxF = max(Fsize))
NC <- left_join(NC, CombiMaxF, by = "Combi")
NC$FsizeCombi[!is.na(NC$Combi)] <- paste(as.character(NC$Fsize[!is.na(NC$Combi)]), NC$Combi[!is.na(NC$Combi)], sep = "")
FamMaid <- NC[!is.na(NC$FsizeCombi), ] %>% group_by(FsizeCombi, MaxF, Fsize) %>% summarise(NumObs = n())
FamMaidWrong <- FamMaid[FamMaid$MaxF != FamMaid$NumObs, ]
kable(unique(NC[!is.na(NC$Combi) & NC$FsizeCombi %in% FamMaidWrong$FsizeCombi, c("Combi", "MaxF")]))
NC$MaxF <- NULL
FamMale <- NC[is.na(NC$Combi), ] %>% group_by(Surname) %>% summarise(MaxF = max(Fsize))
NC <- left_join(NC, FamMale, by = "Surname")
NCMale <- NC[is.na(NC$Combi), ] %>% group_by(Surname, FsizeName, MaxF) %>% summarise(count = n()) %>% group_by(Surname, MaxF) %>% filter(n() > 1) %>% summarise(NumFsizes = n())
NC$Combi[NC$Surname %in% NCMale$Surname] <- NC$Surname[NC$Surname %in% NCMale$Surname]
kable(NCMale[, c(1, 2)])
kable(all[all$Surname == "Vander Planke", c(2, 3, 4, 5, 6, 7, 8, 9, 15)])
NC <- NC[(NC$FsizeCombi %in% FamMaidWrong$FsizeCombi) | (NC$Surname %in% NCMale$Surname), ]
NC1 <- NC %>% group_by(Combi) %>% summarise(Favg = mean(Fsize))
kable(NC1)
NC <- left_join(NC, NC1, by = "Combi")
NC$Favg <- round(NC$Favg)
NC <- NC[, c("PassengerId", "Favg")]
all <- left_join(all, NC, by = "PassengerId")
all$Fsize[!is.na(all$Favg)] <- all$Favg[!is.na(all$Favg)]
kable(all[all$Ticket == "1601", c("Survived", "Pclass", "Title", "Surname", "Age", "Ticket", "SibSp", "Parch", "Fsize")])
TicketGroup <- all %>% select(Ticket) %>% group_by(Ticket) %>% summarise(Tsize = n())
all <- left_join(all, TicketGroup, by = "Ticket")
ggplot(all[!is.na(all$Survived), ], aes(x = Tsize, fill = Survived)) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Ticket Size")
all$Group <- all$Fsize
for (i in 1:nrow(all)) {
    all$Group[i] <- max(all$Group[i], all$Tsize[i])
}
all$Ticket2 <- sub("..$", "xx", all$Ticket)
rest <- all %>% select(PassengerId, Title, Age, Ticket, Ticket2, Surname, Group) %>% filter(Group == "1") %>% group_by(Ticket2, Surname) %>% summarise(count = n())
rest <- rest[rest$count > 1, ]
rest1 <- all[(all$Ticket2 %in% rest$Ticket2 & all$Surname %in% rest$Surname & all$Group == "1"), c("PassengerId", "Surname", "Title", "Age", "Ticket", "Ticket2", "Group", "SibSp", "Parch")]
rest1 <- left_join(rest1, rest, by = c("Surname", "Ticket2"))
rest1 <- rest1 %>% arrange(Ticket2, Surname)
kable(rest1[1:12, ])
all <- left_join(all, rest1)
for (i in 1:nrow(all)) {
    if (!is.na(all$count[i])) {
        all$Group[i] <- all$count[i]
    }
}
ggplot(all[!is.na(all$Survived), ], aes(x = Group, fill = Survived)) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Final Groups")
all$GroupSize[all$Group == 1] <- "solo"
all$GroupSize[all$Group == 2] <- "duo"
all$GroupSize[all$Group >= 3 & all$Group <= 4] <- "group"
all$GroupSize[all$Group >= 5] <- "large group"
all$GroupSize <- as.factor(all$GroupSize)
ggplot(all[!is.na(all$Survived), ], aes(x = GroupSize, fill = Survived)) + geom_bar(stat = "count", position = "dodge") + labs(x = "Final Group Sizes")
all$count <- NULL
all$Name <- NULL
rm(CombiMaxF)
rm(FamMaid)
rm(FamMaidWrong)
rm(FamMale)
rm(labels1)
rm(labels2)
rm(NC)
rm(NC1)
rm(NCMale)
rm(rest)
rm(rest1)
rm(SizeCheck)
rm(TicketGroup)
rm(p1)
rm(p2)
rm(p3)
rm(p4)
rm(p5)
rm(p6)
kable(all[which(is.na(all$Embarked)), c("Surname", "Title", "Survived", "Pclass", "Age", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Embarked", "Group")])
all$FarePP <- all$Fare/all$Tsize
tab2 <- all[(!is.na(all$Embarked) & !is.na(all$Fare)), ] %>% group_by(Embarked, Pclass) %>% summarise(FarePP = median(FarePP))
kable(tab2)
all$Embarked[all$Ticket == "113572"] <- "C"
all$Embarked <- as.factor(all$Embarked)
all$FarePP[1044] <- 7.8
tab3 <- all[(!is.na(all$FarePP)), ] %>% group_by(Pclass) %>% summarise(MedianFarePP = median(FarePP))
all <- left_join(all, tab3, by = "Pclass")
all$FarePP[which(all$FarePP == 0)] <- all$MedianFarePP[which(all$FarePP == 0)]
hist(all$FarePP, main = "Histogram of Fare Per Person", col = "blue", xlab = "Fare Per Person")
all$FareBins <- cut2(all$FarePP, g = 5)
table(all$FareBins)
ggplot(all[(!is.na(all$Survived) & !is.na(all$Age)), ], aes(x = Age, fill = Survived)) + geom_density(alpha = 0.5, aes(fill = factor(Survived))) + labs(title = "Survival density and Age") + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
ggplot(all[!is.na(all$Age), ], aes(x = Title, y = Age, fill = Pclass)) + geom_boxplot() + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
set.seed(12000)
AgeLM <- lm(Age ~ Pclass + Sex + SibSp + Parch + Embarked + Title + GroupSize, data = all[!is.na(all$Age), ])
summary(AgeLM)
all$AgeLM <- predict(AgeLM, all)
par(mfrow = c(1, 2))
hist(all$Age[!is.na(all$Age)], main = "Original data, non-missing", xlab = "Age", col = "green")
hist(all$AgeLM[is.na(all$Age)], main = "LM NA predictions", xlab = "Age", col = "orange", xlim = range(0:80))
all[(is.na(all$Age) & all$AgeLM < 18), c("Sex", "SibSp", "Parch", "Title", "Pclass", "Survived", "AgeLM")]
indexMissingAge <- which(is.na(all$Age))
indexAgeSurvivedNotNA <- which(!is.na(all$Age) & (!is.na(all$Survived)))
all$Age[indexMissingAge] <- all$AgeLM[indexMissingAge]
all$Cabin[is.na(all$Cabin)] <- "U"
all$Cabin <- substring(all$Cabin, 1, 1)
all$Cabin <- as.factor(all$Cabin)
ggplot(all[(!is.na(all$Survived) & all$Cabin != "U"), ], aes(x = Cabin, fill = Survived)) + geom_bar(stat = "count") + facet_grid(. ~ Pclass) + labs(title = "Survivor split by class and Cabin")
c1 <- round(prop.table(table(all$Survived[(!is.na(all$Survived) & all$Cabin != "U")], all$Cabin[(!is.na(all$Survived) & all$Cabin != "U")]), 2) * 100)
kable(c1)
ggplot(all[all$Age < 14.5 & !is.na(all$Survived), ], aes(x = Pclass, fill = Survived)) + geom_bar(stat = "count")
all$IsChildP12 <- "No"
all$IsChildP12[all$Age <= 14.5 & all$Pclass %in% c("1", "2")] <- "Yes"
all$IsChildP12 <- as.factor(all$IsChildP12)
d1 <- ggplot(all[!is.na(all$Survived), ], aes(x = Embarked, fill = Survived)) + geom_bar(stat = "count") + labs(x = "Embarked", y = "Count")
d2 <- ggplot(all[!is.na(all$Survived), ], aes(x = Embarked, fill = Survived)) + geom_bar(stat = "count", position = "fill") + labs(x = "Embarked", y = "Percent")
plot_grid(d1, d2)
ggplot(all[indexAgeSurvivedNotNA, ], aes(x = Age, fill = Survived)) + geom_histogram(aes(fill = factor(Survived))) + labs(title = "Survival density, known-ages, and Embarked") + scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + facet_grid(. ~ Embarked)
tab1 <- rbind(table(all$Embarked[!is.na(all$Survived)]), table(all$Embarked[indexAgeSurvivedNotNA]))
tab1 <- cbind(tab1, (rowSums(tab1)))
tab1 <- rbind(tab1, tab1[1, ] - tab1[2, ])
tab1 <- rbind(tab1, round((tab1[3, ]/tab1[1, ]) * 100))
rownames(tab1) <- c("All", "With Age", "Missing Age", "Percent Missing")
colnames(tab1) <- c("C", "Q", "S", "Total")
kable(tab1)
TicketSurvivors <- all %>% group_by(Ticket) %>% summarize(Tsize = length(Survived), NumNA = sum(is.na(Survived)), SumSurvived = sum(as.numeric(Survived) - 1, na.rm = T))
all <- left_join(all, TicketSurvivors)
all$AnySurvivors[all$Tsize == 1] <- "other"
all$AnySurvivors[all$Tsize >= 2] <- ifelse(all$SumSurvived[all$Tsize >= 2] >= 1, "survivors in group", "other")
all$AnySurvivors <- as.factor(all$AnySurvivors)
table(all$AnySurvivors)
all$PassengerId <- NULL
all$SibSp <- NULL
all$Parch <- NULL
all$Ticket <- NULL
all$Fare <- NULL
all$Cabin <- NULL
all$Surname <- NULL
all$Fsize <- NULL
all$FsizeName <- NULL
all$Favg <- NULL
all$Tsize <- NULL
all$Group <- NULL
all$Ticket2 <- NULL
all$AgeLM <- NULL
all$Child <- NULL
all$HasParch <- NULL
all$MedianFarePP <- NULL
rm(tab1)
rm(tab2)
rm(tab3)
rm(AgeLM)
rm(c1)
rm(d1)
rm(d2)
trainClean <- all[!is.na(all$Survived), ]
testClean <- all[is.na(all$Survived), ]
set.seed(2017)
caret_matrix <- train(x = trainClean[, c("PclassSex", "GroupSize", "FarePP", "AnySurvivors", "IsChildP12")], y = trainClean$Survived, data = trainClean, method = "rf", trControl = trainControl(method = "cv", number = 7))
caret_matrix
caret_matrix$resample
caret_matrix$results
varImpPlot(caret_matrix$finalModel, main = " Variable importance")
solution_rf <- predict(caret_matrix, testClean)
submission_rf <- data.frame(PassengerId = test$PassengerId, Survived = solution_rf)
write.csv(submission_rf, file = "Titanic_rf.csv", row.names = F)
