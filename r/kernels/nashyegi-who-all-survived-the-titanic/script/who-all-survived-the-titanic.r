library(ggplot2)
library(readr)
system("ls ../input")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
test$Survived <- NA
allData <- rbind(train, test)
head(allData, n = 3)
str(allData)
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    library(grid)
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
    if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), ncol = cols, nrow = ceiling(numPlots/cols))
    }
    if (numPlots == 1) {
        print(plots[[1]])
    }
    else {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        for (i in 1:numPlots) {
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
        }
    }
}
genderImpact <- data.frame(table(allData$Sex, allData$Survived))
names(genderImpact) <- c("Sex", "Survived", "Count")
sexVsSurvivedGraph <- ggplot(genderImpact, aes(x = Sex, y = Count, fill = Survived))
p1 <- sexVsSurvivedGraph + geom_bar(stat = "identity")
pClassImpact <- data.frame(table(allData$Pclass, allData$Survived))
names(pClassImpact) <- c("Pclass", "Survived", "Count")
pClassVsSurvivedGraph <- ggplot(pClassImpact, aes(x = Pclass, y = Count, fill = Survived))
p2 <- pClassVsSurvivedGraph + geom_bar(stat = "identity")
sibSpImpact <- data.frame(table(allData$SibSp + allData$Parch, allData$Survived))
names(sibSpImpact) <- c("FamilyMembers", "Survived", "Count")
sibSpVsSurvivedGraph <- ggplot(sibSpImpact, aes(x = FamilyMembers, y = Count, fill = Survived))
p3 <- sibSpVsSurvivedGraph + geom_bar(stat = "identity")
ageImpact <- data.frame(Age = allData$Age, Survived = allData$Survived)
p4 <- ggplot(ageImpact, aes(Age, fill = factor(Survived))) + geom_histogram()
embarkedImpact <- data.frame(table(allData$Survived, allData$Embarked))
names(embarkedImpact) <- c("Survived", "Embarked", "Count")
embarkVsSurvivedGraph <- ggplot(embarkedImpact, aes(x = Embarked, y = Count, fill = Survived))
p5 <- embarkVsSurvivedGraph + geom_bar(stat = "identity")
cabinImpact <- data.frame(table(allData$Survived, substr(allData$Cabin, 0, 1)))
names(cabinImpact) <- c("Survived", "Cabin", "Count")
cabinVsSurvivedGraph <- ggplot(cabinImpact, aes(x = Cabin, y = Count, fill = Survived))
p6 <- cabinVsSurvivedGraph + geom_bar(stat = "identity")
multiplot(p1, p2, p3, p4, p5, p6, cols = 2)
age <- allData$Age
n = length(age)
set.seed(123)
for (i in 1:n) {
    if (is.na(age[i])) {
        age[i] = sample(na.omit(allData$Age), 1)
    }
}
f.survived = train$Survived
t.survived = test$Survived
f.age = age[1:891]
t.age = age[892:1309]
f.cabin = substr(allData$Cabin, 0, 1)[1:891]
t.cabin = substr(allData$Cabin, 0, 1)[892:1309]
family <- allData$SibSp + allData$Parch
f.family = family[1:891]
t.family = family[892:1309]
f.pclass = train$Pclass
t.pclass = test$Pclass
f.sex = train$Sex
t.sex = test$Sex
f.embarked = allData$Embarked[1:891]
t.embarked = allData$Embarked[892:1309]
new_train = data.frame(survived = f.survived, age = f.age, sex = f.sex, embarked = f.embarked, family = f.family, cabin = f.cabin, pclass = f.pclass)
library("randomForest")
set.seed(123)
fit_rf <- randomForest(factor(survived) ~ age + sex + embarked + family + cabin + pclass, data = new_train)
rf.fitted = predict(fit_rf)
ans_rf = rep(NA, 891)
for (i in 1:891) {
    ans_rf[i] = as.integer(rf.fitted[[i]]) - 1
}
mean(ans_rf == train$Survived)
table(ans_rf)
a = sum(ans_rf == 1 & f.survived == 1)
b = sum(ans_rf == 1 & f.survived == 0)
c = sum(ans_rf == 0 & f.survived == 1)
d = sum(ans_rf == 0 & f.survived == 0)
data.frame(a, b, c, d)
test_data_set <- data.frame(survived = t.survived, age = t.age, sex = t.sex, embarked = t.embarked, family = t.family, cabin = t.cabin, pclass = t.pclass)
levels(test_data_set$survived) <- levels(new_train$survived)
levels(test_data_set$age) <- levels(new_train$age)
levels(test_data_set$sex) <- levels(new_train$sex)
levels(test_data_set$embarked) <- levels(new_train$embarked)
levels(test_data_set$family) <- levels(new_train$family)
levels(test_data_set$cabin) <- levels(new_train$cabin)
levels(test_data_set$pclass) <- levels(new_train$pclass)
rf_predict = predict(fit_rf, newdata = test_data_set)
ans_rf_predict = rep(NA, 418)
for (i in 1:418) {
    ans_rf_predict[i] = as.integer(rf_predict[[i]]) - 1
}
table(ans_rf_predict)
endResult <- data.frame(PassengerId = test$PassengerId, Survived = ans_rf_predict)
write.csv(endResult, file = "SurvivingTheTitanicResult.csv", row.names = F)
