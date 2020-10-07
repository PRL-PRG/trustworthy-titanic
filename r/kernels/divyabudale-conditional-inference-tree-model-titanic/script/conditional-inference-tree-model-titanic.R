train <- read.csv("../input/train.csv", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", header = TRUE, stringsAsFactors = FALSE)
library(dplyr)
comb <- bind_rows(train, test)
comb$Title <- gsub("(.*,)|(\\..*)", "", comb$Name)
table(comb$Sex, comb$Title)
f <- c("Dona", "Lady", "Mlle", "Mme", "Ms", "the Countess")
m <- c("Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir")
library(stringr)
comb$Title <- ifelse(str_trim(comb$Title) %in% f, "Miss", str_trim(comb$Title))
comb$Title <- ifelse(str_trim(comb$Title) %in% m, "Mr", str_trim(comb$Title))
comb[comb$Title == "Dr" & comb$Sex == "female", ]$Title <- "Miss"
comb[comb$Title == "Dr" & comb$Sex == "male", ]$Title <- "Mr"
comb$Title <- factor(comb$Title)
comb$Cabin <- substr(comb$Cabin, 1, 1)
table(comb$Cabin, comb$Pclass)
comb[comb$Cabin == "T", ]$Cabin <- "C"
comb$Cabin <- factor(ifelse(comb$Cabin == "", "O", comb$Cabin))
table(comb$Embarked)
comb$Embarked <- as.character(comb$Embarked)
comb$Embarked <- factor(ifelse(comb$Embarked == "", "S", comb$Embarked))
comb[is.na(comb$Fare), ]
comb[is.na(comb$Fare), ]$Fare <- mean(comb[comb$Pclass == 3 & comb$Cabin == "O", ]$Fare, na.rm = TRUE)
comb <- comb[order(comb$Ticket), ]
comb$GroupNum <- factor(match(comb$Ticket, unique(comb$Ticket)))
x <- group_by(comb, GroupNum)
y <- summarise(x, GroupSize = n())
y <- as.data.frame(y)
comb <- merge(x = comb, y = y, by = "GroupNum", all = TRUE)
comb$GroupNum <- NULL
comb$GroupSize <- factor(comb$GroupSize)
comb$FamilySize <- factor(comb$SibSp + comb$Parch + 1)
comb$Pclass <- factor(comb$Pclass)
comb$Sex <- factor(comb$Sex)
comb$Survived <- factor(comb$Survived)
comb$PassengerId <- factor(comb$PassengerId)
myformula_age <- Age ~ Pclass + Sex + Fare + Cabin + Embarked + Title
model_age <- step(lm(myformula_age, data = comb), direction = "backward")
comb$Age[is.na(comb$Age)] <- predict(model_age, comb[is.na(comb$Age), ])
summary(comb$Age)
comb1 <- comb[order(comb$PassengerId), ]
set.seed(1234)
ind <- sample(2, nrow(comb1), replace = TRUE, prob = c(0.7, 0.3))
train.data <- comb1[1:891, ]
test.data <- comb1[892:1309, ]
library(party)
myformula <- Survived ~ Pclass + Sex + Age + Fare + Cabin + Embarked + Title + GroupSize + FamilySize
fulltree <- ctree(myformula, data = train.data)
plot(fulltree, gp = gpar(fontsize = 10))
Survived <- predict(fulltree, test.data)
PassengerId <- test.data$PassengerId
result <- data.frame(PassengerId, Survived)
write.csv(result, file = "my_solution.csv", row.names = FALSE)
