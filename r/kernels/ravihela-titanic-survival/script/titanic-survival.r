
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

#read data
library(tidyverse)
trainT <-
  read_csv(file = "../input/train.csv")
testT <-
  read_csv(file = "../input/test.csv")
glimpse(trainT)
glimpse(testT)
testT$Survived <- NA
testT$Survived <- as.integer(testT$Survived)
t <- testT[, c(1, 12, 2:11)]
testT <- t
trainT$set <- "training"
testT$set <- "test"
cmbT_new <- rbind(trainT, testT)
glimpse(cmbT_new)
summary(cmbT_new)

cmbT_new$Sex <- as.factor(cmbT_new$Sex)
cmbT_new$Embarked <- as.factor(cmbT_new$Embarked)
cmbT_new$Survived <- as.factor(cmbT_new$Survived)
cmbT_new %>%
  filter(set == "training") %>%
  select(Survived) %>% table() %>%
  addmargins()

cmbT_new %>%
  filter(set == "training") %>%
  select(Survived) %>% table() %>%
  prop.table() %>%
  addmargins()

summary(cmbT_new)

cabinIntial <- ((str_extract(cmbT_new$Cabin, "^.")))
cmbT_new$CabinInitial <- cabinIntial
cmbT_new$CabinInitial[which(is.na(cmbT_new$CabinInitial))] <- "NoCabin"
cmbT_new$CabinInitial <- as.factor(cmbT_new$CabinInitial)

cmbT_new %>% ggplot(aes(x=Embarked, fill=as.factor(Pclass))) + geom_bar(position= "fill")
cmbT_new %>%  ggplot(aes(x=Embarked, fill=CabinInitial)) + geom_bar(position= "fill")
cmbT_new$Embarked[which(is.na(cmbT_new$Embarked))] <- "C"
cmbT_new$Title <- str_extract(cmbT_new$Name, "[A-Z][a-z]+\\.")
NameMedAge <-
  cmbT_new %>% filter(!is.na(Age)) %>% group_by(Title) %>% summarise(med = median(Age))

plugMedAge <- function(x, NameMedAge) {
  for (i in 1:nrow(x)) {
    if (is.na(x$Age[i])) {
      x$Age[i] <-
        as.numeric(NameMedAge[which(NameMedAge$Title == x$Title[i]), 2])
    }
  }
  return(x)
}
cmbT_new <- plugMedAge(cmbT_new, NameMedAge)
cmbT_new$FamSize <- cmbT_new$SibSp + cmbT_new$Parch + 1

plugTitleType <- function(x) {
  if (x %in% c("Capt.", "Col.", "Major.")) {
    return("officer")
  } else if (x %in% c("Countess.",
                      "Don.",
                      "Dona.",
                      "Jonkheer.",
                      "Lady.",
                      "Mlle.",
                      "Mme.",
                      "Sir.")) {
    return("Royalty")
  } else if (x %in% c("Dr.")) {
    return("Professional")
  } else if (x %in% c("Master.")) {
    return("boy")
  } else if (x %in% c("Rev.")) {
    return("Priest")
  } else if (x %in% c("Miss.")) {
    return("Comm_YoungWomen")
  } else{
    return("Commoner")
  }
}

cmbT_new$TType <- as.factor(sapply(cmbT_new$Title, plugTitleType))

FamSizeBucket <- function(x) {
  if (x <= 2) {
    return("small")
  }
  else if (x <= 4) {
    return("mid")
  }
  else if (x <= 8) {
    return("big")
  }
  else{
    return("vbig")
  }
}
cmbT_new$FamSizeCat <- sapply((cmbT_new$Parch + cmbT_new$SibSp + 1), FamSizeBucket)
cmbT_new$alone <- sapply(cmbT_new$FamSize, function(x){if(x==1){1}else{0}})

df_fare_evaluation <- cmbT_new %>% filter(cmbT_new$Fare < boxplot.stats(cmbT_new$Fare)$stats[4] & is.na(cmbT_new$Fare) == FALSE )

#df_fare_evaluation_train <- sample_frac(df_fare_evaluation, 0.7 )
#df_fare_evaluation_test <- df_fare_evaluation %>% filter(!(PassengerId %in% df_fare_evaluation_train$PassengerId))
#names(df_fare_evaluation)

fare_model <- lm(Fare~ Pclass + Sex + Age + FamSize + Embarked +TType, data = df_fare_evaluation)
cmbT_new$Fare[which(is.na(cmbT_new$Fare))] <- predict(fare_model, cmbT_new[ which(is.na(cmbT_new$Fare)), ])
cmbT_new$FareCat <- cut(cmbT_new$Fare, c(boxplot.stats(cmbT_new$Fare)$stats, max(cmbT_new$Fare)))
cmbT_new$FareCat[which(is.na(cmbT_new$FareCat))] <- "(0,7.9]"
summary(cmbT_new$FareCat)
cmbT_new$Cabin[(which(is.na(cmbT_new$Cabin)))] <- "No Cabin"

cmbT_new %>% ggplot(aes(y=Survived, x = Age, color = Sex)) + geom_jitter(aes(size = Fare, alpha = .00005)) + facet_grid(Pclass ~ Embarked)

#discarding field tht obviouly doesnt cotribute to prediction/outocome (Name, Ticket)
tit <- cmbT_new[, -c(4, 9, 15)]
x <- data.frame(sapply(tit[which(sapply(tit, is.character))], as.factor))
y <- tit[which(!sapply(tit, is.character))]
tit <- cbind(y, x)
tit$Cabin[(which(is.na(tit$Cabin)))] <- "No Cabin"
tit_train <- tit[which(tit$set == "training"), ]
tit_train$set <- NULL
tit_train$PassengerId <- NULL
tit_train$Cabin <- NULL


tit_test <- tit[which(tit$set == "test"), ]
tit_test$set <- NULL


null_model <- glm(Survived ~ 1 , data = tit_train, family = "binomial")
full_model <- glm(Survived ~ . , data = tit_train, family = "binomial")

#cmbT$Cabin <- as.factor(cmbT$Cabin)
#cmbT$Ticket <- as.factor(cmbT$Ticket)
#cmbT$Name <- as.factor(cmbT$Name)

step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

step_prob <- predict(step_model, type = "response")

library(pROC)
ROC <- roc(tit_train$Survived, step_prob)
plot(ROC, col = "red")
auc(ROC)

#confuion matrix on Training set
indexTrain <- which((predict(step_model, tit_train, type = "response") < 0.55))
SurvivedTrain <- vector(mode="integer", length=nrow(tit_train))
SurvivedTrain[indexTrain] <- 0
SurvivedTrain[-indexTrain] <- 1

confusionMatrix(as.factor(SurvivedTrain), as.factor(tit_train$Survived))


index <- which((predict(step_model, tit_test, type = "response") < 0.6))
Survived <- vector(mode="numeric", length=nrow(tit_test))
Survived[index] <- 0
Survived[-index] <- 1

titLogiSub <- cbind(PassengerId = tit_test$PassengerId, Survived)

write.csv(titLogiSub, "titLogiSub.csv", row.names = FALSE)






