train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
str(train)
train[is.na(train)] <- 1000
str(train)
train[train == "female"] <- 0
train[train == "male"] <- 1
train[train == "S"] <- 0
train[train == "Q"] <- 1
train[train == "C"] <- 2
train[["Sex"]] <- as.numeric(train[["Sex"]])
train[["Embarked"]] <- as.numeric(train[["Embarked"]])
train[["Pclass"]] <- as.numeric(train[["Pclass"]])
train[["SibSp"]] <- as.numeric(train[["SibSp"]])
train[["Parch"]] <- as.numeric(train[["Parch"]])
train[is.na(train)] <- 0
corvec <- cor(train[["Survived"]], train[sapply(train, is.numeric)])
corvec <- corvec^2
str(corvec)
train[["Age"]] <- scale(train[["Age"]])
train[["Fare"]] <- scale(train[["Fare"]])
train[["Embarked"]] <- scale(train[["Embarked"]])
train[["Pclass"]] <- scale(train[["Pclass"]])
train[["SibSp"]] <- scale(train[["SibSp"]])
train[["Parch"]] <- scale(train[["Parch"]])
train[["Sex"]] <- scale(train[["Sex"]])
test[is.na(test)] <- 1000
str(test)
test[test == "female"] <- 0
test[test == "male"] <- 1
test[test == "S"] <- 0
test[test == "Q"] <- 1
test[test == "C"] <- 2
test[["Sex"]] <- as.numeric(test[["Sex"]])
test[["Embarked"]] <- as.numeric(test[["Embarked"]])
test[["Pclass"]] <- as.numeric(test[["Pclass"]])
test[["SibSp"]] <- as.numeric(test[["SibSp"]])
test[["Parch"]] <- as.numeric(test[["Parch"]])
test[is.na(test)] <- 0
test[["Age"]] <- scale(test[["Age"]])
test[["Fare"]] <- scale(test[["Fare"]])
test[["Embarked"]] <- scale(test[["Embarked"]])
test[["Pclass"]] <- scale(test[["Pclass"]])
test[["SibSp"]] <- scale(test[["SibSp"]])
test[["Parch"]] <- scale(test[["Parch"]])
test[["Sex"]] <- scale(test[["Sex"]])
corsquaredist <- function(x, y) {
    PclassComp <- corvec[3] * (as.vector(x[["Pclass"]]) - as.vector(y[["Pclass"]]))^2
    SexComp <- corvec[4] * (as.vector(x[["Sex"]]) - as.vector(y[["Sex"]]))^2
    AgeComp <- corvec[5] * (as.vector(x[["Age"]]) - as.vector(y[["Age"]]))^2
    SibSpComp <- corvec[6] * (as.vector(x[["SibSp"]]) - as.vector(y[["SibSp"]]))^2
    ParchComp <- corvec[7] * (as.vector(x[["Parch"]]) - as.vector(y[["Parch"]]))^2
    FareComp <- corvec[8] * (as.vector(x[["Fare"]]) - as.vector(y[["Fare"]]))^2
    EmbarkedComp <- corvec[9] * (as.vector(x[["Embarked"]]) - as.vector(y[["Embarked"]]))^2
    return(PclassComp + SexComp + AgeComp + SibSpComp + ParchComp + FareComp + EmbarkedComp)
}
survivalguess <- function(x, y) {
    templist <- corsquaredist(x, y)
    tempreturn <- order(templist)
    SurvivalFitness <- y[tempreturn[1], 2] + y[tempreturn[2], 2] + y[tempreturn[3], 2]
    if (SurvivalFitness < 2) {
        tempanswer <- 0
    }
    else {
        tempanswer <- 1
    }
    return(tempanswer)
}
TotalSurvival <- function(x) {
    output <- matrix(ncol = 2, nrow = nrow(x))
    for (i in 1:nrow(x)) {
        output[i, 1] = x[i, 1]
        output[i, 2] = survivalguess(x[i, ], train)
    }
    colnames(output) <- c("PassengerId", "Survived")
    return(output)
}
answer <- TotalSurvival(test)
str(answer)
write.csv(answer, file = "Rushton_Solution.csv", row.names = F)
