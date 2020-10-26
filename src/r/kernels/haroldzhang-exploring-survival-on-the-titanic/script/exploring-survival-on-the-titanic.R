library("randomForest")
reAssembleVariable = function(trainDataList, type) {
    tmpNames = names(trainDataList)
    for (i in 1:length(tmpNames)) {
        if (tmpNames[i] == "Survived") {
            survivedList = trainDataList[i]
        }
        if (tmpNames[i] == "PassengerId") {
            passengerList = trainDataList[i]
        }
        if (tmpNames[i] == "Pclass") {
            pClassList = trainDataList[i]
        }
        if (tmpNames[i] == "Name") {
            nameList = trainDataList[i]
        }
        if (tmpNames[i] == "Sex") {
            sexList = trainDataList[i]
            maleVector = vector("double", length(sexList[, 1]))
            maleIndex = which(sexList == "male")
            maleVector[maleIndex] = 1
            maleList = list(Sex = maleVector)
        }
        if (tmpNames[i] == "Age") {
            ageList = trainDataList[i]
        }
        if (tmpNames[i] == "SibSp") {
            sibspList = trainDataList[i]
        }
        if (tmpNames[i] == "Parch") {
            parchList = trainDataList[i]
        }
        if (tmpNames[i] == "Ticket") {
            ticketList = trainDataList[i]
        }
        if (tmpNames[i] == "Fare") {
            fareList = trainDataList[i]
        }
        if (tmpNames[i] == "Cabin") {
            cabinList = trainDataList[i]
        }
        if (tmpNames[i] == "Embarked") {
            embarkedList = trainDataList[i]
            newEmbarkedList = fullfillEmbark(embarkedList)
            isCEList = vector("double", length(newEmbarkedList[, 1]))
            isQEList = vector("double", length(newEmbarkedList[, 1]))
            ceIndex = which(newEmbarkedList == "C")
            qeIndex = which(newEmbarkedList == "Q")
            isCEList[ceIndex] = 1
            isQEList[qeIndex] = 1
            isCEList = list(C = isCEList)
            isQEList = list(Q = isQEList)
        }
    }
    newAgeList = fullfillAgeData(ageList[1], nameList[1], sibspList[1], parchList[1], 7)
    newFareList = fullfillFare(fareList)
    if (type == 1) {
        newDataList = data.frame(survivedList, passengerList, pClassList, maleList, newAgeList, sibspList, parchList, newFareList, cabinList, isCEList, isQEList)
    }
    else {
        newDataList = data.frame(passengerList, pClassList, maleList, newAgeList, sibspList, parchList, newFareList, cabinList, isCEList, isQEList)
    }
    return(newDataList)
}
fullfillAgeData = function(ageList, nameList, sibspList, parchList, type) {
    noneAgeIndex = which(is.na(ageList))
    nAgeIndex = which(!is.na(ageList))
    if (type == 1) {
        noneAgeIndex = which(is.na(ageList))
        nAgeIndex = which(!is.na(ageList))
        meanAge = mean(ageList[nAgeIndex, 1])
        ageList[noneAgeIndex, 1] = meanAge
        return(ageList)
    }
    if (type == 2) {
        noneAgeIndex = which(is.na(ageList))
        nAgeIndex = which(!is.na(ageList))
        medianAge = median(ageList[nAgeIndex, 1])
        ageList[noneAgeIndex, 1] = medianAge
        return(ageList)
    }
    if (type == 3) {
        noneAgeIndex = which(is.na(ageList))
        nAgeIndex = which(!is.na(ageList))
        modeAge = which.max(table(ageList[nAgeIndex, 1]))
        modeName = as.numeric(names(modeAge))
        ageList[noneAgeIndex, 1] = modeName
        return(ageList)
    }
    if (type == 4) {
        noneAgeIndex = which(is.na(ageList))
        nAgeIndex = which(!is.na(ageList))
        nAgeList = ageList[nAgeIndex, 1]
        minAge = nAgeList[which.min(nAgeList)]
        maxAge = nAgeList[which.max(nAgeList)]
        rand = runif(length(noneAgeIndex), minAge, maxAge)
        ageList[noneAgeIndex, 1] = rand
        return(ageList)
    }
    if (type == 5) {
        noneAgeIndex = which(is.na(ageList))
        nAgeIndex = which(!is.na(ageList))
        nAgeList = ageList[nAgeIndex, 1]
        meanAge = mean(nAgeList)
        std4age = sd(nAgeList)
        rand = rnorm(length(noneAgeIndex), meanAge, std4age)
        ageList[noneAgeIndex, 1] = rand
        return(ageList)
    }
    if (type == 6) {
    }
    if (type == 7) {
        noneAgeIndex = which(is.na(ageList))
        nAgeIndex = which(!is.na(ageList))
        for (i in 1:length(noneAgeIndex)) {
            dist1 = vector()
            for (j in 1:length(nAgeIndex)) {
                ni = noneAgeIndex[i]
                nj = nAgeIndex[j]
                ma = matrix(c(sibspList[nj, 1] - sibspList[ni, 1], parchList[nj, 1] - parchList[ni, 1]), 1)
                dist1[j] = norm(ma, "2")
            }
            distIndex = order(dist1)
            count1 = 0
            count2 = 0
            targetAge = 0
            lastAge = 0
            tAgeList = vector()
            for (k in 1:20) {
                age = nAgeIndex[distIndex[k]]
                age = floor(age/5) * 5 + 2.5
                tAgeList[k] = age
            }
            modeAge = which.max(table(tAgeList))
            modeName = as.numeric(names(modeAge))
            tmpIndex = noneAgeIndex[i]
            ageList[tmpIndex, 1] = modeName
        }
        return(ageList)
    }
}
fullfillEmbark = function(embarkedList) {
    noneEmbarkAgeIndex = which(embarkedList == "")
    nEmbarkAgeIndex = which(embarkedList != "")
    modeEmbark = which.max(table(embarkedList[nEmbarkAgeIndex, 1]))
    embarkName = names(modeEmbark)
    embarkedList[noneEmbarkAgeIndex, 1] = embarkName
    return(embarkedList)
}
fullfillFare = function(fareList) {
    nonefareIndex = which(is.na(fareList))
    nfareIndex = which(!is.na(fareList))
    medianFare = median(fareList[nfareIndex, 1])
    fareList[nonefareIndex, 1] = medianFare
    return(fareList)
}
train_data = read.csv("../input/train.csv", header = T)
test_data = read.csv("../input/test.csv", header = T)
newTrainData = reAssembleVariable(train_data, 1)
newTestData = reAssembleVariable(test_data, 2)
lm1_model = lm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, newTrainData)
rf1_model = randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare, newTrainData)
lm1_ans = predict(lm1_model, newTestData)
rf1_ans = predict(rf1_model, newTestData)
passedCoune = 0
for (i in 1:length(lm1_ans)) {
    if (lm1_ans[i] >= 0.5) {
        lm1_ans[i] = 1
    }
    else {
        lm1_ans[i] = 0
    }
}
testAns = data.frame(PassengerID = newTestData$PassengerId, Survived = lm1_ans)
write.csv(testAns, "ans.csv")
