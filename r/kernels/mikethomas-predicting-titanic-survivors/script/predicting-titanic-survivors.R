
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
suppressMessages(library(randomForest))
suppressMessages(library(ggplot2))
suppressMessages(library(wordcloud))
suppressMessages(library(tm))

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
#summary(train)

#ggplot(train,aes(x=Survived, y=Embarked))+geom_jitter(width=0.025)+aes(colour=train$Age)
#ggplot(train,aes(x=Survived, y=Pclass))+geom_jitter(width=0.025)
#ggplot(train,aes(x=Survived, y=Sex))+geom_jitter(width=0.025)+aes(colour=train$Pclass)
#ggplot(train,aes(x=Survived, y=Age))+geom_jitter(width=0.025)+aes(colour=train$Embarked)
#ggplot(train,aes(x=Survived, y=Fare))+geom_jitter(width=0.025)

#nameCorpus <- Corpus(VectorSource(train$Name))
#nameCorpus <- tm_map(nameCorpus, content_transformer(tolower))
#myStopWords <- c(stopwords('english'),"miss","mrs","mr","dr","master","rev","major","col","mme","mlle","ms","don","capt","countess","jonkheer")
#nameCorpus <- tm_map(nameCorpus, removeWords, myStopWords)
#nameCorpus <- tm_map(nameCorpus, removePunctuation)
#wordcloud(nameCorpus)

train$Embarked <- as.character(train$Embarked)
train$Embarked[train$Embarked==""] <- "X"
train$Embarked <- as.factor(train$Embarked)

train$Cabin <- as.character(train$Cabin)
train$Cabin[train$Cabin==""] <- "X000"
train$Cabin <- as.factor(train$Cabin)

train$Status <- as.factor(tolower(substr(train$Name,regexpr(",",train$Name)+2,regexpr("\\.",train$Name)-1)))
train$CabinList <- strsplit(as.vector(train$Cabin), " ")
train$HasCabin <- train$Cabin!=""
train$CabinCount <- do.call(rbind, lapply(train$CabinList, length))

train$TicketLetter <- as.factor(gsub("[^[:alpha:]]", "", tolower(train$Ticket)))
train$TicketNumber <- as.factor(gsub("\\D", "", tolower(train$Ticket)))

A_count <- setNames(aggregate(x=train$Age, by=list(train$Pclass, train$Sex, train$Status), length),c("Pclass","Sex","Status","StatusCount"))
A_averages <- setNames(aggregate(x=train$Age, by=list(train$Pclass, train$Sex, train$Status), mean, na.rm=TRUE),c("Pclass","Sex","Status","StatusAverage"))
B_count <- setNames(aggregate(x=train$Age, by=list(train$Pclass, train$Sex), length),c("Pclass","Sex","SexCount"))
B_averages <- setNames(aggregate(x=train$Age, by=list(train$Pclass, train$Sex), mean, na.rm=TRUE),c("Pclass","Sex","SexAverage"))
C_count <- setNames(aggregate(x=train$Age, by=list(train$Status, train$Sex), length),c("Status","Sex","NoClassCount"))
C_averages <- setNames(aggregate(x=train$Age, by=list(train$Status, train$Sex), mean, na.rm=TRUE),c("Status","Sex","NoClassAverage"))

A_impute <- merge(merge(merge(merge(merge(A_count,A_averages,by.x=c("Pclass","Status","Sex"),by.y=c("Pclass","Status","Sex")),B_count,by.x=c("Pclass","Sex"),by.y=c("Pclass","Sex")),B_averages,by.x=c("Pclass","Sex"),by.y=c("Pclass","Sex")),C_count,by.x=c("Status","Sex"),by.y=c("Status","Sex")),C_averages,by.x=c("Status","Sex"),by.y=c("Status","Sex"))
A_impute$imputeMin <- with(A_impute, pmin(ifelse(A_impute$StatusCount>9,A_impute$StatusCount,NA), ifelse(A_impute$SexCount>9,A_impute$SexCount,NA), ifelse(A_impute$NoClassCount>9,A_impute$NoClassCount,NA), na.rm=TRUE))
imputeLogic <- data.frame(matrix(unlist(which(sapply(A_impute[-10], function(x,y) x==y, y=A_impute$imputeMin)==TRUE, arr.ind=TRUE)),nrow=27,byrow=F),stringsAsFactors=F)
A_impute$avcol <- imputeLogic$X2 + 1
A_impute$imputeVal <- sapply(seq_along(A_impute[,1]), function(x) A_impute[x,A_impute$avcol[x]])
A_impute <- A_impute[,c(1,2,3,12)]

train <- merge(train,A_impute,by.x=c("Pclass","Status","Sex"),by.y=c("Pclass","Status","Sex"))
train$Age <- ifelse(is.na(train$Age),train$imputeVal,train$Age)

train[1:20,]
#train$CabinLetter <- gsub("\\d", "", train$CabinList)
#train$CabinNumber <- gsub("\\D", "", train$CabinList)

