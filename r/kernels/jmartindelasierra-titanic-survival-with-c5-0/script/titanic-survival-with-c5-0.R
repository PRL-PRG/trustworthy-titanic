## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(C50)


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Training data
train <- read.csv(file="../input/train.csv")

# Test data
test <- read.csv(file="../input/test.csv")


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Add 'Survived' feature to test data
test$Survived <- NA

# Join training and test data
all <- rbind(train, test)

# Variable classes
all$Survived <- as.factor(all$Survived)
all$Name <- as.character(all$Name)
all$Pclass <- as.factor(all$Pclass)
all$Age <- as.numeric(as.character(all$Age))
all$Ticket <- as.character(all$Ticket)
all$Fare <- as.numeric(as.character(all$Fare))
all$Cabin <- as.character(all$Cabin)
all$Embarked <- as.factor(all$Embarked)


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 'Title' from 'Name'
all$Title <- regmatches(all$Name, regexpr("([A-z]+\\.)", all$Name))
all$Title <- gsub("\\.", "", all$Title)
all <- all %>% mutate(Title=ifelse(!(Title %in% c("Master", "Miss", "Mr", "Mrs")), "Other", Title))
all$Title <- as.factor(all$Title)

# 'Family.Size' from 'SibSp' and 'Parch'. Members in family.
all$Family.Size <- all$SibSp + all$Parch + 1

# 'Sharing.Ticket' from 'Ticket'. Number of people with same ticket.
tickets <- all %>% count(Ticket) %>% rename(Sharing.Ticket=n)
all$Sharing.Ticket <- tickets$Sharing.Ticket[match(all$Ticket, tickets$Ticket)]

# 'Fare.Person' from 'Fare' and 'Sharing.Ticket'
all$Fare.Person <- all$Fare / all$Sharing.Ticket


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Separate all data in two datasets
train <- all %>% filter(!is.na(Survived))
test <- all %>% filter(is.na(Survived))


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Despite the algorithm produces always the same model, a seed is set because different runs could change the order of the splits.
set.seed(123)

# Decision tree model
tree_model <- C5.0(train[, c("Pclass", "Sex", "Family.Size", "Title", "Sharing.Ticket", "Fare.Person")], train[, "Survived"])


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Feature importance by usage
C5imp(tree_model)

# Less important feature
less_important <- data.frame(C5imp(tree_model)) %>% tail(1)


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Feature importance by splits
C5imp(tree_model, metric="splits")


## ----warning=FALSE, fig.align="center", fig.width=15, fig.height=7---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Tree and plot
as.party.C5.0(tree_model)
plot(tree_model)

#summary(C5.0(train[, c("Pclass", "Sex", "Family.Size", "Title", "Sharing.Ticket", "Fare.Person")], train[, "Survived"]))
#0.848 training accuracy
#0.80382 public score


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Save data to a temporal variable since training set will change in each iteration
train_data <- train

# Results data frame
accuracies <- data.frame(Iteration=integer(), Train=numeric(), Val=numeric())

# Repeat the process 500 times
for (i in 1:500) {

# Train - validation split
samples <- sample(dim(train_data)[1], 682)
train <- train_data[samples,]
validation <- train_data[-samples,]

# Tree model
#tree_model <- C5.0(train[, c("Pclass", "Sex", "Family.Size", "Title", "Sharing.Ticket", "Fare.Person")], train[, "Survived"])

# Prediction for the training set
train$Prediction <- predict(tree_model, train)

# Confusion table and accuracy for the training set
conf_table <- table(Expected=train$Survived, Prediction=train$Prediction)
train_acc <- sum(diag(conf_table))/sum(conf_table)

# Prediction for the validation set
validation$Prediction <- predict(tree_model, validation)

# Confusion table and accuracy for the validation set
conf_table <- table(Expected=validation$Survived, Prediction=validation$Prediction)
val_acc <- sum(diag(conf_table))/sum(conf_table)

# Save iteration data
accuracies <- rbind(accuracies, data.frame(Iteration=i, Train=train_acc, Val=val_acc))

}


## ----warning=FALSE, fig.align="center", fig.width=9------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Validation accuracy plot
ggplot(accuracies, aes(x=Iteration)) + geom_line(aes(y=Val), size=0.5, color="gold4", alpha=0.4) + geom_hline(yintercept=mean(accuracies$Val), size=1, color="gold4", alpha=0.8) + geom_hline(yintercept=quantile(accuracies$Val)[2] + 1.5*IQR(accuracies$Val), linetype="dashed", color="gold4") + geom_hline(yintercept=quantile(accuracies$Val)[4] - 1.5*IQR(accuracies$Val), linetype="dashed", color="gold4") + theme_classic() + ggtitle("Cross-validation") + ylab("Accuracy") + scale_y_continuous(breaks=seq(0.7, 1, by=0.01), limits=c(0.75, 0.95))


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Survival prediction on the test dataset
test$Survived <- predict(tree_model, test)

# Submission
submission <- test %>% select(PassengerId, Survived) %>% arrange(PassengerId)
write.csv(submission, file="submission.csv", row.names=FALSE, quote=FALSE)

