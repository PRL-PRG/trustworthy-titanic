

library(ggplot2)
library(randomForest)
library(dplyr)
library(mice)

set.seed(1)

train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
train$split <- "train"

test  <- read.csv("../input/test.csv",  stringsAsFactors=FALSE)
test$split <- "test"

full <- bind_rows(train,test)

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
full$Title <- ifelse(full$Title %in% c("Miss","Mlle","Mme"),"Miss",
              ifelse(full$Title %in% c("Mrs","Mme","Mme"),"Mrs",
              ifelse(full$Title %in% c("Mr"),"Mr",
              ifelse(full$Title %in% c("Master"),"Master",
                     "Other"))))


full$Surname <- gsub(',(.*)','',full$Name)
full$FamSize <- full$SibSp + full$Parch + 1

full.survival <- subset(full,select=c("PassengerId","Survived"))
full <- full[,!names(full) %in% c("Cabin","Ticket","Name")]


# Make variables factors into factors
factor_vars <- c('Pclass','Sex','Embarked',
                 'Title','FamSize')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

imp.rf <- mice(full,method=c("",
                          "",
                          "",
                          "",
                          "rf",
                          "",
                          "",
                          "rf",
                          "",
                          "",
                          "",
                          "",
                          "" ))

imp.rf.1 <- complete(imp.rf)

rf.train <- imp.rf.1[imp.rf.1$split %in% "train",]
rf.test <- imp.rf.1[imp.rf.1$split %in% "test",]

rf <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                                      Fare + Embarked + Title + FamSize,
                   data=rf.train, ntree=100, importance=TRUE)


submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, rf.test)
write.csv(submission, file = "1_random_forest_r_submission.csv", row.names=FALSE)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
     geom_bar(stat="identity", fill="#53cfff") +
     coord_flip() + 
     theme_light(base_size=20) +
     xlab("") +
     ylab("Importance") + 
     ggtitle("Random Forest Feature Importance\n") +
     theme(plot.title=element_text(size=18))


ggsave("2_feature_importance.png", p)
