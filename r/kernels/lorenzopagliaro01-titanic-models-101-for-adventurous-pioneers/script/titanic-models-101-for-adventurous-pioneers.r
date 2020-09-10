
rm(list=ls())
#not all libraries will be used. 

library(plyr)
library(DescTools)
library(dplyr)
library(ggplot2)
library(aod)
library(ROCR)
library(pROC)
library(dplyr)
library(readxl)
library(caret)
library(MASS)
library(Rcpp)
library(sand)
library(igraph)
library(ppcor)
library(dplyr)
library(ggplot2)
library(corrplot)
library(xts)
library(DescTools)
library(dplyr)
library(aod)
library(ROCR)
library(pROC)
library(readxl)
library(caret)
library(MASS)
library(PerformanceAnalytics)
library(lmtest)
library(digest)
library(scales)
library(tm)
library(SnowballC)
library(wordcloud)
library(tidyverse)
library(ggpubr)
library(reshape2)
library(lubridate)
library(anytime)
library(ggfortify)
library(data.table)
library(gridExtra)


set.seed(123)

data <- train
#make a complete dataset
data <- data[complete.cases(data),]

perc <- floor((nrow(data)/4)*3)       
data <- data[sample(nrow(data)), ]          
data.train <- data[1:perc, ]              
data.test <- data[(perc+1):nrow(data), ] 

log1 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = data.train)
step <- stepAIC(log1, direction="both")
step$anova

# step$anova
#Stepwise Model Path 
#Analysis of Deviance Table

#Initial Model:
#Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked

#Final Model:
#Survived ~ Pclass + Sex + Age + SibSp


#        Step Df     Deviance Resid. Df Resid. Dev      AIC
#1                                  525   75.38998 491.8823
#2    - Parch  1 0.0002069457       526   75.39019 489.8838
#3     - Fare  1 0.0514154893       527   75.44160 488.2485
#4 - Embarked  3 0.8427454650       530   76.28435 488.1918


log2 <- glm(Survived ~ Pclass + Sex + Age + SibSp, data = data.train)
fitted.results <- predict(log2, newdata = data.test, type = "response")
data.test$prob <- fitted.results

confusionMatrix(round(data.test$prob), data.test$Survived)

Confusion matrix output and observations

> Confusion Matrix and Statistics
> 
>           Reference
> Prediction  0  1
>          0 85 22
>          1 20 52
>                                           
>                Accuracy : 0.7654          
>                  95% CI : (0.6964, 0.8254)
>     No Information Rate : 0.5866          
>     P-Value [Acc > NIR] : 3.876e-07       
>                                           
>                   Kappa : 0.5143          
>  Mcnemar's Test P-Value : 0.8774          
>                                           
>             Sensitivity : 0.8095          
>             Specificity : 0.7027          
>          Pos Pred Value : 0.7944          
>          Neg Pred Value : 0.7222          
>              Prevalence : 0.5866          
>          Detection Rate : 0.4749          
>    Detection Prevalence : 0.5978          
>       Balanced Accuracy : 0.7561          
>                                           
>        'Positive' Class : 0 

Dropped variables:  Parch, Fare, Embarked.

Not bad for a first attempt!

#copy paste libraries I usually use when dealing with Tree models
library(readr)
library(readxl)
library(DescTools)
library(rpart)
library(partykit)
library(evtree)
library(rattle)
library(rpart.plot)
library(caret)
library(RColorBrewer)
library(ggplot2)
library(aod)
library(ROCR)
library(pROC)
library(dplyr)
library(randomForest)

fit.cart <- train(Survived ~ Pclass + Sex + Age + SibSp, data = data.train, method="rpart")
fit.rf <- train(Survived ~ Pclass + Sex + Age + SibSp, data = data.train, method="rf")

results <- resamples(list(cart=fit.cart, rf=fit.rf))
summary(results)
dotplot(results)

predictions_cart <- predict(fit.cart, newdata = data.test)
confusionMatrix(round(predictions_cart), data.test$Survived)

> Confusion Matrix and Statistics
> 
>           Reference
> Prediction   0   1
>          0 103  34
>          1   2  40
>                                          
>                Accuracy : 0.7989         
>                  95% CI : (0.7326, 0.855)
>     No Information Rate : 0.5866         
>     P-Value [Acc > NIR] : 1.377e-09      
>                                          
>                   Kappa : 0.5571         
>  Mcnemar's Test P-Value : 2.383e-07      
>                                          
>             Sensitivity : 0.9810         
>             Specificity : 0.5405         
>          Pos Pred Value : 0.7518         
>          Neg Pred Value : 0.9524         
>              Prevalence : 0.5866         
>          Detection Rate : 0.5754         
>    Detection Prevalence : 0.7654         
>       Balanced Accuracy : 0.7607         
>                                          
>        'Positive' Class : 0    

predictions_rf <- predict(fit.rf, newdata = data.test)
confusionMatrix(round(predictions_rf), data.test$Survived)

> Confusion Matrix and Statistics
> 
>           Reference
> Prediction  0  1
>          0 95 20
>          1 10 54
>                                          
>                Accuracy : 0.8324         
>                  95% CI : (0.7695, 0.884)
>     No Information Rate : 0.5866         
>     P-Value [Acc > NIR] : 1.442e-12      
>                                          
>                   Kappa : 0.6474         
>  Mcnemar's Test P-Value : 0.1003         
>                                          
>             Sensitivity : 0.9048         
>             Specificity : 0.7297         
>          Pos Pred Value : 0.8261         
>          Neg Pred Value : 0.8438         
>              Prevalence : 0.5866         
>          Detection Rate : 0.5307         
>    Detection Prevalence : 0.6425         
>       Balanced Accuracy : 0.8172         
>                                          
>        'Positive' Class : 0  

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
test2 <- ddply(test, ~ Sex, transform, Age = impute.mean(Age)) 

predictions_rf2 <- round(predict(fit.rf, newdata = test2))
my_submission <- data.frame(PassengerId = test2$PassengerId)
my_submission$Survived <- predictions_rf2
write.csv(my_submission, file = "titanic_submission_rf1.csv", row.names=FALSE)

data2 <- ddply(train, ~ Sex, transform, Age = impute.mean(Age))
data2[!complete.cases(data2),]

data2$Fare <- ifelse(data2$Fare > 50, 1, 0)
data2$Alone <- ifelse(data2$SibSp == 0, 1,0)

perc <- floor((nrow(data2)/4)*3)       
data2 <- data2[sample(nrow(data2)), ]          
data2.train <- data2[1:perc, ]              
data2.test <- data2[(perc+1):nrow(data2), ] 

log21 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Alone, data = data2.train)
step <- stepAIC(log21, direction="both")
step$anova

> Stepwise Model Path 
> Analysis of Deviance Table
> 
> Initial Model:
> Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + 
>     Alone
> 
> Final Model:
> Survived ~ Pclass + Sex + Age + SibSp + Fare + Alone
> 
> 
>         Step Df   Deviance Resid. Df Resid. Dev      AIC
> 1                                657   88.45895 569.1731
> 2 - Embarked  3 0.37875663       660   88.83770 566.0272
> 3    - Parch  1 0.08241704       661   88.92012 564.6467

log22 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Alone + , data = data2.train)
fitted.results <- predict(log2, newdata = data2.test, type = "response")
data2.test$prob <- fitted.results

confusionMatrix(round(data2.test$prob), data2.test$Survived)

> Confusion Matrix and Statistics
> 
>           Reference
> Prediction   0   1
>          0 121  35
>          1  16  51
>                                           
>                Accuracy : 0.7713          
>                  95% CI : (0.7105, 0.8247)
>     No Information Rate : 0.6143          
>     P-Value [Acc > NIR] : 4.454e-07       
>                                           
>                   Kappa : 0.4967          
>  Mcnemar's Test P-Value : 0.01172         
>                                           
>             Sensitivity : 0.8832          
>             Specificity : 0.5930          
>          Pos Pred Value : 0.7756          
>          Neg Pred Value : 0.7612          
>              Prevalence : 0.6143          
>          Detection Rate : 0.5426          
>    Detection Prevalence : 0.6996          
>       Balanced Accuracy : 0.7381          
>                                           
>        'Positive' Class : 0     

fit.cart <- train(Survived ~ Pclass + Sex + Age + SibSp + Alone + Fare, data = data2.train, method="rpart")
fit.rf <- train(Survived ~ Pclass + Sex + Age + SibSp + Alone + Fare, data = data2.train, method="rf")

results <- resamples(list(cart=fit.cart, rf=fit.rf))
summary(results)
dotplot(results)

predictions_cart <- predict(fit.cart, newdata = data2.test)
confusionMatrix(round(predictions_cart), data2.test$Survived)

> Confusion Matrix and Statistics
> 
>           Reference
> Prediction   0   1
>          0 135  54
>          1   2  32
>                                           
>                Accuracy : 0.7489          
>                  95% CI : (0.6866, 0.8044)
>     No Information Rate : 0.6143          
>     P-Value [Acc > NIR] : 1.507e-05       
>                                           
>                   Kappa : 0.4028          
>  Mcnemar's Test P-Value : 9.416e-12       
>                                           
>             Sensitivity : 0.9854          
>             Specificity : 0.3721          
>          Pos Pred Value : 0.7143          
>          Neg Pred Value : 0.9412          
>              Prevalence : 0.6143          
>          Detection Rate : 0.6054          
>    Detection Prevalence : 0.8475          
>       Balanced Accuracy : 0.6787          
>                                           
>        'Positive' Class : 0  

predictions_rf <- predict(fit.rf, newdata = data2.test)
confusionMatrix(round(predictions_rf), data2.test$Survived)

> Confusion Matrix and Statistics
> 
>           Reference
> Prediction   0   1
>          0 119  32
>          1  18  54
>                                           
>                Accuracy : 0.7758          
>                  95% CI : (0.7153, 0.8288)
>     No Information Rate : 0.6143          
>     P-Value [Acc > NIR] : 2.049e-07       
>                                           
>                   Kappa : 0.512           
>  Mcnemar's Test P-Value : 0.06599         
>                                           
>             Sensitivity : 0.8686          
>             Specificity : 0.6279          
>          Pos Pred Value : 0.7881          
>          Neg Pred Value : 0.7500          
>              Prevalence : 0.6143          
>          Detection Rate : 0.5336          
>    Detection Prevalence : 0.6771          
>       Balanced Accuracy : 0.7483          
>                                           
>        'Positive' Class : 0    

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
test22 <- ddply(test, ~ Sex, transform, Age = impute.mean(Age))
test22$Fare <- ifelse(test22$Fare > 50, 1, 0)
test22$Alone <- ifelse(test22$SibSp == 0, 1,0)

#incomplete dataset! Storey, Mr. Thomas, Fare NA. Given he is a 3rd class, he surely was in category 0
test22$Fare[is.na(test22$Fare)] <- 0


#file submission RFv2
predictions_rf22 <- round(predict(fit.rf, newdata = test22))
my_submission2 <- data.frame(PassengerId = test22$PassengerId)
my_submission2$Survived <- predictions_rf22
write.csv(my_submission2, file = "titanic_submission_rf22.csv", row.names=FALSE)

#file submission CARTv2
predictions_cart22 <- round(predict(fit.cart, newdata = test22))
my_submission3 <- data.frame(PassengerId = test22$PassengerId)
my_submission3$Survived <- predictions_cart22
write.csv(my_submission3, file = "titanic_submission_cart22.csv", row.names=FALSE)

fit.rf <- train(Survived ~ Pclass + Sex + Age + SibSp + Alone, data = data2.train, method="rf")

predictions_rf <- predict(fit.rf, newdata = data2.test)
confusionMatrix(round(predictions_rf), data2.test$Survived)

> Confusion Matrix and Statistics
> 
>           Reference
> Prediction   0   1
>          0 124  34
>          1  13  52
>                                           
>                Accuracy : 0.7892          
>                  95% CI : (0.7298, 0.8408)
>     No Information Rate : 0.6143          
>     P-Value [Acc > NIR] : 1.714e-08       
>                                           
>                   Kappa : 0.534           
>  Mcnemar's Test P-Value : 0.003531        
>                                           
>             Sensitivity : 0.9051          
>             Specificity : 0.6047          
>          Pos Pred Value : 0.7848          
>          Neg Pred Value : 0.8000          
>              Prevalence : 0.6143          
>          Detection Rate : 0.5561          
>    Detection Prevalence : 0.7085          
>       Balanced Accuracy : 0.7549          
>                                           
>        'Positive' Class : 0               
                              
