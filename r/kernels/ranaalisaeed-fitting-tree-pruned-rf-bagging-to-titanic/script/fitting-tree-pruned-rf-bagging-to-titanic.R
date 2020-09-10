## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library('dplyr') # data manipulation
library('tree') # construct classification or regression trees
library('mice') # missing data imputation
library('lattice') # used with mice to plot
library('VIM') # visualisation of missing data
library('ggplot2') # plotting
library('ggthemes') # plotting
library('scales') # plotting with scales
library('randomForest')
library('gbm')
library('caret') # confusionMatrix to calculate accuracy


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train = read.csv('../input/train.csv', header = T, stringsAsFactors = F, na.strings = "")
test = read.csv('../input/test.csv', header = T, stringsAsFactors = F, na.strings = "")
full = bind_rows(train, test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
factor_vars = c('PassengerId', 'Survived', 'Pclass', 'Sex', 'Embarked')
full[factor_vars] = lapply(full[factor_vars], function(x) as.factor(x))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(full)


## ---- warning=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missing <- function(x){ sum(is.na(x)) }
apply(full, 2, missing)


## ---- message=F, warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
md.pattern(full)
aggr_plot = aggr(full, col=c('navyblue','red'), numbers=TRUE, 
                 sortVars=TRUE, labels=names(full),
                 cex.axis=.7, gap=3, 
                 ylab=c("Histogram of missing data","Pattern"))
# marginplot(full[c(10,4)])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full[which(is.na(full$Embarked)),]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
    colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Embarked[c(62, 830)] <- 'C'


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full[which(is.na(full$Fare)),]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
  aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
    colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)


## ---- message=F, warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
mice_vars = c('Pclass','Sex','Age','SibSp','Parch','Fare','Embarked')
# try different method of available 25 methods ``methods(mice)``
# mice_mod = mice(full[, names(full) %in% mice_vars], m=5, maxit=5, method = 'pmm', seed=500); 
mice_mod = mice(full[, names(full) %in% mice_vars], method = 'pmm'); 
# summary(mice_mod)
# mice_mod$imp$Age
# mice_mod$meth
mice_output = complete(mice_mod)


## ---- echo=F, message=F, warning=F-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# xyplot(mice_mod, Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, pchisq=18, cex=1)
# densityplot(mice_mod)
stripplot(mice_mod, pch = 20, cex = 0.2)

par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', col='lightgreen', ylim=c(0,0.04))
par(mfrow=c(1,1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Age <- mice_output$Age


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train = full[1:500,]
validate = full[501:891,]
test = full[892:1309,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2)
tree_mod = tree(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train)
summary(tree_mod)


## ---- message=F, warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(tree_mod)
text(tree_mod, pretty = 0)
#train_tree


## ---- message=F, warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predictions = predict(tree_mod, validate, type = "class")
# table(predictions, validate$Survived)
confusionMatrix(predictions, validate$Survived)
# create a cross validation set


## ---- message=F, warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(3)
cv_mod = cv.tree(tree_mod, FUN = prune.misclass)
# names(cv_train)
cv_mod
par(mfrow =c(1,2))
plot(cv_mod$size, cv_mod$dev, type="b")
plot(cv_mod$k, cv_mod$dev, type="b")
par(mfrow =c(1,1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pruned_tree = prune.misclass(tree_mod, best = 6)
plot(pruned_tree)
text(pruned_tree, pretty = 0)


## ---- message=F, warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predictions = predict(pruned_tree, validate, type = "class")
confusionMatrix(predictions, validate$Survived)
## add cross validation data


## ---- message=F, warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(122)
bag_mod = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, mtry=13, importance=T)
#bag_mod
plot(bag_mod, ylim=c(0,0.36))
legend('topright', colnames(bag_mod$err.rate), col=1:3, fill=1:3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predictions = predict(bag_mod, validate, type = "class")
confusionMatrix(predictions, validate$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(111)
rf_mod = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, mtry=6, importance=T)
rf_mod
plot(rf_mod, ylim=c(0,0.36))
legend('topright', colnames(rf_mod$err.rate), col=1:3, fill=1:3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predictions = predict(rf_mod, validate, type = "class")
confusionMatrix(predictions, validate$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
importance(rf_mod)

importance    <- importance(rf_mod)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
    y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
    hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predictions <- predict(pruned_tree, test, type="class")
solution <- data.frame(PassengerID = test$PassengerId, Survived = predictions)
write.csv(solution, file = 'rana_pruned_Solution.csv', row.names = F)

