suppressMessages({
  library(rpart)
  #library(rattle)
  library(rpart.plot)
  library(RColorBrewer)
  library(Amelia)
  library(prediction)
  library(plyr)
  library(aod)
  library(ROCR)
})
```

### Load and check data

```{r}
train <- read.csv("train_titanic.csv", header=T, na.strings=c(""))
test <- read.csv("test_titanic.csv", header=T, na.strings=c(""))
test$Survived <- 0
data <- rbind(train, test, fill = TRUE)
```

### missing value

```{r}
missmap(data, main = "NA vs. Observed")
```
```{r}
summary(data)
```
```{r}
summary(data$Age)
```
### Distribution plot
```{r}
z <- data$Age
s <- 2 * z + 4 + rnorm(length(data$Age)) 
par(mfrow = c(2, 2)) 
hist(z)
plot(s, z)
qqnorm(z)
qqline(z)
plot(1:length(z),log(z), lty = 1)
```

```{r}
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = T)
plot(density(data$Age))
```
```{r}
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

train$Embarked[is.na(train$Embarked)] <- mode(train$Embarked) 
```

```{r}
# Grab title from passenger names
data$Title <- gsub('(.*, )|(\\..*)', '', data$Name)
# Show title counts by sex
table(data$Sex, data$Title)
```
```{r}
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
data$Title[data$Title == 'Mlle']        <- 'Miss' 
data$Title[data$Title == 'Ms']          <- 'Miss'
data$Title[data$Title == 'Mme']         <- 'Mrs' 
data$Title[data$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(data$Sex, data$Title)
```

### Split into training & test sets
```{r}
train <- data[1:891,]
test <- data[892:1309,]
```

```{r}
table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived)) 
prop.table(table(train$Sex, train$Survived),1) 
```
```{r}
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

train$Child <- 0
train$Child[train$Age < 18] <- 1

table(train$Child, train$Survived)
prop.table(table(train$Child, train$Survived),1)
```
```{r}
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length) 
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)}) 

```
```{r}
summary(train$Pclass)
summary(train$Fare)
```
```{r}
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
```

```{r}
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
```

```{r}
test$Child <- 0
test$Child[test$Age < 18] <- 1
test$Fare2 <- '30+'
test$Fare2[test$Fare < 30 & test$Fare >= 20] <- '20-30'
test$Fare2[test$Fare < 20 & test$Fare >= 10] <- '10-20'
test$Fare2[test$Fare < 10] <- '<10'
```



### logit
```{r}
model <- glm(Survived ~ Title + Child + Sex + Pclass + Fare2 + Embarked, family=binomial(link='logit'), data=train)

summary(model)

```
```{r}
anova(model, test="Chisq")
```
## Predict
```{r}
fitted.results <- predict(model, newdata = subset(test, select = c('Title','Child', 'Sex', 'Pclass', 'Fare2', 'Embarked')), type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))
```
```{r}
p <- predict(model, newdata = subset(test), type="response")
pr <- prediction(fitted.results, test$Survived)
perf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf)

```

```{r}
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
```

```{r}
#submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
#write.csv(submit, file = "titanic_logit.csv", row.names = FALSE)

```
