## ----setup, include=TRUE, echo=TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo=TRUE, error=FALSE)


## ----message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# general visualisation
library('ggplot2')
library('scales')
library('grid')
library('ggthemes')
library('gridExtra')
library('RColorBrewer')
library('corrplot')
library('rpart.plot')

# general data manipulation
library('mice')
library('dplyr')
library('readr')
library('data.table')
library('tibble')
library('tidyr')
library('stringr')
library('forcats')
library('rlang')
library('caret')
library('ade4')
library('rlang')

# analysis and models
library('rpart')
library('randomForest') 
library('e1071')


## ----warning=FALSE, results=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- as.tibble(fread('../input/titanic/train.csv'))
test <- as.tibble(fread('../input/titanic/test.csv'))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glimpse(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glimpse(test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
colSums(is.na(train))
colSums(is.na(test))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(train$Survived))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(train$Sex, train$Survived), margin = 1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(train$Pclass, train$Survived), margin = 1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all_dataInvestigate <- bind_rows(train, test)
# We have a missing fare for passenger 1044 which we set to the median fare
all_dataInvestigate$Fare[1044] <- median(all_dataInvestigate$Fare, na.rm = TRUE)
# Plot data
fig.4.2.a <- ggplot(all_dataInvestigate, aes(x = factor(Pclass), y = Fare)) +
  geom_boxplot() +
  theme(axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        legend.position="none") +
  labs(x = "Pclass", y = "Fare (units)", title = "Fig 4.2.a: Fare box plots for each passenger class.")
plot(fig.4.2.a)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
filtered <- all_dataInvestigate %>% filter((Pclass==2 & Fare > 30.0)) %>% arrange(desc(Fare)) %>% head(10) 
kable(filtered, caption="Second class (Pclass=2) passengers with ticket Fares greater than 30.00, ordered by fare")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all_dataInvestigate <- all_dataInvestigate %>%
  group_by(Fare, Ticket) %>%
  dplyr::mutate(GroupCount = n())
  
all_dataInvestigate <- transform(all_dataInvestigate, FarePerPerson = Fare / GroupCount)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(all_dataInvestigate, aes(x = factor(Pclass), y = FarePerPerson)) +
  geom_boxplot() +
    theme(axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        legend.position="none") +
  labs(x = "Pclass", y = "Fare per person (units)", title = "Fig 4.2.b: Fare-per-person box plots for each passenger class.")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainInvestigate <- all_dataInvestigate[1:891,]
# Pearson Correlation for Fare and Pclass
cor.test(trainInvestigate$Pclass, trainInvestigate$Fare, method = "pearson", use = "complete.obs")
# Pearson Correlation for FarePerPerson and Pclass
cor.test(trainInvestigate$Pclass, trainInvestigate$FarePerPerson, method = "pearson", use = "complete.obs")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Select fares above 0
trainInvestigate_farePerPerson <- trainInvestigate[(trainInvestigate$FarePerPerson>0.0), ]
# Group fares in 5 unit intervals
trainInvestigate_farePerPerson$fareGroup <- cut(trainInvestigate_farePerPerson$FarePerPerson, seq(0, 130, 5), include.lowest=T)
# Plot
fig.4.2.c <- ggplot(trainInvestigate_farePerPerson, aes(x = fareGroup)) + 
  geom_bar(stat='count', aes(fill = factor(Survived)), position = 'fill') +
      theme(axis.text.x = element_text(angle = 50, hjust = 1),
            axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey")) +
  labs(x = "Fare-per-person", y = "Survival rate", title = "Fig 4.2.c: Passenger survival rate for grouped fares per person.") +
  scale_fill_discrete(name  ="Survived") 
plot(fig.4.2.c)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_age <- train[(!is.na(train$Age)), ]
train_age$Age_group <- cut(train_age$Age, seq(0, 85, 5), include.lowest=T)
# Plot
fig.4.3.a <- ggplot(train_age, aes(x = Age_group)) + 
  geom_bar(stat='count', aes(fill = factor(Survived)), position = 'fill') +
        theme(axis.text.x = element_text(angle = 50, hjust = 1),
            axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey")) +
  labs(x = "Age", y = "Survival rate", title = "Fig 4.3.a: Passenger survival rate for grouped ages.") +
  scale_fill_discrete(name ="Survived") 
plot(fig.4.3.a)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainInvestigate <- train %>% dplyr::mutate(Family= ifelse(SibSp > 0 | Parch > 0, "Multiple", "Single"))
prop.table(table(trainInvestigate$Family, trainInvestigate$Survived), margin = 1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(train$Embarked, train$Survived), margin = 1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainNationalitySubset <- train[,c("PassengerId","Name")]


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## trainNationalitySubset$FullName <- lapply(as.character(trainNationalitySubset$Name), formatName)


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## # Name preperation
## formatName <- function(name) {
##   title <- gsub('(.*, )|(\\..*)', '', name)
##   nameSplit <- strsplit(name, split = '[,./]')
##   surname <- gsub(" ", "%20", nameSplit[[1]][1], fixed = TRUE)
##   firstname <- strsplit(nameSplit[[1]][3], split = '[ ]')[[1]][2]
##   if(title=='Mrs') {
##     maidenFullName <- regmatches(name, gregexpr("(?<=\\().*?(?=\\))", name, perl=T))[[1]]
##     if (length(maidenFullName)!=0) {
##       firstname <- strsplit(maidenFullName, split = "[ ]")[[1]][1]
##     }
##   }
## 
##   paste(firstname, surname, sep= "%20")
## }
## # API request and process response
## requestLocation <- function(name) {
##   url <- paste("http://www.name-prism.com/api/json/", name, sep='')
##   response <- fromJSON(url, simplifyDataFrame = FALSE)
##   nationalityProbs <- as_data_frame(response)
##   nationality <- colnames(nationalityProbs)[apply(nationalityProbs,1,which.max)]
##   nationality
## }
## # Make requests to Name Prism api
## trainNationalitySubset$Nationality <- sapply(trainInvestigateNationalitySubset$fullName, requestLocation)
## # Exclude redundant columns
## trainNationalitySubset = trainNationalitySubset[ , c("PassengerId", "Nationality")]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Import
trainNationalitySubset <- as.tibble(fread('../input/titanic-passenger-nationalities/trainNationalitySubset.csv'))
# Merge
trainInvestigateNationality <- merge(x = train, y = trainNationalitySubset, by = "PassengerId", all.x=TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(trainInvestigateNationality$Nationality, trainInvestigateNationality$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fig.4.6.a = ggplot(trainInvestigateNationality, aes(x = reorder(Nationality,Nationality, function(x)-length(x)))) +
  geom_bar(stat='count', aes(fill = factor(Survived)), position = 'stack') +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey")) +
  labs(x = "Nationality", y = "Number of Passengers", title = "Fig 4.6.a: Passenger survival by nationality ordered by largest nationality") +
  scale_fill_discrete(name  ="Survived") 

fig.4.6.b = ggplot(trainInvestigateNationality, aes(x = reorder(Nationality,Nationality, function(x)-length(x)))) +
  geom_bar(stat='count', aes(fill = factor(Survived)), position = 'fill') +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x = "Nationality", y = "Survival Rate", title = "Fig 4.6.b: Passenger survival rate by Nationality") +
  scale_fill_discrete(name  ="Survived")
plot(fig.4.6.a)
plot(fig.4.6.b)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all_data <- bind_rows(train, test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sort(table(all_data$Embarked),decreasing=TRUE)[1]
all_data$Embarked[c(62, 830)] <- "S"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
median(all_data$Fare, na.rm = TRUE)
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                       data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all_data$Name = as.factor(all_data$Name)
all_data$Sex = as.factor(all_data$Sex)
all_data$Ticket = as.factor(all_data$Ticket)
all_data$Cabin = as.factor(all_data$Cabin)
all_data$Embarked = as.factor(all_data$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# FarePerPerson
all_data <- all_data %>%
  group_by(Ticket, Fare) %>%
  dplyr::mutate(FarePerPerson = Fare / n())
# Family
all_data <- all_data %>% 
  dplyr::mutate(Family= ifelse(SibSp > 0 | Parch > 0, "Multiple", "Single"))
# Nationality
trainNationalitySubset <- as.tibble(fread('../input/titanic-passenger-nationalities/trainNationalitySubset.csv'))
testNationalitySubset <- as.tibble(fread('../input/titanic-passenger-nationalities/testNationalitySubset.csv'))
all_dataNationalitySubset <- bind_rows(trainNationalitySubset, testNationalitySubset)
all_data <- merge(x = all_data, y = all_dataNationalitySubset, by = "PassengerId", all.x=TRUE) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dummy = acm.disjonctif(all_data['Nationality'])
all_data['Nationality'] = NULL
all_data = cbind(all_data, dummy)
all_data <- all_data %>%
  mutate_at(vars(starts_with("Nationality")), funs(as.integer))
names(all_data) <- make.names(names(all_data))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all_data$Family = as.factor(all_data$Family)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all_PassengerId <- all_data %>% select(PassengerId)
all_data <- all_data %>% select(-PassengerId, -Name, -Ticket, -Fare, -Cabin)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_PassengerId <- all_PassengerId[1:891,]
test_PassengerId <- all_PassengerId[892:1309,]

train <- all_data[1:891,]
test <- all_data[892:1309,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create Folds
folds <- createFolds(train$Survived, k=5, list=TRUE, returnTrain=FALSE)
# Set up results table
results <- data.frame(PassengerId = train_PassengerId, Fold = NA, Survived = train$Survived)
for(i in 1:5){
  foldIndex <- folds[[i]]
  results$Fold[foldIndex] <- i
}


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results$Tmodel <- NA
for(i in 1:5){
  indexes <- folds[[i]]
  holdoutData <- train[indexes, ]
  trainData <- train[-indexes, ]
  
  t_model <- rpart(Survived ~ ., 
                  method = "class", 
                  data = trainData)
  predict <- predict(t_model, holdoutData, type="class")
  results$Tmodel[indexes] <- as.vector(predict)
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
confMat <- table(results$Tmodel, results$Survived)
sum(diag(confMat))/sum(confMat)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create model
t_model <- rpart(Survived ~ ., 
                  method = "class", 
                  data = train)
# Calculate variable importance
importance <- varImp(t_model)
varImportance <- data.frame(Variables = row.names(importance), Score = importance$Overall)
# Create a rank variable based on importance and filter out 0 score variables
rankImportance <- varImportance %>% filter(Score > 0) %>%
  mutate(Rank = paste0('#',dense_rank(desc(Score))))
# Use ggplot2 to visualise the relative importance of variables
fig.5.3.a <- ggplot(rankImportance, aes(x = reorder(Variables, Score), 
    y = Score, fill = Score)) +
  geom_bar(stat='identity') + 
  theme(axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        panel.grid.major.x = element_line(colour = "grey")) +
  labs(x = "Variables", y = "Scores", title = "Fig 5.3.a: Tree model variable importance scores") +
  coord_flip()
print(fig.5.3.a)
# Use rpart plot function to visualise the decision tree
rpart.plot(t_model)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results$LRmodel <- NA
for(i in 1:5){
  indexes <- folds[[i]]
  holdoutData <- train[indexes, ]
  trainData <- train[-indexes, ]
  
  lr_model <- glm(Survived ~ ., 
                  family = binomial(link='logit'), 
                  data = trainData)
  predict <- predict(lr_model, holdoutData, type="response")
  results$LRmodel[indexes] <- as.vector(ifelse(predict > 0.5,1,0))
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
confMat <- table(results$LRmodel, results$Survived)
sum(diag(confMat))/sum(confMat)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
results$RFmodel <- NA
for(i in 1:5){
  indexes <- folds[[i]]
  holdoutData <- train[indexes, ]
  trainData <- train[-indexes, ]
  
  rf_model <- randomForest(Survived ~ ., 
                  method = "class", 
                  data = trainData)
  predict <- predict(rf_model, holdoutData, type="response")
  results$RFmodel[indexes] <- as.vector(ifelse(predict > 0.5,1,0))
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
confMat <- table(results$RFmodel, results$Survived)
sum(diag(confMat))/sum(confMat)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
names(train) <- make.names(names(train))
results$SVMmodel <- NA
for(i in 1:5){
  indexes <- folds[[i]]
  holdoutData <- train[indexes, ]
  trainData <- train[-indexes, ]
  svm_model <- svm(Survived ~ ., 
                  data = trainData,
                  cost = 100, 
                  gamma = 1)
  predict <- predict(svm_model, holdoutData)
  results$SVMmodel[indexes] <- as.vector(ifelse(predict > 0.5,1,0))
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
confMat <- table(results$SVMmodel, results$Survived)
sum(diag(confMat))/sum(confMat)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rf_model <- randomForest(Survived ~ ., 
                  method = "class", 
                  data = train)
prediction <- predict(rf_model, test, type="response")
prediction <- as.vector(ifelse(prediction > 0.5,1,0))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
solution <- data.frame(PassengerId = test_PassengerId, Survived = prediction)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(solution, file = "solution.csv", row.names = FALSE)

