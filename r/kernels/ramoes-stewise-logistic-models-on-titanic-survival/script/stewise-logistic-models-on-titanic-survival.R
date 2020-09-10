## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test) # bind training & test data

# check data
str(full)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(full$Survived)
prop.surv <- sum(train$Survived==1)/dim(train)[1]
prop.surv
#38.4% survived
odds.surv <- prop.surv/(1-prop.surv)
odds.surv
logit.surv <- log(odds.surv)
logit.surv


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Intercept only model
#As we saw earlier, 0=not survived, 1=survived

null.glm <- glm(Survived ~ 1, family=binomial, data=full)
summary(null.glm)
#Intercept==log odds=-.47, p<.001. AIC=1188.7, deviance=1186.7, df=890)
#The probability of survival are obviously lower than the chances of not-surviving

#H0: odds(survival) = 1
#H0: P(survival) = 0.5

b0 <- coef(null.glm)
exp(b0) #odds of survival
exp(b0)/(1+exp(b0)) #probability of survival


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(plyr)
full$male <- revalue(full$Sex, c("male"="1", "female"="0"))

full$male <- as.numeric(full$male)
library(Hmisc)
describe(full)
summary(full)
class(full$male)
male.glm <- update(null.glm, ~ . + full$male)
male.glm #AIC=921.8, LP=-2.51. Males are less likely to survive


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$FsizeC <- revalue(full$FsizeD, c("singleton"="1", "small"="2", "large"="3"))
full$FsizeCat <- as.numeric(full$FsizeC)
fam.size.glm <- update(null.glm, ~. + full$FsizeCat)
fam.size.glm

#Odds ratios of survival of singletons
odds.single <- exp(coef(fam.size.glm)[1])
odds.single
#Odds ratio of survival of small families
OR.fam <- exp(coef(fam.size.glm)[2])
OR.fam
odds.small <- odds.single*OR.fam
odds.large <- odds.single*OR.fam^2

#Probabilities of survival
prob.single <- odds.single/(1+odds.single)
prob.small <- odds.small/(1+odds.small)
prob.large <- odds.large/(1+odds.large)
c(prob.single, prob.small, prob.large)
#Individuals with large families on the boat have highest probability of survival. This is probably because
#richer people could bring along more family embers and had better access to rescue boats.
#Singletons are most likely the poorer ones, able to only pay for one ticket, and worst access to rescue boats.


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Stepwise model selection
log.model <- glm(Survived ~ FsizeCat*male*Age*SibSp*Fare, data=na.omit(full))
final.model<-step(log.model, direction="backward", trace=FALSE)
final.model


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Prediction <- (1- full$male)
train <- full[1:891,]
test <- full[892:1309,]
if (full$PassengerId>=892) {
 prediction <- (1- full$male)
 }
newdata <- subset(test, PassengerId>=892, 
    select=c(PassengerId, Prediction)) 
solution <- data.frame(PassengerID = full$PassengerId, Survived = Prediction)
write.csv(solution, file = 'glmsolution.csv', row.names = F)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Write the solution to file
#train1 <- full[1:891,]
#test1 <- full[892:1309,]
#lm.model <- lm(Survived ~ FsizeCat+male+Age+SibSp+Fare, test1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#prediction <- predict(lm.model, test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
#write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

