
library(ggplot2); library(ggthemes) # Data visualization
library(caret); library(dplyr)  # for preprocessing
library(rstan) # load stan
library(readr) # CSV file I/O, e.g. the read_csv function

#list.files("../input")
train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")
full <- bind_rows(train,test)

head(train)

full$Cabin <- as.factor(full$Cabin)
full$Embarked <- as.factor(full$Embarked)
full$Sex <- as.factor(full$Sex)
full$Pclass <- as.factor(full$Pclass) #no reason to assume this variable is linear with survival

full <- full[complete.cases(full$Embarked),]
train.size <- nrow(train) - sum(!complete.cases(train$Embarked))

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
full$Title <- as.factor(full$Title)

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1
full$FsizeD <- NA
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

hist(full$Fare, main="original Fare variable")
full$Fare <- log(full$Fare + 1)
hist(full$Fare, main="log transformed Fare variable")

ggplot(subset(full[1:train.size,], complete.cases(Age))) + 
    aes(x = as.vector(Age), y = factor(Survived), color=Sex) + 
    geom_jitter(alpha=.5,height=.1,width=0) + 
    ylab("Survived") + xlab("Age") +
    facet_wrap(~Sex) + ggtitle("for men, age 18 was a death sentence")

#build a model for age
age.model <- lm(Age ~ 0 + Title, data=full)

summary(age.model)

ggplot(subset(full,Title == "Master" & complete.cases(Age))) + aes(Age) + 
ggtitle("\"Masters\" tend to be very young") + xlab("Age") + 
geom_histogram(bins=15, alpha=.8) + theme_minimal()

with(subset(full,Title == "Master" & complete.cases(Age)),
    table(Sex, Pclass))

unknown.age <- full[is.na(full$Age),]
pred.age <- as.matrix(predict(age.model, unknown.age))
data.frame(predictedAge=round(pred.age,1),unknown.age)

# predict fare from your title, your class, and where you embarked
fare.model <- lm(Fare ~ Title + Pclass + Embarked, data=full)
summary(fare.model)

unknown.fare <- full[is.na(full$Fare),]
pred.fare <- as.matrix(predict(fare.model, unknown.fare))
data.frame(predictedLogFare=round(pred.fare,2),unknown.fare)
cat("avg (log) fare for the dataset:",round(mean(full$Fare, na.rm=T),2)) 
cat("\navg fare for a person in 3rd class:",round(mean(subset(full,Pclass==3)$Fare, na.rm=T),2))
cat("\npredicted fare for *this* person in 3rd class:", round(mean(pred.fare),2))

full$Fare[is.na(full$Fare)] <- pred.fare
full$Age[is.na(full$Age)] <- pred.age

full$Young <- as.numeric(full$Age < 18)
full$OldMen <- ifelse(full$Young == 0 & full$Sex == "male",1,-1)
mosaicplot(table( factor(full$OldMen,levels=c(-1,1),labels=c("Not over 18 and male","Over 18 and Male")), 
                 factor(full$Survived,levels=c(0,1),labels=c("Died","Survived"))),main="")

full$AgeZ <- scale(full$Age)

y <- full[1:train.size,]$Survived # outcome
IDs <- full[(train.size + 1): nrow(full),]$PassengerId # ID for predictions

predictor.variables <- c("Pclass","Sex","AgeZ","SibSp","Parch","Fare",
                          "Embarked","Title","FsizeD","Young","OldMen")
tosplit <- subset(full, select=predictor.variables)

train <- tosplit[1:train.size,]
test <- tosplit[(train.size + 1):nrow(tosplit),]

X <- model.matrix(y~., train)[,-1] #drop int
head(X)

test <- subset(test, select=predictor.variables)
X_test <- model.matrix(~., test)[,-1] #drop int
head(X_test)

model_string <- "
data {
  int<lower=0> N; // number of data items
  int<lower=0> N_pred; // number of items to be predicted
  int<lower=0> K; // number of predictors
  matrix[N, K] X; // predictor matrix
  matrix[N_pred, K] X_test; // predictor matrix for test data
  int y[N]; // outcome vector
}
parameters {
  real alpha; // intercept
  vector[K] beta; // coefficients for predictors
}
model {
  alpha ~ normal(0,5); // prior for intercept
  beta ~ double_exponential(0,1); // like a LASSO prior
  y ~ bernoulli_logit(X * beta + alpha); // likelihood
}
generated quantities{
  vector[N_pred] y_pred;
  for (n in 1:N_pred)
    y_pred[n] = bernoulli_rng(inv_logit(X_test[n] * beta + alpha));
}
"

stan_dat <- list(X=X, X_test=X_test, y = y, N = nrow(X), K=ncol(X), N_pred = nrow(X_test))

m <- stan(model_code = model_string, data = stan_dat, iter=4000, warmup=1000, chains=4, cores=4)

# plot beta coefficients with labels
beta.posteriors <- plot(m, pars="beta")
beta.posteriors + scale_y_continuous(breaks=1:ncol(X), labels=rev(colnames(X)))

# and print full fit (reminding me what the beta coefficients were)
data.frame(betaCoefficientNumber=1:ncol(X),Predictor=colnames(X))
m

smps <- extract(m)
y_pred <- smps$y_pred
probs <- apply(y_pred, 2, mean)

preds <- ifelse(probs > .5, 1, 0)

test <- cbind(data.frame(Age=round((test$AgeZ * sd(full$Age)) + mean(full$Age))),test) # get un-Z-scored age back real quick

testPassenger <- 13
test[testPassenger,]

a <- smps$alpha
betas <- smps$beta

plotPosterior <- function(testPassenger){
betasTimesCovariates <- t(t(betas) * X_test[testPassenger,])
posterior <- rowSums(cbind(a,betasTimesCovariates))
posterior <- 1 / (1 + exp(-posterior)) # take logistic to get a probability
plot(density(posterior),xlab="probability of survival",main="",xlim=0:1)
}

plotPosterior(testPassenger)
title("estimated probability of survival\nfor 23 year old female in first class")

testPassenger <- 3
test[testPassenger,]

plotPosterior(testPassenger)
title("estimated probability of survival\nfor 62 year old male in second class")

testPassenger <- 81
test[testPassenger,]

plotPosterior(testPassenger)
title("estimated probability of survival\nfor 6 year old boy in third class")

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = IDs, Survived = preds)
cat("Est. survival rate for test set:", round(mean(preds),2)*100,"%")
# Write the solution to file.
write.csv(solution, file = 'Solution.csv', row.names = F)

