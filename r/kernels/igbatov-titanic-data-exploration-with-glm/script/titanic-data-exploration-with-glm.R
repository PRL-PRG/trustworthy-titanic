## ----message=F, warning=F--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# for glmnet()
library(glmnet)

# cv.glm (cross-validation of GLM)
library(boot)

# logistf()
library(logistf)

# lda()
library(MASS)

# impute with missForest()
library(missForest)

# createDataPartition()
library(caret)

# for missmap()
library(Amelia)

# for ggpairs() function
library(GGally)


## ----message=F, warning=F, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Taken from http://www.highstat.com/Book2/HighstatLibV6.R
#Library files for courses provided by: Highland Statistics Ltd.
#To cite these functions, use:
#Mixed effects models and extensions in ecology with R. (2009).
#Zuur, AF, Ieno, EN, Walker, N, Saveliev, AA, and Smith, GM. Springer.

#Copyright Highland Statistics LTD.

#####################################################################
#VIF FUNCTION.
#To use:  corvif(YourDataFile)
corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  #correlation part
  #cat("Correlations of the variables\n\n")
  #tmp_cor <- cor(dataz,use="complete.obs")
  #print(tmp_cor)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}


#Support function for corvif. Will not be called by the user
myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}
#END VIF FUNCTIONS


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotPredictors = function(respname,data){
  # arguments type check
  data = as.data.frame(data);
  if(!is.character(respname)){
    print('respname must be a string');
    return(FALSE)
  }

  par(mfrow=c(1,length(colnames(data))-1)); 
  for(predname in colnames(data)){
    if(predname == respname) next();
    print(plot(data[,c(respname)], data[,c(predname)], xlab=respname, ylab=predname))
  }
  
  # default par
  par(mfrow=c(1,1));
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load data
train <- read.csv('../input/train.csv',na.strings=c(""))
test <- read.csv('../input/test.csv',na.strings=c(""))
# cast right datatypes on test

# 891 obs.
dim(train)
# plot missing variables
missmap(train);
# check for variables with bad predictive power
nearZeroVar(train,saveMetrics= TRUE)
# check how many NA values each predictor has
sapply(train, function(x) sum(is.na(x)))
# check how many unique valies each predictor has
sapply(train, function(x) length(unique(x)))

# Clean #1 
# -  remove NA cells
cTrain = na.omit(train)
# 891 obs.
dim(cTrain)
# - remove unnecesarry predictors
cTrain = subset(cTrain, select = -c(PassengerId,Ticket) )
# - set right predictor types
str(cTrain)
cTrain$Survived = factor(cTrain$Survived)
cTrain$Pclass = factor(cTrain$Pclass)

## Create new predictors
#  - extract Deck from Cabin
cTrain$CabinLetter = factor(substr(cTrain$Cabin, 1, 1))
#  - extract cabin number from Cabin
cTrain$CabinNumber = factor(as.numeric(substr(cTrain$Cabin, 2, 4)))
#  - extract title
cTrain$Title = factor(gsub('(.*, )|(\\..*)', '', cTrain$Name))
# - extract Surnames
cTrain$Surname <- factor(sapply(cTrain$Name, function(x){x=as.character(x); strsplit(x, split = '[,.]')[[1]][1];}))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Clean #2 
# - remove all rows with empty Deck
ccTrain = subset(cTrain, CabinLetter != '')
dim(ccTrain)
# - remove all not-numeric CabinNumber
ccTrain = subset(ccTrain, !is.na(CabinNumber))
dim(ccTrain)
# - remove all not-numeric CabinNumber
ccTrain = subset(ccTrain, Embarked != '')
dim(ccTrain)
# - make CabinNumber to be a factor
ccTrain$CabinNumber = factor(ccTrain$CabinNumber);


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glmfit = glm(Survived~.,data=subset(ccTrain,select=-c(Surname,Cabin,Name,CabinNumber)),family=binomial);


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
levels(ccTrain$CabinNumber)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glmfit = glm(Survived~.,data=subset(ccTrain,select=-c(Surname,Cabin,Name,CabinNumber)),family=binomial);

summary(glmfit)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
corvif(subset(ccTrain,select=-c(Surname,Cabin,Name,CabinNumber)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
corvif(subset(ccTrain,select=-c(Surname,Cabin,Name,CabinNumber,Title)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
corvif(subset(ccTrain,select=-c(Surname,Cabin,Name,CabinNumber,Title, CabinLetter)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glmfit = glm(Survived~.,data=subset(ccTrain,select=-c(Surname,Cabin,Name,CabinNumber,Title,CabinLetter)),family=binomial);
summary(glmfit)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cTrain = subset(train,select=-c(Cabin,Name,Ticket,PassengerId))
#impute Age - this will give us extra 0.01 in prediction accuaracy
cTrain$Age[is.na(cTrain$Age)] <- mean(cTrain$Age,na.rm=T)
cTrain = na.omit(cTrain)
dim(cTrain)
cTrain$Survived = factor(cTrain$Survived)
cTrain$Pclass = factor(cTrain$Pclass)
glmfit = glm(Survived~.,data=cTrain,family=binomial);
summary(glmfit)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
corvif(cTrain)


## ----message=F, warning=F--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotPredictors('Survived',cTrain)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(effects)
plot(allEffects(glmfit))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# split cTrain on train and test
set.seed(726) 
inTrain <- createDataPartition(cTrain$Survived, p = 0.7, list = FALSE)
cTrainTrain <- cTrain[inTrain, ]
cTrainTest <- cTrain[-inTrain, ]
# check how data is splitted
table(cTrainTrain$Survived)
table(cTrainTest$Survived)
# make model fit on train data
glmfit = glm(Survived~.,data=cTrainTrain,family=binomial);
# predict test data
pr = factor(ifelse(predict(glmfit, newdata = cTrainTest) > 0.5,1,0))
confusionMatrix(cTrainTest$Survived, pr)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use 10-fold cross-validation
glmfit = glm(Survived~.,data=cTrain,family=binomial);
error = cv.glm(cTrain, glmfit,K=10)
# standard k-fold CV estimate
error$delta [1]
# bias corrected version k-fold CV estimate
error$delta [2]


## ----message=F, warning=F--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Stepwise Regression
library(MASS)
glmfit = glm(Survived~.^2, cTrain, family=binomial);
step <- stepAIC(glmfit, direction="both",trace=FALSE)
#step$anova # display results
# return initial layout
par(mfrow=c(1,1))
error = cv.glm(cTrain, glmfit,K=10)
# standard k-fold CV estimate
error$delta [1]
# bias corrected version k-fold CV estimate
error$delta [2]

