## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

rm(list = ls())

library(reshape2)
library(ggplot2)
library(GGally)
library(Hmisc)
library(plyr)
library(gridExtra)

library(gbm)
library(np)
library(earth)
library(rpart)
library(randomForest)
library(nnet)
library(e1071)

random_seed = 123



## ----load_data-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
col_classes = c('numeric','numeric', 'factor', 'character', 'factor', 'numeric', 'factor', 'factor', 'character', 'numeric', 'character', 'factor')


train <- read.csv('../input/train.csv', colClasses = col_classes)
test  <- read.csv('../input/test.csv', colClasses = col_classes[-2])
test$Survived = NA

str(train)



## ----functions-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# compute correct classification ratio
to_numeric<-function(x){
  as.numeric(as.character(x))
}
model_ccr = function(actual, model) {
  cm  <- table(actual == 1, model >= 0.5)
  ccr <- sum(diag(cm))/sum(cm)
  return (ccr)
} 

model_ks = function(actual, model) {
  m1 = sort(model[actual == 1])
  m0 = sort(model[actual == 0])

  #estimate difference between cdf
  xc = seq(0, 1, 0.01)
  q1 = ecdf(m1)(xc)
  q0 = ecdf(m0)(xc)
  ks = 100 * max(abs(q1 - q0), na.rm = T)
  return (ks)
} 

trapez_rule_integration <- function(x, y){
  index = order(x)
  dx = diff(x[index])
  ys = y[index]
  yc = 0.5 * (ys[-1] + ys[-length(y)])
  return (sum(dx * yc))
}

model_roc = function(actual, model, buckets = 101) {
    thresholds = seq(0, 1, length.out = buckets)
  res = ldply(thresholds, .id = "threshold", function(x) {
    cm = confusion_matrix(actual, model, x)
    tp = cm$true_pos/cm$pos
    fp = cm$false_pos/cm$neg
    data.frame(threshold = x, tp, fp)
    })
  }
  
model_auc = function(actual, model) {
  roc = model_roc(actual, model)
  auc = trapez_rule_integration(roc$fp, roc$tp)
  return (auc)
} 

confusion_matrix = function(actual, model, threshold = 0.5) {
  pred = ifelse(model >= threshold, 1, 0)
  cm = list()
  cm$true_pos = sum(actual == 1 & pred == 1)
  cm$true_neg = sum(actual == 0 & pred == 0)
  
  cm$false_pos = sum(actual == 0 & pred == 1)
  cm$false_neg = sum(actual == 1 & pred == 0)
  
  cm$neg = sum(actual == 0)
  cm$pos = sum(actual == 1)
  
  cm$ccr = (cm$true_pos + cm$true_neg) / (cm$neg + cm$pos)
  
  return (cm)
} 

#do all diagnostic plots
plot_binmodel_predictions<-function(actual, model){
  p1 = plot_binmodel_percentiles(actual, model, 10)
  p2 = plot_binmodel_cdf(actual, model)
  p3 = plot_binmodel_roc(actual, model)
  p4 = plot_binmodel_density(actual, model)
  grid.arrange(p1, p2, p3, p4, ncol=2)
}

#plot ROC curve
plot_binmodel_roc<-function(actual, model){
  dt = 0.01
  thresholds = seq(0, 1, by = dt)
  res = ldply(thresholds, .id = "threshold", function(x) {
    cm = confusion_matrix(actual, model, x)
    tp = cm$true_pos/cm$pos
    fp = cm$false_pos/cm$neg
    ccr = cm$ccr
    data.frame(threshold = x, tp, fp, ccr)
  })
  
  fp05 = res$fp[abs(res$threshold-0.5)<dt][1]
  
  auc = trapez_rule_integration(res$fp, res$tp)
  
  ggplot(res, aes(fp, tp, color = threshold )) +  
    geom_point(size = 1) + 
    geom_line(size = 0.5)  + xlim(0, 1) + ylim(0, 1) +
    geom_point(aes(fp, ccr), size = 1) + 
    geom_abline(slope = 1, intercept = 0, colour = 'red', linetype = 2) +
    geom_vline(xintercept  = fp05, colour = 'blue', linetype = 2) +
  labs(x = "false positive",   y = "true positive") +
  ggtitle(paste('auc:', round(auc, 6) ))
  }

#plot density of predictions 
plot_binmodel_density<-function(actual, model){
  ggplot(data.frame(actual, model), aes(model, fill = factor(actual)))  + 
    geom_histogram(binwidth = 0.1, bins = 10, boundary = 0) +
    xlim(0, 1) +  scale_fill_manual(values = c('black', 'red'))
}
  
#plot cdf of predictions
plot_binmodel_cdf<-function(actual, model){
  m1 = sort(model[actual == 1])
  m0 = sort(model[actual == 0])

  #estimate difference between cdf
  xc = seq(0, 1, 0.01)
  q1 = ecdf(m1)(xc)
  q0 = ecdf(m0)(xc)
  ks = 100 * max(abs(q1 - q0), na.rm = T)
  
  res1 = data.frame(p = m1, q = seq(0, 1, length.out = length(m1)), outcome = 'actual = 1')
  res2 = data.frame(p = m0, q = seq(0, 1, length.out = length(m0)), outcome = 'actual = 0')
  
  res = rbind(res1, res2)
  
  ggplot(res, aes(p, q, group = outcome, color = outcome)) + 
  geom_line(size = 1) + xlim(0, 1) + ylim(0, 1) +
  ggtitle(paste('ks:', round(ks, 6) ))+
  scale_color_manual(values = c('red', 'black')) +
  labs(x = "probability",   y = "fraction")
}
 
#percentile plot, model vs average actuals  
plot_binmodel_percentiles<-function(actual, model, n = 10){
  xb = seq(0, n, 1) / n
  buckets = cut(model, xb, ordered_result = TRUE, include.lowest = TRUE)
 
  df = data.frame(actual, model, buckets)
  df = df[!is.na(actual),]
  
  res = ddply(df, .(buckets), function(x) {
    c(avg_actual = mean(x$actual), 
      avg_model = mean(x$model, na.rm = T),
      std = sd(x$actual),
      count = length(x$actual) )
  })
  res$error = 2 * res$std / sqrt(res$count)
  
  ccr = model_ccr(actual, model)
  
  ggplot(res, aes(avg_model, avg_actual)) + 
    geom_point(aes(size = count)) +  xlim(0, 1) + 
    geom_errorbar(aes(ymax = avg_actual + error, ymin=avg_actual - error), width=0.02) + 
    geom_abline(slope = 1, intercept = 0, colour = 'red', linetype = 2) +
    geom_hline(yintercept = 0, colour = 'black', linetype = 2) +
    geom_hline(yintercept = 1, colour = 'black', linetype = 2) +
    geom_vline(xintercept = 0, colour = 'black', linetype = 2) +
    geom_vline(xintercept = 1, colour = 'black', linetype = 2) +
    labs(x = "model",   y = "actual") + 
    ggtitle(paste('ccr:', round(ccr, 6) ))
}

#plot missing values
ggplot_missing <- function(x){
  mx = melt(is.na(x))
  ggplot(mx, aes(Var2, Var1)) + geom_raster(aes(fill = value)) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  scale_fill_grey(name = "", labels = c("Valid","NA")) +
  labs(x = "Variable name",   y = "Rows") + 
    ggtitle (paste('total number of missing values:',  sum(mx$value)))
}

#plot number of missing values
ggplot_missing_count <- function(x){
  mc = adply(is.na(x), 2, sum)
  names(mc) <- c('name', 'value')
  ggplot(mc, aes(name, value)) + geom_bar(stat = "identity") +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  labs(x = "Variable name",   y = "Missing Variables")
}

# Friedman's H-statistic to assess the relative strength of interaction effects in non-linear models. H is on the scale of [0-1] 
gbm_interactions <- function(gbm_model, data, min_influence = 1, degree = 2){
  gbm_summary = summary(gbm_model, plotit=FALSE)
  vars = gbm_summary$var[gbm_summary$rel.inf > min_influence]
  all_combinations = combn(as.vector(vars), degree, simplify = TRUE)
  df = ldply(seq(dim(all_combinations)[2]), function(i) {
    data.frame(vars = paste(all_combinations[,i], collapse = '-'), 
               interaction_score = interact.gbm(gbm_model, data, all_combinations[,i])) 
    })
  return ( df[order(df$interaction_score, decreasing = TRUE),] )
}


## ----overview, fig.width = 8, fig.height = 6, dpi = 100--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#combine data 
train$Tag = "train"
test$Tag = "test"

names = intersect( names(train), names(test))
full_df = rbind(train[,names], test[,names])

train_index = full_df$Tag == "train"
test_index = full_df$Tag == "test"

full_df$Surv = as.factor(full_df$Survived)


#there are two empty Embarked fields in train set, table(full_df$Embarked) 
full_df$Embarked = as.character(full_df$Embarked)
full_df$Embarked[full_df$Embarked == ""] = "S"
full_df$Embarked = factor(full_df$Embarked)

#non of the people with 5 or more siblings survived, so no need to consider more levels 
full_df$Siblings = to_numeric(full_df$SibSp)
full_df$Siblings[full_df$Siblings>5] = 5
full_df$Siblings = factor(full_df$Siblings)
full_df$SiblingsOrdered = factor(full_df$Siblings,ordered = TRUE)

full_df$Parch = to_numeric(full_df$Parch)
full_df$Parch[full_df$Parch>5] = 5
full_df$Parch = factor(full_df$Parch)
full_df$ParchOrdered = factor(full_df$Parch, ordered = TRUE)

family_size = 1 + to_numeric(full_df$Parch) + to_numeric(full_df$Siblings)
full_df$FamilySize = factor(family_size)
full_df$FamilySizeOrdered = factor(family_size, ordered = TRUE)

p1 = ggplot_missing(train) + ggtitle('Missing values')
p2 = ggplot_missing_count(train)
grid.arrange(p1, p2, ncol=2)

#plot
p1 = ggplot(full_df[train_index,], aes(x = Sex, fill = Surv)) +geom_bar()
p2 = ggplot(full_df[train_index,], aes(x = Pclass, fill = Surv)) + geom_bar()
p3 = ggplot(full_df[train_index,], aes(x = SibSp, fill = Surv)) + geom_bar()
p4 = ggplot(full_df[train_index,], aes(x = Parch, fill = Surv)) + geom_bar()
grid.arrange(p1, p2, p3, p4, ncol=2)


p1 = ggplot(full_df[train_index,], aes(x = Age, fill = Surv)) + stat_density(adjust = 0.5)
p2 = ggplot(full_df[train_index,], aes(x = Fare, fill = Surv)) + stat_density(adjust = 0.7)
p3 = ggplot(full_df[train_index,], aes(x = Embarked, fill = Surv)) + geom_bar()
p4 = ggplot(full_df[train_index,], aes(x = Pclass, Age)) + geom_boxplot(aes(fill = Surv)) + geom_jitter()
grid.arrange(p1, p2, p3, p4, ncol=2)



## ----parsing_titles, fig.width = 8, fig.height = 6, dpi = 100--------------------------------------------------------------------------------------------------------------------------------------------------------------------

names_df = adply(full_df$Name,1, function(x) {
  tokens = unlist(strsplit(x, split='[,.]')) #split string
  tokens = sapply(tokens, FUN = function(x) sub(" ", "", x))  #remove spaces
  c( Lastname = tokens[1], 
     Title = tokens[2],
     Firstname = tokens[3])
  })

full_df$Lastname = names_df$Lastname
full_df$Title = names_df$Title

table(full_df$Title,full_df$Survived)
table(full_df$Title,full_df$Sex)

#combine rare titles
full_df$Title[full_df$Title %in% c('Capt', 'Don', 'Major', 'Col', 'Sir', 'Jonkheer')] <- 'Sir'
full_df$Title[full_df$Title %in% c('Dona', 'Lady', 'the Countess' )] <- 'Lady'
full_df$Title[full_df$Title %in% c('Miss', 'Mlle')] <- 'Miss'
full_df$Title[full_df$Title %in% c('Mrs', 'Mme', 'Ms')] <- 'Mrs'
full_df$Title[full_df$Title == 'Dr' &  full_df$Sex == 'female' ] <- 'Lady'
full_df$Title[full_df$Title == 'Dr' &  full_df$Sex == 'male' ] <- 'Sir'

full_df$Title = factor(full_df$Title)

#Mr and Rev - are the most unfortunate title 
ggplot(full_df[train_index,], aes(x = Title, fill = Surv)) + geom_bar()

# we assumed that outcomes for people with the same last name are correlated 
# compute the number of people with the same last numes
# 
family_survival = ddply(full_df[train_index,], .(Lastname), function(x) c(
  family_size = length(x$Survived), 
  family_surv = sum(x$Survived==1, na.rm = T),
  family_died = sum(x$Survived==0, na.rm = T),
  family_surv_known = sum(!is.na(x$Survived))))

head(family_survival)

temp_df = merge(full_df, family_survival, all.x = TRUE)
#table(family_survival$family_surv_ratio)
#table(family_survival$family_surv_known)

#number of surviving family members other than a person
temp_df$surv_family_count = temp_df$family_surv 
temp_df$surv_family_count[temp_df$Survived == 1 & temp_df$Tag == "train"] = temp_df$surv_family_count[temp_df$Survived == 1 & temp_df$Tag == "train"] - 1
temp_df$surv_family_count[is.na(temp_df$surv_family_count)] = 0

temp_df$died_family_count = temp_df$family_died 
temp_df$died_family_count[temp_df$Survived == 0 & temp_df$Tag == "train"] = temp_df$died_family_count[temp_df$Survived == 0 & temp_df$Tag == "train"] - 1
temp_df$died_family_count[is.na(temp_df$died_family_count)] = 0


#table(temp_df$surv_family_count)
#table(temp_df$died_family_count)

full_df = temp_df

train_index = full_df$Tag == "train"
test_index = full_df$Tag == "test"



## ----parsing_cabins, fig.width = 8, fig.height = 6, dpi = 100--------------------------------------------------------------------------------------------------------------------------------------------------------------------

cabins_df = adply(full_df$Cabin,1, function(x) {
  tokens = unlist(strsplit(x, split='[ ]')) #split string
  if(length(tokens)>=1){
    c( 
      CabinSection = substring(tokens[1],1,1), 
      CabinNumber = substring(tokens[1],2, nchar(tokens[1])),
      CabinCount = length(tokens))
  }else{
    c(
      CabinSection = "N", #unknown values
      CabinNumber = 0,    
      CabinCount = 1)
    }
  })

#section F - seems to be important becuase model predictions are off
cabins_df$CabinSection = as.character(cabins_df$CabinSection)
cabins_df$CabinSection[cabins_df$CabinSection %in% c('A', 'G', 'T')] = 'AGT'
cabins_df$CabinNumber[cabins_df$CabinNumber==""] = "0"

full_df$CabinSection = as.factor(cabins_df$CabinSection)
full_df$CabinNumber = as.factor(40*ceil(as.numeric(cabins_df$CabinNumber)/40))
full_df$CabinCount = as.numeric(cabins_df$CabinCount)
full_df$CabinCount[full_df$CabinCount>=2] = 2 #sample size too small
full_df$CabinCount = as.factor(full_df$CabinCount)
 

table(full_df$CabinSection,full_df$Survived)
#table(full_df$CabinSection[test_index], ifelse(pred.gbm_title[test_index]>=0.5, 1, 0))

table(full_df$CabinCount,full_df$Survived)
#table(full_df$CabinCount[test_index], ifelse(pred.gbm_title[test_index]>=0.5, 1, 0))

table(full_df$CabinNumber,full_df$Survived)
#table(50*ceil(as.numeric(full_df$CabinNumber[test_index])/50), ifelse(pred.gbm_title[test_index]>=0.5, 1, 0))

#Mr and Rev - are the most unfortunate title 
ggplot(full_df[train_index,], aes(x = CabinCount, fill = Surv)) + geom_bar()
ggplot(full_df[train_index,], aes(x = CabinSection, fill = Surv)) + geom_bar()
ggplot(full_df[train_index,], aes(x = CabinNumber, fill = Surv)) + geom_bar()



## ----parsing_ticket, fig.width = 8, fig.height = 6, dpi = 100--------------------------------------------------------------------------------------------------------------------------------------------------------------------

ticket_df = adply(full_df$Ticket,1, function(x) {
  tokens = unlist(strsplit(x, split='[ ]')) #split string
  n = length(tokens)
  if(n==1){
    c(TicketPrefix = "", TicketNumber = tokens[1])
  }else if(n>=1){
    c(TicketPrefix = paste(tokens[-n], collapse = ''), TicketNumber = tokens[n])
  }else{
    c(TicketPrefix = "", TicketNumber = "0")
    }
  })

ticket_df$TicketNumber[ticket_df$TicketNumber == ""]

ticket_df$TicketPrefix[ticket_df$TicketPrefix != 'PC'] = "NA"

#table(ticket_df$TicketPrefix)

full_df$TicketPrefix = as.factor(ticket_df$TicketPrefix)
full_df$TicketNumber = to_numeric(ticket_df$TicketNumber)
full_df$TicketNumber[is.na(full_df$TicketNumber)] = 0

#table(full_df$TicketPrefix,full_df$Survived)

#not useful 
ggplot(full_df[train_index,], aes(x = TicketPrefix, fill = Surv)) + geom_bar()
ggplot(full_df[train_index,], aes(x = TicketNumber, fill = Surv)) + geom_bar()


## ----missing_fare, fig.width = 8, fig.height = 6, dpi = 150----------------------------------------------------------------------------------------------------------------------------------------------------------------------

fare_na_index = is.na(full_df$Fare)

fare_formula = as.formula(Fare ~ Sex + Pclass + Siblings + Parch + Embarked)

ntrees = 2000
set.seed(random_seed)
model.gbm.fare = gbm(fare_formula, 
                  data = full_df[!fare_na_index, all.vars(fare_formula)], 
                  distribution = 'gaussian',
                  n.trees = ntrees,
                  shrinkage = 0.005,
                  bag.fraction = 0.7,
                  interaction.depth = 2, cv.folds = 3)
print(model.gbm.fare)

par(mfrow = c(1, 2), las = 1)
best_it.fare = gbm.perf(model.gbm.fare, method = 'cv')
grid()
summary(model.gbm.fare, n.trees = best_it.fare)
grid()

par(mfrow=c(3,3))
for(i in seq(length(all.vars(fare_formula)) - 1)) {
  plot.gbm(model.gbm.fare, n.trees =best_it.fare,  i = i)
  grid()
}

full_df$FareEstimate = predict(model.gbm.fare, newdata = full_df, n.trees =best_it.fare)

ggplot(full_df, aes(FareEstimate, Fare, color = Pclass)) + geom_point() + geom_smooth(method = 'lm')

#there is missing fare in test set, estimate at 8.6
full_df$Fare[fare_na_index] = full_df$FareEstimate[fare_na_index]

cat(paste('missing fare estimate:', full_df$FareEstimate[fare_na_index]))



## ----missing_age, fig.width = 8, fig.height = 6, dpi = 100-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
age_na_index = is.na(full_df$Age)

age_formula = as.formula(Age ~  Pclass + Siblings + Parch)

age_df = full_df[!age_na_index,all.vars(age_formula)]

ntrees = 10000
set.seed(random_seed)
model.gbm.age = gbm(age_formula, 
                  data = age_df, 
                  distribution = 'gaussian',
                  n.trees = ntrees,
                  shrinkage = 0.005,
                  bag.fraction = 0.7,
                  interaction.depth = 2, cv.folds = 3)
print(model.gbm.age)

par(mfrow = c(1, 2), las = 1)
best_it_age = gbm.perf(model.gbm.age, method = 'cv')
grid()
summary(model.gbm.age, n.trees = best_it_age) # influence
grid()

par(mfrow=c(2,2))
for(i in seq(length(all.vars(age_formula)) - 1)) {
  plot.gbm(model.gbm.age, n.trees =best_it_age,  i = i)
  grid()
}
 
cat('Two-way interactions') 
gbm_interactions(model.gbm.age, age_df, 1, 2)

full_df$AgeEstimateGBM = predict(model.gbm.age, n.trees = best_it_age, newdata = full_df)

age_df_gbm = cbind( age_df, age_estimate = full_df$AgeEstimateGBM[!age_na_index], model = 'gbm')

ggplot(age_df_gbm, aes(age_estimate, Age, color = Pclass)) + 
  geom_point() + ggtitle('Age estimate using gbm')



## ----missing_age_linear, fig.width = 8, fig.height = 4, dpi = 100----------------------------------------------------------------------------------------------------------------------------------------------------------------
#linear model, exclude fare as it does not seem to be important 
model.lm.age = lm(Age ~  Pclass + Siblings + Parch, data = age_df)
summary(model.lm.age)

full_df$AgeEstimateLM = predict(model.lm.age, newdata = full_df)

#store age estimate from linear model
age_df_lm = cbind( age_df, age_estimate = full_df$AgeEstimateLM[!age_na_index], model = 'glm')

ggplot(age_df_lm, aes(age_estimate, Age, color = Pclass)) + 
  geom_point() + ggtitle('Age estimate using linear model')



## ----missing_age_comparison, fig.width = 8, fig.height = 6, dpi = 150------------------------------------------------------------------------------------------------------------------------------------------------------------
age_df_agg = rbind(age_df_lm, age_df_gbm)

ggplot(age_df_agg, aes(age_estimate, Age, color = model, group = model)) + 
  geom_point() + facet_grid(.~model) + geom_smooth(method = 'lm') + geom_abline() + ggtitle('Comparison of Age Estimates')

print(ddply(age_df_agg, .(model), function(x) c(r2 = summary(lm(x$Age~x$age_estimate))$r.squared)))

#copy estimated ages to the data (using GBM method)
full_df$Age = full_df$AgeEstimateGBM

Survived = full_df$Survived[train_index]


## ----rpart, fig.width = 8, fig.height = 6, dpi = 150-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model.rp = rpart(Survived ~ Sex + Pclass + Siblings + Fare + Title + CabinNumber + surv_family_count + died_family_count,
                 data = full_df[train_index,], 
                 control = rpart.control(cp = 0.01, minsplit = 20))

printcp(model.rp)

par(mfrow=c(2,1))
plotcp(model.rp) 
plot(model.rp, uniform=TRUE) 
text(model.rp, use.n=TRUE, all=TRUE, cex=.8)

pred.rp = predict(model.rp, newdata = full_df)

cat(paste('rpart: ', model_ccr(Survived, pred.rp[train_index])))
plot_binmodel_predictions(Survived, pred.rp[train_index])



## ----rf, fig.width = 8, fig.height = 6, dpi = 100--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model.rf <- randomForest(Surv ~ Sex +  Pclass + Siblings + Fare + Title + CabinNumber + surv_family_count + died_family_count, data =  full_df[train_index,])

pred.rf = to_numeric(predict(model.rf, newdata = full_df))

importance(model.rf)

cat(paste('random forest:', model_ccr(Survived, pred.rf[train_index])))
plot_binmodel_predictions(Survived, pred.rf[train_index])


## ----logistic, fig.width = 8, fig.height = 6, dpi = 100--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model.glm <- glm(Survived ~ Sex +  Pclass + Siblings + Fare + Title + CabinNumber + surv_family_count + died_family_count, 
                 data = full_df[train_index,], family = binomial)
summary(model.glm)
pred.glm = predict(model.glm, newdata = full_df, type = 'response')

cat(paste('logistic regression:', model_ccr(Survived, pred.glm[train_index])))
plot_binmodel_predictions(Survived, pred.glm[train_index])


## ----logistic_age, fig.width = 8, fig.height = 6, dpi = 100----------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_df_spline = full_df

fare_spline = bs(full_df_spline$Fare, knots =  c(0, 50, 100, 150), degree = 1)
full_df_spline$FareSpline = fare_spline[, -dim(fare_spline)[2]]

model.glm_s <- glm(Survived ~ Sex +  Pclass + Siblings + Fare + Title + CabinNumber + surv_family_count + died_family_count + FareSpline, 
                   data = full_df_spline[train_index,], family = binomial)

summary(model.glm_s)

pred.glm_s      = predict(model.glm_s, newdata = full_df_spline, type = 'response')

cat(paste('logistic regression (spline):', model_ccr(Survived, pred.glm_s[train_index])))
plot_binmodel_predictions(Survived, pred.glm_s[train_index])



## ----nnet, fig.width = 8, fig.height = 6, dpi = 100------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(random_seed)
model.nnet <- nnet(Survived ~ Sex + Age + Pclass + Siblings + Parch + Embarked + Fare, 
                   data = full_df[train_index,], size=6, maxit = 1000, trace=FALSE, decay = 1e-4)
pred.nnet <- predict(model.nnet, newdata = full_df)

print(model.nnet)

cat(paste('Neural Networks:', model_ccr(Survived, pred.nnet[train_index])))
plot_binmodel_predictions(Survived, pred.nnet[train_index])


## ----mars, fig.width = 8, fig.height = 6, dpi = 100------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(random_seed)
mars_formula = formula (Survived ~ Sex +  Pclass + Siblings + Fare + Title + CabinNumber + surv_family_count + died_family_count)
model.mars <- earth(mars_formula, 
                    data = full_df[train_index, all.vars(mars_formula)], 
                    glm=list(family=binomial), degree = 2, nfold = 3)

plot(model.mars)
summary(model.mars)
par(mfrow=c(1,1))
plot(evimp(model.mars))
#plotmo(model.mars)

pred.mars = as.vector(predict(model.mars, type = 'response', newdata = full_df))

cat(paste('MARS (cabin):', model_ccr(Survived, pred.mars[train_index])))
plot_binmodel_predictions(Survived, pred.mars[train_index])


## ----gbm, fig.width = 8, fig.height = 6, dpi = 150-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fml_name = as.formula(Survived ~ Pclass + Fare + Title + CabinNumber  + surv_family_count + died_family_count)
#fml = as.formula(Surv ~ Sex + Age + Pclass + Siblings + Parch + Embarked + Fare)
set.seed(random_seed)

ntrees = 4000
model.gbm = gbm(fml_name, 
                data = full_df[train_index, all.vars(fml_name)], 
                distribution = 'bernoulli',
                n.trees = ntrees,
                shrinkage = 0.005,
                bag.fraction = 1.0,
                interaction.depth = 2,
                cv.folds = 3)
print(model.gbm)

par(mfrow = c(1, 2), las = 1)
best_it_name = gbm.perf(model.gbm, method = 'cv')
grid()
summary(model.gbm, n.trees = best_it_name) # influence
grid()

par(mfrow=c(4,3))
for(i in seq(length(all.vars(fml_name)) - 1)) {
  plot.gbm(model.gbm, n.trees =best_it_name,  i = i)
  grid()
}

cat('Two-way interactions') 
gbm_interactions(model.gbm, full_df[train_index,], 1, 2)

pred.gbm = predict(model.gbm, n.trees = best_it_name, newdata = full_df, type = 'response')

cat(paste("gbm ccr:", model_ccr(Survived, pred.gbm[train_index])))
plot_binmodel_predictions(Survived, pred.gbm[train_index])


## ----svm, fig.width = 8, fig.height = 6, dpi = 150, eval = TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------
svn_formula = formula(Surv ~ Sex + Age + Pclass + Siblings + Parch + Embarked + Fare + CabinNumber + surv_family_count + died_family_count)
model.svm <- svm(svn_formula, data=full_df[train_index,all.vars(svn_formula)], cost = 1, gamma = 0.125)
summary(model.svm)

#svm_tune <- tune(svm, train.x=svn_formula,  data=full_df[train_index,],
#                 kernel="radial", 
#                 ranges=list(cost=2^(-6:6), gamma=2^(-6:6)))
#print(svm_tune)


pred.svm = to_numeric(predict(model.svm, newdata = full_df))

cat(paste("svm ccr:", model_ccr(Survived, pred.svm[train_index])))
plot_binmodel_predictions(Survived, pred.svm[train_index])



## ----compare, fig.width = 8, fig.height = 6, dpi = 100---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

results = list()
#results$np = pred.np
results$glm = pred.glm
results$glm_s = pred.glm_s
results$gbm = pred.gbm 
results$nnet = pred.nnet
results$svm = pred.svm
results$rf = pred.rf
results$mars =pred.mars
results$rp = pred.rp


res = ldply(results, .id = 'model', function(x) {
  c(ccr=model_ccr(Survived, x[train_index]),
    ks =model_ks(Survived, x[train_index]),
    auc = model_auc(Survived, x[train_index]),
    na_count = sum(is.na(x)))
})

print(res)

ggplot(res, aes(model, ccr)) + geom_bar(stat = "identity") + coord_flip() + coord_cartesian(ylim = c(0.9*min(res$ccr), 1.0))
ggplot(res, aes(model, ks)) + geom_bar(stat = "identity") + coord_flip() + coord_cartesian(ylim = c(0.9*min(res$ks), 100.0))
ggplot(res, aes(model, auc)) + geom_bar(stat = "identity") + coord_flip() + coord_cartesian(ylim = c(0.9*min(res$auc), 1.0))



