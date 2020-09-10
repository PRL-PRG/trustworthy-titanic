## ----setup, include=FALSE, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo=TRUE)



## ----dependencies, message = FALSE, warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# data wrangling
library(tidyverse)
library(forcats)
library(stringr)

# data assessment/visualizations
library(DT)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(corrplot)
library(VIM) 
library(knitr)
library(vcd)

# model
library(xgboost)
library(MLmetrics)


## ----data, message=FALSE, warning=FALSE, results='hide'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

train <- read_csv('../input/train.csv')
test  <- read_csv('../input/test.csv')

train$set <- "train"
test$set  <- "test"
test$Survived <- NA
full <- rbind(train, test)

kable(head(full, 10), caption="Data Set", format="markdown")



## ----structure, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

glimpse(full)



## ----missing, message=FALSE, warning=FALSE, fig.height=5, fig.width=9------------------------------------------------------------------------------------------------------------------------------------------------------------

# re-encode missing data as NA
full <- full %>%
  mutate_if(is.character, funs(replace(., .=="", NA)))

# visualize missing data
missplot <- aggr(full %>% select(-Survived), prop=FALSE, combined=TRUE, numbers=TRUE, sortVars=TRUE, sortCombs=TRUE, plot=FALSE)

plot(missplot)


## ----age, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9---------------------------------------------------------------------------------------------------------------------------------------------------

full <- full %>%
    mutate(
      Age = ifelse(is.na(Age), mean(full$Age, na.rm=TRUE), Age),
      `Age Group` = case_when(Age < 13 ~ "Age.0012", 
                                 Age >= 13 & Age < 18 ~ "Age.1317",
                                 Age >= 18 & Age < 60 ~ "Age.1859",
                                 Age >= 60 ~ "Age.60Ov"))



## ----pp_embarked, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9-------------------------------------------------------------------------------------------------------------------------------------------

full$Embarked <- replace(full$Embarked, which(is.na(full$Embarked)), 'S')



## ----pp_titles, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9---------------------------------------------------------------------------------------------------------------------------------------------

full <- full %>%
  mutate(Title = as.factor(str_sub(Name, str_locate(Name, ",")[, 1] + 2, str_locate(Name, "\\.")[, 1]- 1)))

title_tbl <- full %>%
  group_by(Title) %>%
  summarise(
    N = n()
  )



## ----pp_familygrp, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9------------------------------------------------------------------------------------------------------------------------------------------

full <- full %>%
  mutate(`Family Size`  = as.numeric(SibSp) + as.numeric(Parch) + 1,
         `Family Group` = case_when(
           `Family Size`==1 ~ "single",
           `Family Size`>1 & `Family Size` <=3 ~ "small",
           `Family Size`>= 4 ~ "large"
         ))



## ----iv, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

full <- full %>%
  mutate(Survived = case_when(Survived==1 ~ "Yes", 
                              Survived==0 ~ "No"))

crude_summary <- full %>%
  filter(set=="train") %>%
  select(PassengerId, Survived) %>%
  group_by(Survived) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

crude_survrate <- crude_summary$freq[crude_summary$Survived=="Yes"]

kable(crude_summary, caption="2x2 Contingency Table on Survival.", format="markdown")



## ----rate_pclass, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9-------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train"), aes(Pclass, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Class") + 
  theme_minimal()


## ----rate_sex, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9----------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train"), aes(Sex, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Sex") + 
  theme_minimal()


## ----rate_age, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9----------------------------------------------------------------------------------------------------------------------------------------------

tbl_age <- full %>%
  filter(set=="train") %>%
  select(Age, Survived) %>%
  group_by(Survived) %>%
  summarise(mean.age = mean(Age, na.rm=TRUE))

ggplot(full %>% filter(set=="train"), aes(Age, fill=Survived)) +
  geom_histogram(aes(y=..density..), alpha=0.5) +
  geom_density(alpha=.2, aes(colour=Survived)) +
  geom_vline(data=tbl_age, aes(xintercept=mean.age, colour=Survived), lty=2, size=1) +
  scale_fill_brewer(palette="Set1") +
  scale_colour_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Density") +
  ggtitle("Survival Rate by Age") + 
  theme_minimal()


## ----rate_age_group, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9----------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train" & !is.na(Age)), aes(`Age Group`, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Age Group") + 
  theme_minimal()


## ----rate_sibsp, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9--------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train"), aes(SibSp, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by SibSp") + 
  theme_minimal()


## ----rate_parch, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9--------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train"), aes(Parch, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Parch") + 
  theme_minimal()


## ----rate_embarked, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9-----------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train"), aes(Embarked, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Embarked") + 
  theme_minimal()


## ----rate_title, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9--------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train") %>% na.omit, aes(Title, fill=Survived)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Title") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## ----rate_family, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9-------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train") %>% na.omit, aes(`Family Group`, fill=Survived)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Family Group") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## ----freq_pclass, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9-------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train"), aes(Pclass, fill=Survived)) +
  geom_bar(position="stack") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=comma) +
  ylab("Passengers") +
  ggtitle("Survived by Class") + 
  theme_minimal()


## ----freq_sex, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9----------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train"), aes(Sex, fill=Survived)) +
  geom_bar(position="stack") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  scale_y_continuous(labels=comma) +
  ylab("Passengers") +
  ggtitle("Survived by Sex") + 
  theme_minimal()


## ----freq_age, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9----------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train"), aes(Age, fill=Survived)) +
  geom_histogram(aes(y=..count..), alpha=0.5) +
  geom_vline(data=tbl_age, aes(xintercept=mean.age, colour=Survived), lty=2, size=1) +
  scale_fill_brewer(palette="Set1") +
  scale_colour_brewer(palette="Set1") +
  scale_y_continuous(labels=comma) +
  ylab("Density") +
  ggtitle("Survived by Age") + 
  theme_minimal()


## ----freq_age_group, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9----------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train" & !is.na(Age)), aes(`Age Group`, fill=Survived)) +
  geom_bar(position="stack") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=comma) +
  ylab("Passengers") +
  ggtitle("Survived by Age Group") + 
  theme_minimal()


## ----freq_sibsp, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9--------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train"), aes(SibSp, fill=Survived)) +
  geom_bar(position="stack") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  scale_y_continuous(labels=comma) +
  ylab("Passengers") +
  ggtitle("Survived by SibSp") + 
  theme_minimal()


## ----freq_parch, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9--------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train"), aes(Parch, fill=Survived)) +
  geom_bar(position="stack") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=comma) +
  ylab("Passengers") +
  ggtitle("Survived by Parch") + 
  theme_minimal()


## ----freq_embarked, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9-----------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train"), aes(Embarked, fill=Survived)) +
  geom_bar(position="stack") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=comma) +
  ylab("Passengers") +
  ggtitle("Survived by Embarked") + 
  theme_minimal()


## ----freq_title, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9--------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train") %>% na.omit, aes(Title, fill=Survived)) +
  geom_bar(position="stack") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=comma) +
  ylab("Passengers") +
  ggtitle("Survived by Title") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## ----freq_family, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4.5, fig.width=9-------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full %>% filter(set=="train") %>% na.omit, aes(`Family Group`, fill=Survived)) +
  geom_bar(position="stack") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=comma) +
  ylab("Passengers") +
  ggtitle("Survived by Family Group") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## ----corrplot, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4, fig.width=9------------------------------------------------------------------------------------------------------------------------------------------------

tbl_corr <- full %>%
  filter(set=="train") %>%
  select(-PassengerId, -SibSp, -Parch) %>%
  select_if(is.numeric) %>%
  cor(use="complete.obs") %>%
  corrplot.mixed(tl.cex=0.85)



## ----mosaicplot, message=FALSE, warning=FALSE, echo=TRUE, fig.height=4, fig.width=9----------------------------------------------------------------------------------------------------------------------------------------------

tbl_mosaic <- full %>%
  filter(set=="train") %>%
  select(Survived, Pclass, Sex, AgeGroup=`Age Group`, Title, Embarked, `Family Group`) %>%
  mutate_all(as.factor)

mosaic(~Pclass+Sex+Survived, data=tbl_mosaic, shade=TRUE, legend=TRUE)



## ----alluvial, message=FALSE, warning=FALSE, echo=TRUE, fig.height=6, fig.width=9------------------------------------------------------------------------------------------------------------------------------------------------
library(alluvial)

tbl_summary <- full %>%
  filter(set=="train") %>%
  group_by(Survived, Sex, Pclass, `Age Group`, Title) %>%
  summarise(N = n()) %>% 
  ungroup %>%
  na.omit
  
alluvial(tbl_summary[, c(1:4)],
         freq=tbl_summary$N, border=NA,
         col=ifelse(tbl_summary$Survived == "Yes", "blue", "gray"),
         cex=0.65,
         ordering = list(
           order(tbl_summary$Survived, tbl_summary$Pclass==1),
           order(tbl_summary$Sex, tbl_summary$Pclass==1),
           NULL,
           NULL))


## ----party, message=FALSE, warning=FALSE, echo=TRUE, fig.height=7, fig.width=9---------------------------------------------------------------------------------------------------------------------------------------------------
library(party)

tbl_party <- full %>%
  filter(set=="train") %>%
  select(Survived, Pclass, Sex, `Age Group`, Title, Embarked, `Family Group`) %>%
    mutate_all(as.factor) 

ctree_mdl <- ctree(Survived ~ ., 
                   data=tbl_party %>% na.omit, 
                   controls=ctree_control(testtype="Bonferroni"))

plot(ctree_mdl)


## ----xgb, message=FALSE, warning=FALSE, echo=TRUE, fig.height=7, fig.width=9-----------------------------------------------------------------------------------------------------------------------------------------------------

full_2 <- full %>% 
  select(-Name, -Ticket, -Cabin, -set) %>%
  mutate(
    Survived = ifelse(Survived=="Yes", 1, 0)
  ) %>% 
  rename(AgeGroup=`Age Group`, FamilySize=`Family Size`, FamilyGroup=`Family Group`)

# OHE
ohe_cols <- c("Pclass", "Sex", "Embarked", "Title", "AgeGroup", "FamilyGroup")
num_cols <- setdiff(colnames(full_2), ohe_cols)

full_final <- subset(full_2, select=num_cols)

for(var in ohe_cols) {
  values <- unique(full_2[[var]])
  for(j in 1:length(values)) {
    full_final[[paste0(var,"_",values[j])]] <- (full_2[[var]] == values[j]) * 1
  }
}

submission <- TRUE

data_train <- full_final %>%
  filter(!is.na(Survived)) 

data_test  <- full_final %>% 
  filter(is.na(Survived))

set.seed(777)
ids <- sample(nrow(data_train))

# create folds for cv
n_folds <- ifelse(submission, 1, 5)

score <- data.table()
result <- data.table()

for(i in 1:n_folds) {
  
  if(submission) {
    x_train <- data_train %>% select(-PassengerId, -Survived)
    x_test  <- data_test %>% select(-PassengerId, -Survived)
    y_train <- data_train$Survived
    
  } else {
    train.ids <- ids[-seq(i, length(ids), by=n_folds)]
    test.ids  <- ids[seq(i, length(ids), by=n_folds)]
    
    x_train <- data_train %>% select(-PassengerId, -Survived)
    x_train <- x_train[train.ids,]
    
    x_test  <- data_train %>% select(-PassengerId, -Survived)
    x_test  <- x_test[test.ids,]
    
    y_train <- data_train$Survived[train.ids]
    y_test  <- data_train$Survived[test.ids]
  }
  
  x_train <- apply(x_train, 2, as.numeric)
  x_test <- apply(x_test, 2, as.numeric)
  
  if(submission) {
    nrounds <- 12
    early_stopping_round <- NULL
    dtrain <- xgb.DMatrix(data=as.matrix(x_train), label=y_train)
    dtest <- xgb.DMatrix(data=as.matrix(x_test))
    watchlist <- list(train=dtrain)
  } else {
    nrounds <- 3000
    early_stopping_round <- 100
    dtrain <- xgb.DMatrix(data=as.matrix(x_train), label=y_train)
    dtest <- xgb.DMatrix(data=as.matrix(x_test), label=y_test)
    watchlist <- list(train=dtrain, test=dtest)
  }
  
  params <- list("eta"=0.01,
                 "max_depth"=8,
                 "colsample_bytree"=0.3528,
                 "min_child_weight"=1,
                 "subsample"=1,
                 "objective"="reg:logistic",
                 "eval_metric"="auc")
  
  model_xgb <- xgb.train(params=params,
                         data=dtrain,
                         maximize=TRUE,
                         nrounds=nrounds,
                         watchlist=watchlist,
                         early_stopping_round=early_stopping_round,
                         print_every_n=2)
  
  pred <- predict(model_xgb, dtest)
  
  if(submission) {
    result <- cbind(data_test %>% select(PassengerId), Survived=round(pred, 0))
  } else {
    score <- rbind(score, 
                   data.frame(accuracy=Accuracy(round(pred, 0), y_test), best_iteration=model_xgb$best_iteration))
    temp   <- cbind(data_train[test.ids,], pred=pred)
    result <- rbind(result, temp)
  }
}

if(submission) {
  write.csv(result, "submission.csv", row.names=FALSE)
} else {
  print(mean(score$accuracy))
  print(score$accuracy)
  print(Accuracy(result$Survived, round(result$pred, 0)))
}


