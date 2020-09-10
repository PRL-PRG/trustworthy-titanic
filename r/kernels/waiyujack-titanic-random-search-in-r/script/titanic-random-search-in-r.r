
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.



packagestoinstall<-c('tidyverse','forcats','stringr','xgboost','MLmetrics')

sapply(packagestoinstall,require,character=T)


test <- read.csv('../input/test.csv', stringsAsFactors = FALSE)
train <- read.csv('../input/train.csv', stringsAsFactors = FALSE)

train$set <- "train"
test$set  <- "test"
test$Survived <- NA
full <- rbind(train, test)


# re-encode missing data as NA
full <- full %>%
  mutate_if(is.character, funs(replace(., .=="", NA)))


full <- full %>%
  mutate(
    Age = ifelse(is.na(Age), mean(full$Age, na.rm=TRUE), Age),
    `Age Group` = case_when(Age < 13 ~ "Age.0012", 
                            Age >= 13 & Age < 18 ~ "Age.1317",
                            Age >= 18 & Age < 60 ~ "Age.1859",
                            Age >= 60 ~ "Age.60Ov"))

full$Embarked <- replace(full$Embarked, which(is.na(full$Embarked)), 'S')

full <- full %>%
  mutate(Title = as.factor(str_sub(Name, str_locate(Name, ",")[, 1] + 2, str_locate(Name, "\\.")[, 1]- 1)))

title_tbl <- full %>%
  group_by(Title) %>%
  summarise(
    N = n()
  )

full <- full %>%
  mutate(`Family Size`  = as.numeric(SibSp) + as.numeric(Parch) + 1,
         `Family Group` = case_when(
           `Family Size`==1 ~ "single",
           `Family Size`>1 & `Family Size` <=3 ~ "small",
           `Family Size`>= 4 ~ "large"
         ))

full <- full %>%
  mutate(Survived = case_when(Survived==1 ~ "Yes", 
                              Survived==0 ~ "No"))

#save.image('FullData.RData')

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

data_train <- full_final %>%
  filter(!is.na(Survived)) 

data_test  <- full_final %>% 
  filter(is.na(Survived))


data_train %>% colnames()

data_train <- apply(data_train, 2, as.numeric)

# index = sample(1:nrow(train), nrow(train)*0.8, replace = F)
# train.tune = data_train[index, ]
# train.valid = data_train[-index, ]
# train.tune.y = train.tune[,1]
# train.valid.y = train.valid[,1]

dtrain = xgb.DMatrix(data = data_train[,-c(1,2)], label = data_train[,2])

best.param = list()
best.seed = 0
best.auc = 0
best.auc.index = 0

### XGBoost random grid search
timer<-system.time({
    
    for (iter in 1:50){
  param = list(objective = 'binary:logistic',
               eval_metric = 'logloss',
               eval_metric = 'auc',
               max_depth = sample(4:8, 1),
               eta = round(runif(1, 0.01, 0.03), 4),
               gamma = round(runif(1, 0.0, 0.2), 4),
               subsample = round(runif(1, 0.6, 0.9), 4),
               colsample_bytree = round(runif(1, 0.5, 0.8), 4),
               min_child_weight = sample(1:40, 1),
               max_delta_step = sample(1:10, 1)
  )
  seed.number = sample.int(1000, 1)[[1]]
  set.seed(seed.number)
  cat("Iteration", iter, "for random grid search. \n")
  cv = xgb.cv(params = param, data = dtrain,
              nfold = 5, nrounds = 1000,
              verbose = T, early.stop.round = 100,
              maximize = T)
  max.auc = max(cv$`evaluation_log`[, test_auc_mean])
  max.auc.index = which.max(cv$`evaluation_log`[, test_auc_mean])
  
  if (max.auc > best.auc){
    best.auc = max.auc
    best.auc.index = max.auc.index
    best.seed = seed.number
    best.param = param
  }
  cat("", sep = "\n\n")
}
    
})



set.seed(best.seed)
xgb.valid.fit = xgb.train(data = dtrain, params = best.param,
                          nrounds = best.auc.index, verbose = T, maximize = F)


data_test <- apply(data_test, 2, as.numeric) 
data_test %>% head()
test_id<-data_test[,1]

data_test <- apply(data_test, 2, as.numeric)

# index = sample(1:nrow(train), nrow(train)*0.8, replace = F)
# train.tune = data_train[index, ]
# train.valid = data_train[-index, ]
# train.tune.y = train.tune[,1]
# train.valid.y = train.valid[,1]
pred_test<-predict(xgb.valid.fit,data_test[,-c(1,2)])

pred_test %>% head()

y_tain_pred<-predict(xgb.valid.fit,data_train[,c(-1,-2)])

pROC::auc(data_train[,2],y_tain_pred)
mean(-(data_train[,2]*log(y_tain_pred)+(1-data_train[,2])*(log(1-y_tain_pred))))

prediction<-data.frame(PassengerID = test_id, Survived = as.numeric(pred_test>0.5))
prediction %>% head(20)
write.csv(prediction,"submission.csv")




