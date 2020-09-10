
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(stringr)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

print(system2("ls",c("../input"),stdout=T))

# Any results you write to the current directory are saved as output.

library(tidyverse)
library(mlr)
library(janitor)

train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")

data <- train %>% bind_rows(test,.id="type")

#test$Survived[test$Sex=='male'] <- 0
#test$Survived[test$Sex=='female'] <- 1
#submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
#write.csv(submit,"genderModel.csv",row.names=F)

glimpse(data)

data <- data %>%
  mutate_at(c("Survived"),as.factor) %>%
  mutate_at(c("Name","Sex","Ticket","Cabin","Embarked","Pclass","SibSp","Parch","type"),as.factor)




summary(data)

data <- data %>% mutate(honorific=factor(str_match(Name,"(\\w*)\\.")[,1])) %>% 
   mutate(honorific=fct_lump(honorific,4)) %>%
   mutate(Parch=fct_lump(Parch,3)) %>%
   mutate(SibSp= fct_lump(SibSp,2)) %>%
   mutate(Fare=ifelse(Fare==0,min(Fare),Fare)) %>%
   mutate(Deck=as.factor(str_match(data$Cabin,"(.{1}).*")[,2]),
          Deck=fct_lump(Deck,6),
          Deck=fct_explicit_na(Deck,na_level = "noCabin")
         ) %>%
   mutate(familiy=factor(str_match(Name,"(\\w*),")[,1])) %>%
   mutate(Cabin=as.numeric(str_match(data$Cabin,"(.{1})(.*)")[,3]))



summary(data)


glimpse(data)

familySize = data %>% group_by(familiy) %>% count %>%
   rename(familiy_size=n)
summary(familySize)

ageByHonorific <- data %>% group_by(honorific) %>% summarise(age.mean=mean(Age,na.rm = T))

data <- data %>% 
    left_join(ageByHonorific) %>% 
    mutate(Age=ifelse(is.na(Age),age.mean,Age)) %>% 
    select(-age.mean) %>%
    left_join(familySize) %>%
    select(-familiy)
summary(data)

data.forModel <- data  %>% select(-Name,-Ticket,-Sex) 
data.forModel.imputed <- impute(data.forModel,cols=
                                     list(Age=imputeLearner("regr.randomForestSRC"),
                                         Fare=imputeMean(),
                                         Cabin=imputeMean(),               
                                         Embarked=imputeMode()))$data
summary(data.forModel.imputed)

survived=data.forModel.imputed$Survived
data.forModel.imputed$Survived=NULL
type=data.forModel.imputed$type
data.forModel.imputed$type=NULL
passengerId=data.forModel.imputed$PassengerId
data.forModel.imputed$PassengerId=NULL
glimpse(data.forModel.imputed)

train.tmp=data.forModel.imputed[type==1,]
train.tmp$Survived=survived[type==1]


boxplot(data$Fare ~ data$Pclass)

library(corrplot)
sjPlot::sjp.xtab(train.tmp$Deck,train.tmp$Survived,show.total = FALSE,bar.pos = "stack",margin = "row",)

data.forModel.imputed <- normalizeFeatures(data.forModel.imputed)
summary(data.forModel.imputed)

data.forModel.imputed.dummy <- createDummyFeatures(data.forModel.imputed)
glimpse(data.forModel.imputed.dummy)



train=data.forModel.imputed.dummy[type=="1",]
train$Survived=survived[type=="1"]
glimpse(train)
test=data.forModel.imputed.dummy[type=="2",]
glimpse(test)


task <- makeClassifTask(data = train ,target = "Survived" )
print(task)

fv = generateFilterValuesData(task, method = "information.gain")
plotFilterValues(fv)



set.seed(1234)

rdesc = cv10

lrns = makeLearners(c("classif.randomForestSRC","classif.ada",
                    "classif.featureless","classif.glmnet","classif.LiblineaRL1L2SVC","classif.LiblineaRL1LogReg",
                      "classif.randomForest","classif.boosting","classif.svm","classif.xgboost",
                      "classif.extraTrees","classif.knn","classif.mlp"))
bmr1 = benchmark(lrns, task, rdesc,measures = list(acc))


lrns=lapply(c("classif.randomForestSRC","classif.xgboost",
              "classif.ada",
              "classif.LiblineaRL1LogReg"),makeLearner)
lrns=lapply(lrns,setPredictType,"prob")
stacked.learner=makeStackedLearner(base.learners = lrns,method = "average",predict.type = "prob")
bmr.stacked = benchmark(stacked.learner, task, rdesc,measures = list(acc))


bmr.merged=mergeBenchmarkResults(list(bmr1,bmr.stacked))

getBMRPerformances(bmr.merged,as.df = T) %>% arrange(desc(acc)) 

plotBMRBoxplots(bmr.merged)

bench.perfs <- getBMRAggrPerformances(bmr.merged,as.df = T)   %>% arrange(desc(acc.test.mean))
bench.perfs

bestLearner <- bench.perfs$learner.id[[1]]
bestLearner

#lrns.toTune = list(
#  makeLearner("classif.randomForestSRC")
#)
#lrn.mm = makeModelMultiplexer(lrns.toTune)
#lrn.mm

#ps = makeModelMultiplexerParamSet(lrn.mm,
# makeDiscreteParam("ntree",c(10,50,100,1000,2000)),
# makeDiscreteParam("bootstrap" ,c("by.root", "by.node", "none")),
# makeIntegerParam("mtry", lower = 3, upper = 10),
# makeIntegerParam("nodesize", lower = 10, upper = 50)                                  
#)
#ps
                   
                   
                                  


#ctrl = makeTuneControlIrace(maxExperiments=200)
#ctrl

#tune.res = tuneParams(lrn.mm, task = task, resampling = rdesc,
#  par.set = ps, control = ctrl,measures=list(acc))

#hyperParData <- generateHyperParsEffectData(tune.res,partial.dep = T)

#hyperParData$data %>% arrange(desc(acc.test.mean))

#plotHyperParsEffect(hyperParData, x = "classif.randomForestSRC.ntree", y = "acc.test.mean", plot.type = "line",
#  partial.dep.learn = "regr.randomForest")
#plotHyperParsEffect(hyperParData, x = "classif.randomForestSRC.bootstrap", y = "acc.test.mean", plot.type = "line",
#  partial.dep.learn = "regr.randomForest")
#plotHyperParsEffect(hyperParData, x = "classif.randomForestSRC.mtry", y = "acc.test.mean", plot.type = "line",
#  partial.dep.learn = "regr.randomForest")
#plotHyperParsEffect(hyperParData, x = "classif.randomForestSRC.nodesize", y = "acc.test.mean", plot.type = "line",
#  partial.dep.learn = "regr.randomForest")



#tune.res$x$selected.learner



#lrn.tuned.best = setHyperPars(makeLearner(tune.res$x$selected.learner), par.vals = tune.res$x)

bestLearner

bestModel=getBMRModels(bmr.merged,learner.ids = c(as.character(bestLearner)),drop = T)[[1]]

bestModel


rin2 = makeResampleDesc(method = "CV", iters = 5, predict = "both")
lc2 = generateLearningCurveData(learners = makeLearner("classif.randomForestSRC"), 
task = task,
  percs = seq(0.1, 1, by = 0.05),
  measures = list(acc, setAggregation(acc, train.mean)), resampling = rin2,
  show.info = TRUE)



plotLearningCurve(lc2, facet = "learner")


prediction=predict(bestModel,newdata = test %>% data.frame())

#prediction=predict(result$models[[1]],newdata = test %>% data.frame())

prediction

summary(prediction$data)

submission <- prediction$data %>% 
  select(response) %>%
  mutate(PassengerId=row.names(prediction$data)) %>%
  rename(Survived=response) %>%
  mutate(Survived=as.integer(as.character(Survived)),
        PassengerId=as.integer(PassengerId)) %>%
  select(PassengerId,Survived)

glimpse(submission)

write_csv(submission,"submission.csv")




