
library(tidyverse);library(mlr)

list.files(path = "../input")
## Saving data
parallel::detectCores()
# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.

dt <- read.csv("../input/train.csv")


dt$PassengerId <- NULL
dt$Name <- NULL
dt$Ticket <- NULL
dt$Cabin <- NULL

dt$Survived <- factor(dt$Survived)
dt$Pclass <- factor(dt$Pclass)
dt$SibSp <- factor(dt$SibSp)
dt$Parch <- factor(dt$Parch)

tsk <- makeClassifTask(data=dt,target="Survived")
tsk <- smote(tsk,rate = 1.61)

lrn.cforest <- makeLearner("classif.cforest")
ps.cforest <- makeParamSet(
  makeIntegerParam("ntree",lower=500, upper=700),
  makeIntegerParam("mtry",lower=2, upper = 8)
)

ctrl <- makeTuneControlIrace(budget = 100)

parallelMap::parallelStartSocket(4)
rslt.cforest <- tuneParams(learner = lrn.cforest,
                           task=tsk,
                           resampling = cv3, 
                           par.set = ps.cforest, 
                           control = ctrl
                           )
parallelMap::parallelStop()

#classif.h2o.glm
#classif.h2o.randomForest
#classif.J48
#classif.naiveBayes
#classif.rpart
print(rslt.cforest)



