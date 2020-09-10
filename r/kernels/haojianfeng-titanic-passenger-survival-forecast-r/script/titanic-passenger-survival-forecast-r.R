## ---- message = FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 可视化包
library('ggplot2')          
library('ggthemes')         
library('scales')     
library('dplyr')        # 数据处理包
library('rpart')        # 缺失值填补
library('randomForest') # 随机森林包


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', stringsAsFactors = F)


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(train)


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test <- read.csv('../input/test.csv', stringsAsFactors = F)
test$Survived <- NA
# rbind函数需要两个数据集列数相同，所以创建test数据集的Survived变量
train_test <- rbind(train,test)


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sapply(train_test,function(x) sum(is.na(x)))
sapply(train_test,function(x) sum(x == ""))


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 查看Fare缺失值的乘客基本信息
faremiss <- which(is.na(train_test$Fare))
train_test[faremiss,]


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Fare1 <- ggplot(train_test[train_test$Pclass=='3' & train_test$Embarked=='S' & train_test$Age>=50 ,],
       aes( x=Fare )) +
  geom_density( fill = '#99d6ff',alpha=0.4 ) +
  geom_vline(aes(xintercept=median(Fare,na.rm=T)),colour='red',linetype='dashed',led=1 ) +
  ggtitle("Fare1:Age considered") +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
Fare2 <- ggplot(train_test[train_test$Pclass=='3' & train_test$Embarked=='S',],
       aes( x=Fare )) +
  geom_density( fill = '#99d6ff',alpha=0.4 ) +
  geom_vline(aes(xintercept=median(Fare,na.rm=T)),colour='red',linetype='dashed',led=1 ) +
  ggtitle("Fare2:Regardless of age") + 
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
# 载入分面数据包gridExtra
library(gridExtra)
grid.arrange(Fare1, Fare2, ncol=2, nrow=1)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Fare1 <- median(train_test[train_test$Pclass=='3' & train_test$Embarked=='S' & train_test$Age>=50 ,]$Fare,
                na.rm = TRUE)
Fare2 <- median(train_test[train_test$Pclass=='3' & train_test$Embarked=='S',]$Fare, na.rm = TRUE)
Fare1
Fare2


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_test$Fare[faremiss] <- 8.00
# 可视化一下相关度
ggplot(train_test[1:891,], aes(x = Fare, color = factor(Survived))) +
  geom_line(stat='count', position='dodge') +
  theme_few()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
embarkedmiss <- which(train_test$Embarked=="")
train_test[embarkedmiss,]


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 去除带有缺失值的乘客的观测行
library('dplyr')
embark_fare <- train_test %>%
  filter(PassengerId != 62 & PassengerId != 830)

# 用ggplot2来可视化登船渡口与舱位等级和票价的关系
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_test$Embarked[c(62, 830)] <- 'C'
train_test$Embarked <- factor(train_test$Embarked)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 创建家庭大小变量
train_test$Fsize <- train_test$SibSp + train_test$Parch + 1
# 用ggplot2可视化存活与否与家庭大小的关系
ggplot(train_test[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 将家庭大小变量离散化，分成个人、小型家庭和大型家庭三个等级
train_test$FsizeD[train_test$Fsize == 1] <- 'singleton'
train_test$FsizeD[train_test$Fsize < 5 & train_test$Fsize > 1] <- 'small'
train_test$FsizeD[train_test$Fsize > 4] <- 'large'
# 转化为因子变量
train_test$FsizeD <- factor(train_test$FsizeD)

# 通过马赛克图来展示不同家庭规模幸存情况
mosaicplot(table(train_test[1:891,]$FsizeD, train_test[1:891,]$Survived), main='Family Size by Survival',
           shade=TRUE)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 从乘客名字中分离出头衔
train_test$Ptitle <- gsub('(.*, )|(\\..*)', '', train_test$Name)
# 根据性别显示头衔
table(train_test$Sex, train_test$Ptitle)
# 整合人数小的特殊头衔群体
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# 整合语意重复头衔
train_test$Ptitle[train_test$Ptitle == 'Mlle']        <- 'Miss' 
train_test$Ptitle[train_test$Ptitle == 'Ms']          <- 'Miss'
train_test$Ptitle[train_test$Ptitle == 'Mme']         <- 'Mrs' 
train_test$Ptitle[train_test$Ptitle %in% rare_title]  <- 'Rare Title'
# 转化为因子变量
train_test$Ptitle <- factor(train_test$Ptitle)
# 再次根据性别显示头衔
table(train_test$Sex, train_test$Ptitle)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
age_model <- rpart(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked+Ptitle+FsizeD,
                   data=train_test[!is.na(train_test$Age),],method='anova')
train_test$Age[is.na(train_test$Age)] <- predict(age_model,train_test[is.na(train_test$Age),])


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 首先我们了解一下年龄和幸存情况的关系
ggplot(train_test[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # 将性别也包括在内，因为性别也是一个重要的影响因素
  facet_grid(.~Sex) + 
  theme_few()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 创建分辨孩子的列，并判断是孩子还是成人
train_test$Age_group[train_test$Age <= 12] <- 'Child'
train_test$Age_group[train_test$Age > 12 & train_test$Age < 18] <- 'youth'
train_test$Age_group[train_test$Age >= 18] <- 'Adult'
# 转化为因子变量
train_test$Age_group  <- factor(train_test$Age_group)

# 可视化统计结果
mosaicplot(table(train_test$Age_group,
                 train_test$Survived),main='Comparison of child and adult',
           color=c("pink","lightblue"))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 加入母亲变量
train_test$Mother <- 'Not Mother'
train_test$Mother[train_test$Sex == 'female' & train_test$Parch > 0 & train_test$Age > 18 & 
                  train_test$Ptitle != 'Miss'] <- 'Mother'
# 可视化统计结果
mosaicplot(table(train_test$Mother,train_test$Survived),
           main='Comparison of mother and non mother',
           color=c("pink","lightblue"))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 转化为因子变量
train_test$Mother <- factor(train_test$Mother)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_test$Sex <- factor(train_test$Sex)
train <- train_test[1:891,]
test <- train_test[892:1309,]
# 设置随机种子
set.seed(754)
# 训练模型
rf_model <- randomForest(factor(Survived) ~ Sex + Ptitle + Pclass + Embarked +
                                            Age_group + Mother + Fare + FsizeD,data = train)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
    y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
    hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prediction <- predict(rf_model, test)

solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

write.csv(solution, file = 'gender_submission.csv', row.names = F)

