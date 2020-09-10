

#input package
library(ggplot2)
library(ggthemes)
library(stringr)
library(dplyr)
library(nycflights13)
library(readr)
library(rJava)
library(xlsxjars)
library(xlsx)
library(openxlsx)
library(mice)
library(randomForest)
library(scales)


newdata <- read.csv("train.csv")

str(newdata)
head(newdata)

newdata$Title <- gsub('(.*, )|(\\..*)', '', newdata$Name)  #用正则表达式匹配乘客的称谓，定义为一个新的向量

table(newdata$Sex,newdata$Title)     #根据性别生成一个数据框显示各个头衔出现的次数

hanjian_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                   'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')                    #把罕见稀少的称谓合并

newdata$Title[newdata$Title == "mlle"] <- "Miss"
newdata$Title[newdata$Title == "Ms"] <- "Miss"
newdata$Title[newdata$Title == "Mme"] <- "Mrs"
newdata$Title[newdata$Title %in% hanjian_title] <- "Hanjian_title"      #把罕见稀少的称谓合并,把女士的称谓同意

table(newdata$Sex,newdata$Title)     #统计一下合并后的称谓的性别人数

is.character(newdata$Name)                       #姓名列不是字符类型
newdata$Name <- as.character(newdata$Name)       #转换位字符串类型

newdata$Surname <- sapply(newdata$Name,function(x) strsplit(x,split = "[,.]")[[1]][1])  #抓取乘客的姓氏



?strsplit
cat(paste("we have <b>",nlevels(factor(newdata$Surname)),"</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time."))
#cat(paste(letters, 100* 1:26), fill = TRUE, labels = paste0("{", 1:10, "}:"))


#把有家庭成员的乘客组成家庭，包含他自己
newdata$jia <- newdata$SibSp + newdata$Parch + 1

#创建一个家庭变量
newdata$jiaating <- paste(newdata$Surname,newdata$jia,sep = "_")

ggplot(newdata[1:891,],aes(x = jia,Fill = factor(Survived))) +
  geom_bar(stat = "count",position = "dodge") +
  scale_x_continuous(breaks = c(1:11)) +
  labs(x = "family size") +
  theme_few()

#根据家庭成员多少分三类
newdata$jiaD[newdata$jia == 1] <- "danshengou"
newdata$jiaD[newdata$jia < 5 & newdata$jia > 1] <- "xiaojia"
newdata$jiaD[newdata$jia > 5] <- "dajia"

#绘图
mosaicplot(table(newdata$jiaD,newdata$Survived),main = "家庭大小和幸存的关系",shade = TRUE)

#查看甲板
newdata$Cabin[1:28]
#转换数据类型
newdata$Cabin <- as.character(newdata$Cabin)

#查看甲板向量的第二个值的名字
strsplit(newdata$Cabin[2],NULL)[[1]]
#创建一个以甲板名称第一个字母为变量的列
newdata$jban <- factor(sapply(newdata$Cabin,function(x) strsplit(x,NULL)[[1]][1]))   #暂时停止



######补充缺失值

sapply(newdata,function(x) sum(is.na(x))) 
sapply(newdata,function(x) sum(x == ""))



which(newdata$Embarked %in% "")    #出发港口的缺失值
newdata[c(62,830),"Fare"]      #62和830号乘客的票价是80$
piaojia2 <- newdata %>%  filter(PassengerId != 62 & PassengerId != 830)  #排除62和830号乘客

ggplot(piaojia2,aes(x = Embarked,y = Fare,Fill = factor(Pclass))) +      #ggplot绘图
  geom_boxplot() +
  geom_hline(aes(yintercept = 80),
             colour = "red",linetype = "dashed",lwd = 2) +
  scale_y_continuous(labels = dollar_format()) +
  theme_classic()
newdata$Embarked[c(62,830)] <- "C"    #62和830号乘客添加出发港“C”

newdata[1044,]

ggplot(newdata[newdata$Pclass == "3" & newdata$Embarked == "S",],aes(x = Fare)) +
  geom_density(Fill = "#996600",alpha = 0.4) +
  geom_vline(aes(xintercept = median(Fare,na.rm = T)),colour = "red",linetype = "dashed",lwd = 1) +
  scale_x_continuous(labels = dollar_format()) +
  theme_few()

newdata$Fare[1044] <- median(newdata[newdata$Pclass == "3" & newdata$Embarked == "S",]$Fare,na.rm = TRUE)


#统计年龄列有多少空值
sum(is.na(newdata$Age))
#定义一个列名向量
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Title','Surname','jiaating',"jiaD")
newdata[factor_vars] <- lapply(newdata[factor_vars],function(x) as.factor(x))

#设置随机种子
set.seed(129)
#建立模型
mice_mod <- mice(newdata[,!names(newdata) %in% c("PassengerId","Name","Ticket",'Cabin','jiaating','Surname','Survived')],method = 'rf')
#把模型引入源数据后填充年龄，得到新的数据
mice_output <- complete(mice_mod)

#用原来的年龄数据绘图
par(mfrow = c(1,2))
hist(newdata$Age, freq = F, main = 'Age: Original Data', col = 'darkgreen', ylim = c(0,0.04))
#补充年龄值后的数据绘图，进行对比
par(mfrow = c(1,2))
hist(mice_output$Age,freq = F,main = 'Age original data',col = 'darkblue',ylim = c(0,0.04))

#把补充完整的年龄数据写入原来的数据
newdata$Age <- mice_output$Age
#检查一下是否还有空的年龄值
sum(is.na(newdata$Age))

#再看一下年龄和幸存的关系
ggplot(newdata[1:891,],aes(Age,Fill = factor(Survived))) +
  geom_histogram() +
  facet_grid(.~Sex) +
  theme_few()
#把乘客分为大人和小孩
newdata$child[newdata$Age < 18] <- 'child'
newdata$child[newdata$Age >= 18] <- 'adult'
#统计人数
table(newdata$child,newdata$Survived)

#母亲获救的几率是否更高？
newdata$mother <- 'not mother'
newdata$mother[newdata$Sex == 'female' & newdata$Parch > 0 & newdata$Age > 18 & newdata$Title != 'Miss'] <- 'mother'

#统计人数
table(newdata$mother,newdata$Survived)

#生成了两个新的因子变量
newdata$child <- factor(newdata$child)
newdata$mother <- factor(newdata$mother)
#检查是否还有丢失的数值或者变量
md.pattern(newdata)


###随机森林分析
#把数据分为两部分
train <- newdata[1:891,]
test <- newdata[892:1309,]
##建立模型
#设置随机种子
set.seed(754)

#用一部分变量建立模型
rf_model <- randomForest(factor(Survived) ~Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + jia + child + mother,data = train)
#显示模型错误
plot(rf_model,ylim = c(0,0.36))
legend('topright',colnames(rf_model$err.rate),col = 1:3,fill = 1:3)

##各个变量的重要性
#
importance <- importance(rf_model)
varImportance <- data.frame(variables = row.names(importance),importance = round(importance[,"MeanDecreaseGini"],2))

#创建一个基于重要性的变量
rankImportance <- varImportance %>% mutate(rank = paste0('#',dense_rank(desc(importance))))

#用ggplot根据变量影响的重要性绘图
ggplot(rankImportance,aes(x = reorder(variables,importance),
                          y = importance,Fill = importance)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = variables,y = 0.5,label = rank),
            hjust = 0, vjust = 0.55,size = 4,colour = 'red') +
  labs(x = 'variables') +
  coord_flip() +
  theme_few()

#使用测试集合进行预测
prediction <- predict(rf_model,test)

solution <- data.frame(PassengerID = test$PassengerId,Survived = prediction)

write.csv(solution,file = "F:\\bigdata\\output\\rf_model_solution.csv",row.names = FALSE)
