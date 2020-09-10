
pacman::p_load(rpart, tidyverse, gridExtra, xgboost, forcats, stringr, caTools, DT, data.table, pander, scales, grid, corrplot, VIM, knitr, vcd, caret, MLmetrics, randomForest, 
              rpart.plot, car, e1071, ROCR, pROC, glmnet)

getwd()

list.files('../input/')

train <- read_csv(file = '../input/titanic/train.csv')
test <- read_csv(file = '../input/titanic/test.csv')

train$set <- "train"
test$set  <- "test"
test$Survived <- NA
full <- rbind(train, test)

str(full)

glimpse(full)

summary(full)

str(test)

dim(full)

# Unique values per column
sapply(full, function(x) length(unique(x)))

#Check for Missing values
missing_values <- full %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

checkColumn = function(df,colname){
  testData = df[[colname]]
  numMissing = max(sum(is.na(testData)|is.nan(testData)|testData==''),0)
  if (class(testData) == 'numeric' | class(testData) == 'Date' | class(testData) == 'difftime' | class(testData) == 'integer'){
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = sum(is.infinite(testData)), 'avgVal' = mean(testData,na.rm=TRUE), 'minVal' = round(min(testData,na.rm = TRUE)), 'maxVal' = round(max(testData,na.rm = TRUE)))
  } else{
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = NA,  'avgVal' = NA, 'minVal' = NA, 'maxVal' = NA)
  }
}

checkAllCols = function(df){
  resDF = data.frame()
  for (colName in names(df)){
    resDF = rbind(resDF,as.data.frame(checkColumn(df=df,colname=colName)))
  }
  resDF
}

checkAllCols(full)

miss_pct <- map_dbl(full, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
miss_pct <- miss_pct[miss_pct > 0]

data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
    ggplot(aes(x=reorder(var, miss), y=miss)) + 
    geom_bar(stat='identity', fill='red') +
    labs(x='', y='% missing', title='Percent missing data by feature') +
    theme(axis.text.x=element_text(angle=90, hjust=1))

full <- full %>%
    mutate(
      Age = ifelse(is.na(Age), mean(full$Age, na.rm=TRUE), Age),
      `Age Group` = case_when(Age < 13 ~ "Age.0012", 
                                 Age >= 13 & Age < 18 ~ "Age.1317",
                                 Age >= 18 & Age < 60 ~ "Age.1859",
                                 Age >= 60 ~ "Age.60Ov"))

table(full$`Age Group`)

full$Embarked <- replace(full$Embarked, which(is.na(full$Embarked)), 'S')
table(full$Embarked)

names <- full$Name
title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)
full$title <- title
table(title)

### MISS, Mrs, Master and Mr은 더 많은 숫자를 사용하고 있습니다.
### 성별과 생존율을 확인하여 다른 과목을 더 큰 바구니에 그룹화하는 것이 더 좋음

full$title[full$title == 'Mlle']        <- 'Miss' 
full$title[full$title == 'Ms']          <- 'Miss'
full$title[full$title == 'Mme']         <- 'Mrs' 
full$title[full$title == 'Lady']          <- 'Miss'
full$title[full$title == 'Dona']          <- 'Miss'

# 나는 작은 데이터로 새로운 변수를 생성하는 것이 overfit을 야기 할 수 있다고 생각한다.

full$title[full$title == 'Capt']        <- 'Officer' 
full$title[full$title == 'Col']        <- 'Officer' 
full$title[full$title == 'Major']   <- 'Officer'
full$title[full$title == 'Dr']   <- 'Officer'
full$title[full$title == 'Rev']   <- 'Officer'
full$title[full$title == 'Don']   <- 'Officer'
full$title[full$title == 'Sir']   <- 'Officer'
full$title[full$title == 'the Countess']   <- 'Officer'
full$title[full$title == 'Jonkheer']   <- 'Officer'  

table(full$title)

full$FamilySize <-full$SibSp + full$Parch + 1 
full$FamilySized[full$FamilySize == 1] <- 'Single' 
full$FamilySized[full$FamilySize < 5 & full$FamilySize >= 2] <- 'Small' 
full$FamilySized[full$FamilySize >= 5] <- 'Big' 
full$FamilySized=as.factor(full$FamilySized)

plot(full$FamilySized)

##Engineer features based on all the passengers with the same ticket
ticket.unique <- rep(0, nrow(full))
tickets <- unique(full$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(full$Ticket == current.ticket)
  for (k in 1:length(party.indexes)) {
    ticket.unique[party.indexes[k]] <- length(party.indexes)
  }
}

full$ticket.unique <- ticket.unique
full$ticket.size[full$ticket.unique == 1]   <- 'Single'
full$ticket.size[full$ticket.unique < 5 & full$ticket.unique>= 2]   <- 'Small'
full$ticket.size[full$ticket.unique >= 5]   <- 'Big'

table(full$ticket.size)

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

g1_pclass <- ggplot(full %>% filter(set=="train"), aes(Pclass, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Class") + 
  theme_minimal()

g2_sex <- ggplot(full %>% filter(set=="train"), aes(Sex, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Sex") + 
  theme_minimal()

tbl_age <- full %>%
  filter(set=="train") %>%
  select(Age, Survived) %>%
  group_by(Survived) %>%
  summarise(mean.age = mean(Age, na.rm=TRUE))

g3_age <- ggplot(full %>% filter(set=="train"), aes(Age, fill=Survived)) +
  geom_histogram(aes(y=..density..), alpha=0.5) +
  geom_density(alpha=.2, aes(colour=Survived)) +
  geom_vline(data=tbl_age, aes(xintercept=mean.age, colour=Survived), lty=2, size=1) +
  scale_fill_brewer(palette="Set1") +
  scale_colour_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Density") +
  ggtitle("Survival Rate by Age") + 
  theme_minimal()

g4_agegroup <- ggplot(full %>% filter(set=="train" & !is.na(Age)), aes(`Age Group`, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Age Group") + 
  theme_minimal()

g5_sibsp <- ggplot(full %>% filter(set=="train"), aes(SibSp, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by SibSp") + 
  theme_minimal()

g6_parch <- ggplot(full %>% filter(set=="train"), aes(Parch, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Parch") + 
  theme_minimal()

g7_embarked <- ggplot(full %>% filter(set=="train"), aes(Embarked, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Embarked") + 
  theme_minimal()

g8_title <- ggplot(full %>% filter(set=="train") %>% na.omit, aes(title, fill=Survived)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Title") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

g9_family <- ggplot(full %>% filter(set=="train") %>% na.omit, aes(`FamilySize`, fill=Survived)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Family Group") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(g1_pclass, g2_sex)

grid.arrange(g3_age , g4_agegroup)

grid.arrange(g5_sibsp  , g6_parch)

grid.arrange(g7_embarked   , g8_title, g9_family )

tbl_corr <- full %>%
  filter(set=="train") %>%
  select(-PassengerId, -SibSp, -Parch) %>%
  select_if(is.numeric) %>%
  cor(use="complete.obs") %>%
  corrplot.mixed(tl.cex=0.85)

tbl_mosaic <- full %>%
  filter(set=="train") %>%
  select(Survived, Pclass, Sex, AgeGroup=`Age Group`, title, Embarked, `FamilySize`) %>%
  mutate_all(as.factor)

mosaic(~Pclass+Sex+Survived, data=tbl_mosaic, shade=TRUE, legend=TRUE)

library(alluvial)

tbl_summary <- full %>%
  filter(set=="train") %>%
  group_by(Survived, Sex, Pclass, `Age Group`, title) %>%
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

data <- rbind(train,test)
data$Surname = substring( data$Name,0,regexpr(',',data$Name)-1)
data$GroupId = paste( data$Surname, data$Pclass, sub('.$','X',data$Ticket), data$Fare, data$Embarked, sep='-')
data[c(195,1067,59,473,1142),c('Name','GroupId')]

# engineer titles
data$Title <- 'man'
data$Title[data$Sex=='female'] <- 'woman'
data$Title[grep('Master',data$Name)] <- 'boy'


# color variable is used in plots below
data$Color <- data$Survived


# engineer "woman-child-groups"
data$GroupId[data$Title=='man'] <- 'noGroup'
data$GroupFreq <- ave(1:1309,data$GroupId,FUN=length)
data$GroupId[data$GroupFreq<=1] <- 'noGroup'
data$TicketId = paste( data$Pclass,sub('.$','X',data$Ticket),data$Fare,data$Embarked,sep='-')
count = 0


# add nannies and relatives to groups
for (i in which(data$Title!='man' & data$GroupId=='noGroup')){
    data$GroupId[i] = data$GroupId[data$TicketId==data$TicketId[i]][1]
    if (data$GroupId[i]!='noGroup') {
        # color variable is used in plots below
        if (is.na(data$Survived[i])) data$Color[i] = 5
        else if (data$Survived[i] == 0) data$Color[i] = -1
        else if (data$Survived[i] == 1) data$Color[i] = 2
        count = count + 1
    }
}
cat(sprintf('우리는 %d 명의 보모/친척을 찾아 그룹에 추가했습니다.\n',count))

data$GroupName = substring( data$GroupId,0,regexpr('-',data$GroupId)-1)

table(data$GroupName)

data$Color[is.na(data$Color) & data$Title=='woman'] <- 3
data$Color[is.na(data$Color) & data$Title=='boy'] <- 4
x = data$GroupId[data$GroupId!='noGroup']; x = unique(x); x=x[order(x)]
plotData <- list(); g <- list()

for (i in 1:3) plotData[[i]] <- data[data$GroupId %in% x[(27*(i-1))+1:27],]
for (i in 1:3) g[[i]] = ggplot(data=plotData[[i]], aes(x=0,y=factor(GroupName))) +
    geom_dotplot(dotsize=0.9,binwidth=1,binaxis='y',method="histodot",stackgroups=T,
        aes(fill=factor(Color),color=Title )) +
    scale_color_manual(values=c('gray70','blue','gray70'),limits=c('man','boy','woman')) +
    scale_fill_manual(values=c('#BB0000','#FF0000','#009900','#00EE00','gray70','gray70','white'),
        limits=c('0','-1','1','2','3','4','5')) +
    scale_y_discrete(limits = rev(levels(factor(plotData[[i]]$GroupName)))) +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.position='none')
grid.arrange(g[[1]],g[[2]],g[[3]],nrow=1,top='All 80 woman-child-groups in the test and training datasets combined (228 passengers).
Red = deceased female or boy, Green = survived, White or Gray = unknown survival, 
White or LightGreen or LightRed = different surname same ticket, Blue outline = boy')

data$Survived <- factor(data$Survived)
data$CabinLetter <- substring(data$Cabin,0,1)
g1 = ggplot(data=data[data$GroupId!='noGroup' & !is.na(data$Survived),]) +
    geom_bar(stat='count',aes(x=Pclass,fill=Survived))
g2 = ggplot(data=data[data$GroupId!='noGroup' & !is.na(data$Survived) & !is.na(data$Age),]) +
    geom_histogram(bins=20,aes(x=Age,fill=Survived))
g3 = ggplot(data=data[data$GroupId!='noGroup' & !is.na(data$Survived),]) +
    geom_bar(stat='count',aes(x=Embarked,fill=Survived))
g4 = ggplot(data=data[data$GroupId!='noGroup' & !is.na(data$Survived),]) +
    geom_bar(stat='count',aes(x=CabinLetter,fill=Survived))
grid.arrange(g1,g2,g3,g4,nrow=2,top='Analysis of training set\'s 156 Woman-Child-Group passengers')

# engineer titles on training set
train$Title <- 'man'
train$Title[train$Sex=='female'] <- 'woman'
train$Title[grep('Master',train$Name)] <- 'boy'


# Perform 25 trials of 10-fold cross validation
trials = 25; sum = 0

for (j in 1:trials){
x = sample(1:890); s = 0
for (i in 0:9){
    # engineer "woman-child-groups"
    train$Surname <- substring(train$Name,0,regexpr(",",train$Name)-1)
    train$GroupId = paste( train$Surname, train$Pclass,sub('.$','X',train$Ticket),train$Fare,train$Embarked,sep='-')
    train$GroupId[train$Title=='man'] <- 'noGroup'
    train$GroupFreq <- ave(1:891,train$GroupId,FUN=length)
    train$GroupId[train$GroupFreq<=1] <- 'noGroup'
    # add nannies and relatives to groups.
    train$TicketId = paste( train$Pclass,sub('.$','X',train$Ticket),train$Fare,train$Embarked,sep='-')
    for (k in which(train$Title!='man' & train$GroupId=='noGroup'))
        train$GroupId[k] = train$GroupId[train$TicketId==train$TicketId[k] & train$PassengerId != train$PassengerId[k]][1]
    train$GroupId[is.na(train$GroupId)] <- 'noGroup'
    train$GroupFreq <- ave(1:891,train$GroupId,FUN=length)
    # calculate training subset's group survival rate
    train$GroupSurvival <- NA
    train$GroupSurvival[-x[1:89+i*89]] <- ave(train$Survived[-x[1:89+i*89]],train$GroupId[-x[1:89+i*89]])
    # calculate testing subset's group survival rate from training set's rate
    for (k in x[1:89+i*89]){ 
        train$GroupSurvival[k] <- train$GroupSurvival[which(!is.na(train$GroupSurvival) & train$GroupId==train$GroupId[k])[1]]
        if (is.na(train$GroupSurvival[k])) train$GroupSurvival[k] <- ifelse(train$Pclass[k]==3,0,1)
    }
    # apply gender model plus WCG
    train$predict <- 0
    train$predict[train$Title=='woman'] <- 1
    train$predict[train$Title=='boy' & train$GroupSurvival==1] <- 1
    train$predict[train$Title=='woman' & train$GroupSurvival==0] <- 0
    c = sum(abs(train$predict[x[1:89+i*89]] - train$Survived[x[1:89+i*89]]))
    s = s + c
}
#cat( sprintf("Trial %d has 10-fold CV accuracy = %f\n",j,1-s/890))
sum = sum + 1-s/890
}
cat(sprintf("Average 10-fold CV accuracy from %d trials = %f\n",trials,sum/trials))

data$GroupSurvival <- NA
data$Survived <- as.numeric(as.character(data$Survived))
data$GroupSurvival[1:891] <- ave(data$Survived[1:891],data$GroupId[1:891])
for (i in 892:1309) data$GroupSurvival[i] <- data$GroupSurvival[which(data$GroupId==data$GroupId[i])[1]]
data$GroupSurvival[is.na(data$GroupSurvival) & data$Pclass==3] <- 0
data$GroupSurvival[is.na(data$GroupSurvival) & data$Pclass!=3] <- 1
data$Predict <- 0
data$Predict[data$Sex=='female'] <- 1
data$Predict[data$Title=='woman' & data$GroupSurvival==0] <- 0
data$Predict[data$Title=='boy' & data$GroupSurvival==1] <- 1

cat('The following 8 males are predicted to live\n')
data[data$Sex=='male' & data$Predict==1 & data$PassengerId>891,c('Name','Title')]
cat('The following 14 females are predicted to die\n')
data[data$Sex=='female' & data$Predict==0 & data$PassengerId>891,c('Name','Title')]
cat('The remaining 258 males are predicted to die\n')
cat('and the remaining 138 females are predicted to live\n')

submit <- data.frame(PassengerId = 892:1309, Survived = data$Predict[892:1309])
write.csv(submit,'genderSurnameModel2.csv',row.names=F)

train <- read.csv('../input/titanic/train.csv',stringsAsFactors=F)
test <- read.csv('../input/titanic/test.csv',stringsAsFactors=F)
test$Survived <- NA; data <- rbind(train,test)
# engineer person type
data$Title <- 'man'
data$Title[grep('Master',data$Name)] <- 'boy'
data$Title[data$Sex=='female'] <- 'woman'

fit <- rpart(Age ~ Title + Pclass + SibSp + Parch,data=data)

fit

data$Age[is.na(data$Age)] <- predict(fit,newdata=data[is.na(data$Age),])

fit <- rpart(Fare ~ Title + Pclass + Embarked + Sex + Age,data=data)

fit

data$Fare[is.na(data$Fare)] <- predict(fit,newdata=data[is.na(data$Fare),])

data <- data %>% as.tibble() %>%
    mutate(TicketFreq  = ave(1:1309, Ticket,FUN=length),
           FareAdj = Fare / TicketFreq,
           FamilySize = SibSp + Parch + 1
          )           

data2 <- data[data$PassengerId<=891 & data$Title=='man',]

data3 <- data.frame(
    y = data2$Survived,
    x1 = data2$Fare / (data2$TicketFreq * 10),
    x2 = (data2$SibSp + data2$Parch + 1) + (data2$Age / 70),
    Pclass = data2$Pclass)

g1 = ggplot(data[data$FareAdj>0 & data$FareAdj<40,]) + 
    geom_density(aes(x=FareAdj,fill=factor(Pclass)),alpha=0.9,show.legend=F) +
    labs(fill='Pclass') + geom_vline(xintercept=c(10,20),linetype='dotted') +
    xlim(0,40) + labs(title='All Passengers',x='x1 = Fare / Ticket Frequency')

g2 = ggplot(data[data$Fare>0 & data$Fare<40 ,]) +
    geom_density(aes(x=Fare,fill=factor(Pclass)),alpha=0.9) +
    xlim(0,40) + labs(title='All Passengers',fill='Pclass',y='')

g3 = ggplot(data[!is.na(data$Survived) & data$Title=='man' & data$Pclass==1 & data$FamilySize<4,]) + 
    geom_density(aes(x=FamilySize+Age/70,fill=factor(Survived)),alpha=0.9,bw=0.04) +
    labs(fill='Survived') +
    annotate('text',x=1.4,y=2.4,label='Age 30\nFS=1') +
    annotate('text',x=2.4,y=2.4,label='Age 30\nFS=2') +
    geom_vline(xintercept=c(1.43,2.43),linetype='dotted') +
    xlim(1,4) + labs(title='Adult Males Pclass=1',x='x2 = FamilySize + Age/70')

grid.arrange(g3,g1,g2,layout_matrix = rbind(c(1, 1), c(2, 3)))

# classify males with xgboost
set.seed(2310)

x1s <- seq(0, 5, length.out = 100)
x2s <- seq(1, 3, length.out = 100)
g <- data.frame(x1 = rep(x1s, each=100), x2 = rep(x2s, time = 100))
param <- list(objective   = "binary:logistic",
            eval_metric = "error",
            max_depth   = 5,
            eta         = 0.1,
            gammma      = 0.1,
            colsample_bytree = 1,
            min_child_weight = 1)

param

dim(data.matrix(data3[,c('x1','x2')]))

length(data3$y)

cat('XGBoosting begun...\n')
xgb <- xgboost(params  = param,
            data    = data.matrix(data3[,c('x1','x2')]),
            label   = as.numeric(as.factor(data3$y)) - 1 , 
            nrounds = 500,
            print_every_n = 100,
            verbose = 1)

p <- predict(xgb,newdata=data.matrix(g[,c('x1','x2')]))
g$y <- ifelse(p>0.5,1,0)

head(g)

ggplot(data=data3[data3$x1<5 & data3$x2<3,]) +
    xlim(0,5) + ylim(1,3) +
    geom_tile(data=g,aes(x1,x2,fill=factor(y))) +
    geom_density_2d(aes(x1,x2,color=factor(y))) +
    geom_point(size=2,aes(x1,x2,color=factor(y),shape=factor(Pclass))) +
    scale_color_manual(values=c('#AA0000','#00AA00'),
        limits=c('0','1'),labels=c('0','1')) +
    scale_fill_manual(values=c('#FF9999','#99FF99'),
        limits=c('0','1'),labels=c('0','1')) +
    labs(x='Fare / (10 x TicketFrequency)',y='FamilySize + (Age / 70)',shape='Pclass',fill='Classify',
        title='XGBoost learns the training set\'s
        537 adult males. Green is P(live)>0.5',color='Survived') +
    geom_vline(xintercept=2.8, linetype='dotted') +
    geom_hline(yintercept=c(1.43,2.43), linetype='dotted') +
    annotate('text',x=2.95,y=2.9,label='Fare = $28') +
    annotate('text',x=4.7,y=2.35,label='Age = 30') +
    annotate('text',x=4.7,y=1.35,label='Age = 30')

set.seed(300)
s = sample(1:537,100)
s2 = (1:537)[-s]
xgb <- xgboost(params  = param,
            data    = data.matrix(data3[s2,c('x1','x2')]),
            label   = as.numeric(as.factor(data3$y[s2])) - 1 ,  
            nrounds = 500,
            print_every_n = 100,
            silent = 1,
            verbose = 0)

p <- predict(xgb,newdata=data.matrix(data3[s,c('x1','x2')]))
data3$y2 <- NA
roc = data.frame(TN=rep(0,102),FP=rep(0,102),FN=rep(0,102)
    ,TP=rep(0,102),FPR=rep(0,102),TPR=rep(0,102))
rownames(roc) <- seq(0,1.01,0.01)
for (i in 1:102){
    data3$y2[s] <- ifelse(p<(i-1)/100,0,1)
    roc$TP[i] <- length(which(data3$y==1 & data3$y2==1))
    roc$TN[i] <- length(which(data3$y==0 & data3$y2==0))
    roc$FP[i] <- length(which(data3$y==0 & data3$y2==1))
    roc$FN[i] <- length(which(data3$y==1 & data3$y2==0))
    roc$FPR[i] <- roc$FP[i] / (roc$FP[i] + roc$TN[i])
    roc$TPR[i] <- roc$TP[i] / (roc$TP[i] + roc$FN[i])
}
diag <- data.frame(x=c(0,1),y=c(0,1))
ggplot(roc,aes(x=FPR,y=TPR)) + 
    geom_line() + labs(title='ROC curve where "positive" = male survives',
        x='False positive rate',y='True positive rate') +
    geom_point(data=roc[91,],aes(x=FPR,y=TPR),size=3,color='red') +
    annotate('text',x=0.13,y=0.12,label='threshold p>=0.9',color='red') +
    geom_point(data=roc[76,],aes(x=FPR,y=TPR),size=3,color='darkgreen') +
    annotate('text',x=0.16,y=0.30,label='threshold p>=0.75',color='darkgreen') +
    geom_point(data=roc[51,],aes(x=FPR,y=TPR),size=3,color='blue') +
    annotate('text',x=0.20,y=0.5,label='threshold p>=0.5',color='blue') +
    geom_line(data=diag,aes(x=x,y=y),linetype='dotted')
area = 0
for(i in 1:101){
    area = area + roc$TPR[i] * (roc$FPR[i]-roc$FPR[i+1])
}
cat(sprintf('Area under ROC = %f\n',area))

#accuracy = matrix(nrow=4,ncol=4)
#rownames(accuracy) <- c('d=3','d=4','d=5','d=6')
#colnames(accuracy) <- c('0.50','0.75','0.90','0.92')
#for (j in 1:4)
#for (k in 1:4){
set.seed(2)
trials=100; sum=0
for (i in 1:trials){
    s = sample(1:891,800)
    s2 = (1:891)[-s]
    dataB <- data[data$PassengerId %in% s & data$Title=='man',]
    dataC <- data[data$PassengerId %in% s2 & data$Title=='man',]
    data$Predict <- 0
    data$Predict[data$Sex=='female'] <- 1
    dataTrain <- data.frame(y=as.numeric(as.factor(dataB$Survived)) - 1,x1=dataB$FareAdj/10,x2=dataB$FamilySize+dataB$Age/70)
    dataTest <- data.frame(y=as.numeric(as.factor(dataC$Survived)) - 1,x1=dataC$FareAdj/10
        ,x2=dataC$FamilySize+dataC$Age/70,PassengerId=dataC$PassengerId)
    param <- list(objective   = "binary:logistic",
              eval_metric = "error",
              max_depth   = 5,
              eta         = 0.1,
              gammma      = 0.1,
              colsample_bytree = 1,
              min_child_weight = 1)
    xgb <- xgboost(params  = param,
              data    = data.matrix(dataTrain[,c('x1','x2')]),
              label   = dataTrain$y, 
              nrounds = 500,
              silent = 1,
              print_every_n = 100,
              verbose = 0)
    p <- predict(xgb,newdata=data.matrix(dataTest[,c('x1','x2')]))
    dataTest$p <- ifelse(p>=0.90,1,0)
    data$Predict[dataTest$PassengerId] <- dataTest$p 
    sm = sum(abs(as.integer(data$Predict[s2]) - as.integer(data$Survived[s2])))
    cv = 1 - sm/length(s2)
    #if (i %% 25==0) 
    #cat(sprintf('Trial %d has CV = %f\n',i,cv))
    sum = sum + cv
}
cat(sprintf('Average CV of %d trials = %f\n',trials,sum/trials))
#accuracy[j,k] <- sum/trials
#}
#accuracy

dataB <- data[data$PassengerId %in% 1:891 & data$Title=='man',]
dataC <- data[data$PassengerId %in% 892:1309 & data$Title=='man',]
data$Predict <- 0
data$Predict[data$Sex=='female'] <- 1
dataTrain <- data.frame(y=as.numeric(as.factor(dataB$Survived)) - 1,x1=dataB$FareAdj/10,x2=dataB$FamilySize+dataB$Age/70)
dataTest <- data.frame(y=as.numeric(as.factor(dataC$Survived)) - 1,x1=dataC$FareAdj/10,Pclass=dataC$Pclass
    ,x2=dataC$FamilySize+dataC$Age/70,PassengerId=dataC$PassengerId)

param <- list(objective   = "binary:logistic",
              eval_metric = "error",
              max_depth   = 5,
              eta         = 0.1,
              gammma      = 0.1,
              colsample_bytree = 1,
              min_child_weight = 1)

xgb <- xgboost(params  = param,
              data    = data.matrix(dataTrain[,c('x1','x2')]),
              label   = dataTrain$y, 
              nrounds = 500,
              silent = 1,
              print_every_n = 100,
              verbose = 0)

p <- predict(xgb,newdata=data.matrix(dataTest[,c('x1','x2')]))
dataTest$p <- ifelse(p>=0.90,1,0)
data$Predict[dataTest$PassengerId] <- dataTest$p 
maleLive = which(data$Title=='man' & data$Predict==1)
cat(sprintf('The following %d adult males are predicted to live\n',length(maleLive)))
data[maleLive,c('PassengerId','Pclass','Name','Age','SibSp','Parch','FareAdj')]

x1s <- seq(2, 5, length.out = 100)
x2s <- seq(1, 3, length.out = 100)
g <- data.frame(x1 = rep(x1s, each=100), x2 = rep(x2s, time = 100))
p <- predict(xgb,newdata=data.matrix(g[,c('x1','x2')]))
g$y <- ifelse(p>=0.90,1,0)
g1 <- ggplot(data=dataTest[dataTest$x1>2 & dataTest$x1<5 & dataTest$x2>1 & dataTest$x2<3,]) +
    xlim(2,5) + ylim(1,3) +
    geom_tile(data=g,aes(x1,x2,fill=factor(y))) +
    geom_point(size=2,aes(x1,x2,color=factor(p),shape=factor(Pclass))) +
    scale_color_manual(values=c('#666666','#0000FF'),
        limits=c('0','1'),labels=c('0','1')) +
    scale_fill_manual(values=c('#FF9999','#99FF99'),
        limits=c('0','1'),labels=c('0','1')) +
    labs(x='Fare / (10 x TicketFrequency)',y='FamilySize + (Age / 70)',shape='Pclass',fill='Classifier',
        title='XGBoost classifies the test set.
        It predicts 4 adult males have P(live)>=0.9',color='Predict') +
    geom_vline(xintercept=2.8, linetype='dotted') +
    geom_hline(yintercept=c(1.43,2.43), linetype='dotted') +
    annotate('text',x=2.95,y=2.9,label='Fare = $28') +
    annotate('text',x=4.7,y=2.35,label='Age = 30') +
    annotate('text',x=4.7,y=1.35,label='Age = 30')

for (i in which(dataTest$p==1)){
    g1 <- g1 + annotate('text',x=dataTest$x1[i]-0.15,y=dataTest$x2[i],label=dataTest$PassengerId[i]
        ,color='darkblue',size=4)
}
g1

# engineer "woman-child-groups"
data$Surname = substring( data$Name,0,regexpr(',',data$Name)-1)
data$GroupId = paste( data$Surname, data$Pclass, sub('.$','X',data$Ticket), data$Fare, data$Embarked, sep='-')
data$GroupId[data$Title=='man'] <- 'noGroup'
# Mrs Wilkes (Needs) is Mrs Hocking (Needs) sister
data$GroupId[893] <- data$GroupId[775]
data$GroupFreq <- ave(1:1309,data$GroupId,FUN=length)
data$GroupId[data$GroupFreq<=1] <- 'noGroup'
cat(sprintf('We found %d woman-child-groups\n',length(unique(data$GroupId))-1))
data$TicketId = paste( data$Pclass,sub('.$','X',data$Ticket),data$Fare,data$Embarked,sep='-')
# add nannies and relatives to groups
c = 0
for (i in which(data$Title!='man' & data$GroupId=='noGroup')){
    data$GroupId[i] = data$GroupId[data$TicketId==data$TicketId[i]][1]
    if (data$GroupId[i]!='noGroup') c = c + 1
}
cat(sprintf('We found %d nannies/relatives and added them to groups.\n',c))
# calculate group survival rates
data$GroupSurvival <- NA
data$Survived <- as.numeric(as.character(data$Survived))
data$GroupSurvival[1:891] <- ave(data$Survived[1:891],data$GroupId[1:891])
for (i in 892:1309) data$GroupSurvival[i] <- data$GroupSurvival[which(data$GroupId==data$GroupId[i])[1]]
# classify unknown groups
data$GroupSurvival[is.na(data$GroupSurvival) & data$Pclass==3] <- 0
data$GroupSurvival[is.na(data$GroupSurvival) & data$Pclass!=3] <- 1
# make predictions
data$Predict <- 0
data$Predict[data$Sex=='female'] <- 1
data$Predict[data$Title=='woman' & data$GroupSurvival==0] <- 0
data$Predict[data$Title=='boy' & data$GroupSurvival==1] <- 1
x = which(data$Sex=='male' & data$Predict==1 & data$PassengerId>891)
cat(sprintf('We found %d boys predicted to live\n',length(x)))
x = which(data$Sex=='female' & data$Predict==0 & data$PassengerId>891)
cat(sprintf('We found %d females predicted to die\n',length(x)))

submit <- data.frame(PassengerId=892:1309,Survived=data$Predict[892:1309])
submit$Survived[maleLive-891] <- 1
write.csv(submit,'WCG_XGBoost1.csv',row.names=F)

# identify WCG females
WCGtrain = which(data$Passenger<=891 & (data$GroupSurvival==0 | data$GroupSurvival==1))
WCGtest = which(data$Passenger>891 & (data$GroupSurvival==0 | data$GroupSurvival==1))
# identify solo females
data2 <- data[data$PassengerId<=891 & data$Title=='woman' & data$FamilySize==1,]
data3 <- data.frame(y=data2$Survived,x1=data2$FareAdj/10,x2=data2$Age/15,Pclass=data2$Pclass)
# set zoom
x1s <- seq(0.5, 1.5, length.out = 100)
x2s <- seq(1, 3, length.out = 100)
g <- data.frame(x1 = rep(x1s, each=100), x2 = rep(x2s, time = 100))

dim(data3)
str(data3)

# classify females with XGBoost
param <- list(objective   = "binary:logistic",
              eval_metric = "error",
              max_depth   = 5,
              eta         = 0.1,
              gammma      = 0.1,
              colsample_bytree = 1,
              min_child_weight = 1)
cat('XGBoosting begun...\n')
xgb <- xgboost(params  = param,
               data    = data.matrix(data3[,c('x1','x2')]),
              label   = data3$y, 
              nrounds = 500,
              print_every_n = 100,
              verbose = 1)
p <- predict(xgb,newdata=data.matrix(g[,c('x1','x2')]))
g$y <- ifelse(p<=0.25,0,1)
# plot results
ggplot(data=data3[data3$x1>0.5 & data3$x1<1.5 & data3$x2>1 & data3$x2<3,]) +
    xlim(0.5,1.5) + ylim(1,3) +
    geom_tile(data=g,aes(x1,x2,fill=factor(y))) +
    geom_density_2d(aes(x1,x2,color=factor(y))) +
    geom_point(size=2,aes(x1,x2,color=factor(y),shape=factor(Pclass))) +
    scale_color_manual(values=c('#AA0000','#00AA00'),
        limits=c('0','1'),labels=c('0','1')) +
    scale_fill_manual(values=c('#FF9999','#99FF99'),
        limits=c('0','1'),labels=c('0','1')) +
    labs(x='Fare / (10 x TicketFrequency)',y='Age / 15',shape='Pclass',fill='Classify',
        title='XGBoost learns the training set\'s
        126 solo females. Red is P(die)>=0.75',color='Survived') +
    geom_vline(xintercept=c(0.8,0.9), linetype='dotted') +
    annotate('text',x=0.77,y=2.95,label='Fare = $8') +
    annotate('text',x=0.93,y=2.95,label='Fare = $9') +
    geom_hline(yintercept=c(1.33,1.67), linetype='dotted') +
    annotate('text',x=1.35,y=1.61,label='Age = 25') +
    annotate('text',x=1.35,y=1.28,label='Age = 20')

#accuracy = matrix(nrow=4,ncol=4)
#rownames(accuracy) <- c('d=3','d=4','d=5','d=6')
#colnames(accuracy) <- c('0.08','0.10','0.25','0.50')
#for (j in 1:4)
#for (k in 1:4){
trials=100; sum=0
for (i in 1:trials){
    s = sample(1:891,800)
    s2 = (1:891)[-s]
    dataB <- data[data$PassengerId %in% s & data$Title=='woman' & data$FamilySize==1,]
    dataC <- data[data$PassengerId %in% s2 & data$Title=='woman'& data$FamilySize==1,]
    data$Predict <- 0
    data$Predict[data$Sex=='female'] <- 1
    dataTrain <- data.frame(y=dataB$Survived,x1=dataB$FareAdj/10,x2=dataB$Age/15)
    dataTest <- data.frame(y=dataC$Survived,x1=dataC$FareAdj/10
        ,x2=dataC$Age/15,PassengerId=dataC$PassengerId)
    param <- list(objective   = "binary:logistic",
              eval_metric = "error",
              max_depth   = 5,
              eta         = 0.1,
              gammma      = 0.1,
              colsample_bytree = 1,
              min_child_weight = 1)
    xgb <- xgboost(params  = param,
              data    = data.matrix(dataTrain[,c('x1','x2')]),
              label   = dataTrain$y, 
              nrounds = 500,
              silent = 1,
              print_every_n = 100,
              verbose = 0)
    p <- predict(xgb,newdata=data.matrix(dataTest[,c('x1','x2')]))
    dataTest$p <- ifelse(p<=0.08,0,1)
    data$Predict[dataTest$PassengerId] <- dataTest$p 
    sm = sum(abs(data$Predict[s2] - data$Survived[s2]))
    cv = 1 - sm/length(s2)
    #if (i %% 25==0) 
    #cat(sprintf('Trial %d has CV = %f\n',i,cv))
    sum = sum + cv
}
cat(sprintf('Average CV of %d trials = %f\n',trials,sum/trials))
#accuracy[j,k] <- sum/trials
#}
#accuracy

dataB <- data[data$PassengerId %in% 1:891 & data$Title=='woman' & data$FamilySize==1,]
dataC <- data[data$PassengerId %in% 892:1309 & data$Title=='woman' & data$FamilySize==1 
    & !data$PassengerId %in% WCGtest,]
data$Predict <- 0
data$Predict[data$Sex=='female'] <- 1
dataTrain <- data.frame(y=dataB$Survived,x1=dataB$FareAdj/10,x2=dataB$Age/15)
dataTest <- data.frame(y=dataC$Survived,x1=dataC$FareAdj/10
    ,x2=dataC$Age/15,PassengerId=dataC$PassengerId,Pclass=dataC$Pclass)
param <- list(objective   = "binary:logistic",
              eval_metric = "error",
              max_depth   = 5,
              eta         = 0.1,
              gammma      = 0.1,
              colsample_bytree = 1,
              min_child_weight = 1)
xgb <- xgboost(params  = param,
              data    = data.matrix(dataTrain[,c('x1','x2')]),
              label   = dataTrain$y, 
              nrounds = 500,
              silent = 1,
              print_every_n = 100,
              verbose = 0)
p <- predict(xgb,newdata=data.matrix(dataTest[,c('x1','x2')]))
dataTest$p <- ifelse(p<=0.08,0,1)
data$Predict[dataTest$PassengerId] <- dataTest$p 
femalePerish = which(data$Title=='woman' & data$Predict==0)
cat(sprintf('The following %d females are predicted to die\n',length(femalePerish)))
data[femalePerish,c('PassengerId','Pclass','Name','Age','SibSp','Parch','FareAdj')]

x1s <- seq(0.6, 1, length.out = 100)
x2s <- seq(1, 3, length.out = 100)
g <- data.frame(x1 = rep(x1s, each=100), x2 = rep(x2s, time = 100))
p <- predict(xgb,newdata=data.matrix(g[,c('x1','x2')]))
g$y <- ifelse(p<=0.08,0,1)
g1 <- ggplot(data=dataTest[dataTest$x1<1 & dataTest$x2<3,]) +
    xlim(0.6,1) + ylim(1,3) +
    geom_tile(data=g,aes(x1,x2,fill=factor(y))) +
    geom_point(size=2,aes(x1,x2,color=factor(p),shape=factor(Pclass))) +
    scale_color_manual(values=c('#0000FF','#666666'),
        limits=c('0','1'),labels=c('0','1')) +
    scale_fill_manual(values=c('#FF9999','#99FF99'),
        limits=c('0','1'),labels=c('0','1')) +
    labs(x='Fare / (10 x TicketFrequency)',y='Age / 15',shape='Pclass',fill='Classifier',
        title='XGBoost classifies the test set.
        It predicts 10 non-WCG females have P(die)>=0.92',color='Predict') +
    geom_vline(xintercept=c(0.8,0.9), linetype='dotted') +
    geom_hline(yintercept=c(1.33,1.67), linetype='dotted') +
    annotate('text',x=0.8,y=2.95,label='Fare = $8') +
    annotate('text',x=0.9,y=2.95,label='Fare = $9') +
    annotate('text',x=0.65,y=1.61,label='Age = 25') +
    annotate('text',x=0.65,y=1.28,label='Age = 20')
# plot passenger numbers
v = c(0.07,0.02,0.02,0.02,-0.04,0.02,0.02,0.02,0.02,0.02); c = 0
for (i in which(dataTest$p==0)){
    c = c + 1;
    g1 <- g1 + annotate('text',x=dataTest$x1[i]+0.015,y=dataTest$x2[i]+v[c],label=dataTest$PassengerId[i]
        ,color='darkblue',size=3)
}
g1

submit$Survived[femalePerish-891] <- 0
write.csv(submit,'WCG_XGBoost2.csv',row.names=F)

g = list(); gp = list()
gt = c('CART learns the training set','kNN k=5 learns the training set'
       ,'SVM with radial kernel','Random Forest with P(die)>0.75')
data2 <- data[data$PassengerId<=891 & data$Title=='woman' & data$FamilySize==1 & data$Pclass==3,]
data3 <- data.frame(y=data2$Survived,x1=data2$FareAdj/10,x2=data2$Age/15)
# set zoom
x1s <- seq(0.6, 1, length.out = 50)
x2s <- seq(1, 3, length.out = 50)
for (i in 1:4){
    g[[i]] <- data.frame(x1 = rep(x1s, each=100), x2 = rep(x2s, time = 100))
    if (i==1){
        fit <- rpart(factor(y) ~ x1 + x2,data3)
        p <- predict(fit,newdata=g[[i]])[,2]
        g[[i]]$y <- ifelse(p<=0.5,0,1)
    } else if (i==2){
        fit <- knn3(factor(y) ~ x1 + x2,data3,k=3)
        p <- predict(fit,newdata=g[[i]])[,2]
        g[[i]]$y <- ifelse(p<=0.5,0,1)
    } else if (i==3){
        fit <- svm(factor(y)~ x1 + x2,data3,kernel='radial')
        p <- predict(fit,newdata=g[[i]])
        g[[i]]$y <- as.numeric(as.character(p))
    } else if (i==4){
        fit <- randomForest(factor(y) ~ x1 + x2,data3)
        p <- predict(fit,newdata=g[[i]],type='prob')[,2]
        g[[i]]$y <- ifelse(p<=0.25,0,1)
    }
    # plot results
    gp[[i]] <- ggplot(data=data3[data3$x1>0.6 & data3$x1<1 & data3$x2>1 & data3$x2<3,]) +
        xlim(0.6,1) + ylim(1,3) +
        geom_tile(data=g[[i]],aes(x1,x2,fill=factor(y)),show.legend=F) +
        geom_point(size=1.5,aes(x1,x2,color=factor(y)),show.legend=F) +
        scale_color_manual(values=c('#AA0000','#00AA00'),
            limits=c('0','1'),labels=c('0','1')) +
        scale_fill_manual(values=c('#FF9999','#99FF99'),
            limits=c('0','1'),labels=c('0','1')) +
        #labs(x='Fare / (10 x TicketFrequency)',y='Age / 15',fill='Classify',
        labs(x='',y='',title=gt[i],color='Survived') +
        geom_vline(xintercept=c(0.8,0.9), linetype='dotted') +
        geom_hline(yintercept=c(1.33,1.67), linetype='dotted')
}
grid.arrange(gp[[1]],gp[[2]],gp[[3]],gp[[4]],nrow=2)
