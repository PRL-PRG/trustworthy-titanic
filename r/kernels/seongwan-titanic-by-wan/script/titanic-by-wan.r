
# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

import os
print(os.listdir("../input"))

# Any results you write to the current directory are saved as output.

## 초기화
rm(list=ls())
## 패키지 불러오기
library('dplyr')
library('tree')
library('ggplot2')
## 데이터 불러오기
getwd()
setwd('C:\\Users\\bangseongwan\\Desktop\\캐글 스터디\\1회차')
train <- read.csv('train.csv')
test <- read.csv('test.csv')
## 전체 데이터셋 만들기
full <- bind_rows(train,test)
tail(full)
## 변수 factor로 만들기
full$Survived <- as.factor(full$Survived)
full$Pclass <- as.factor(full$Pclass)
full$Ticket <- as.factor(full$Ticket)
full$Cabin <- as.factor(full$Cabin)
full$Embarked <- as.factor(full$Embarked)
## 다시 train test 데이터 생성
index.train <- 1:600
index.validation <- 601:nrow(train)
index.test <- (nrow(train)+1):nrow(full)
train <- full[index.train,]
validation <- full[index.validation,]
test <- full[index.test,]
## 데이터 탐색을 위해 단순 트리 모형 적합
## 변수는 종속 변수, index 변수, factor 변수면서 factor가 32개 넘는 변수를 제외한다.
model.tree <- tree(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train)
plot(model.tree)
text(model.tree,pretty=0)
post.tree <- predict(model.tree,select(validation,Pclass,Sex,Age,SibSp,Parch,Fare,Embarked))
pred.tree <- ifelse(post.tree[,1]>0.5,0,1)
## 예측율 계산
sum(validation$Survived == pred.tree)/length(pred.tree)
table(validation$Survived,pred.tree)
## 결론 : Sex, Pclass, Age, SibSp 이 영향력 있다고 판단
## 결측치 확인
sum(is.na(full))
sum(is.na(select(full,Sex)))
sum(is.na(select(full,Pclass)))
sum(is.na(select(full,Age)))
sum(is.na(select(full,SibSp)))
## 딥러닝을 위한 table 만들기
full.deep <- select(full,c(Survived,Sex,Pclass,SibSp))
train.deep <- full.deep[index.train,]
validation.deep <- full.deep[index.validation,]
test.deep <- full.deep[index.test,]
## h2o패키지
# 환경변수 설정
Sys.setenv("JAVA_HOME"='C:\\Program Files\\Java\\jdk1.8.0_161')
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "C:\\Program Files\\Java\\jdk1.8.0_161", sep = ":"))
# 이전 버전의 h2o    패키지가 있다면, 이전 h2o는 삭제
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# h2o 설치를 위한    의존성 패키지들    설치
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}
# 가장 최신버전의  h2o 설치하기 (source 타입)
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-wheeler/4/R")
require(h2o)
# h2o 시작
h2o.init()
# h2o 파일 올리기
full.h2o <- as.h2o(full.deep)
train.h2o <- as.h2o(train.deep)
validation.h2o <- as.h2o(validation.deep)
test.h2o <- as.h2o(select(test.deep,c(Sex,Pclass,SibSp)))
h2o.ls()
# target/feature 설정
target <- "Survived"
features <- names(full.deep)[!names(full.deep) %in% target]
# h2o glm model
model.glm <- h2o.glm(x=features,y=target,training_frame = train.h2o,model_id='glm_model',family='binomial')
pred.glm <- h2o.predict(model.glm,newdata=validation.h2o)
pred.glm$predict
table(validation.deep$Survived,as.vector(pred.glm$predict))
sum(validation.deep$Survived==as.vector(pred.glm$predict))/nrow(validation.deep)
# h2o randomforest model
model.rf <- h2o.randomForest(x=features,y=target,training_frame = train.h2o,model_id = 'rf_model',ntrees = 100)
pred.rf <- h2o.predict(model.rf,newdata = validation.h2o)
table(validation.deep$Survived,as.vector(pred.rf$predict))
sum(validation.deep$Survived==as.vector(pred.rf$predict))/nrow(validation.deep)

## 3가지 방법의 테스트 셋 예측 비교
pred.glm2 <- h2o.predict(model.glm,newdata=test.h2o)
pred.glm2$predict
pred.rf2 <- h2o.predict(model.rf,newdata=test.h2o)
pred.rf2$predict
cbind(as.vector(pred.glm2$predict),as.vector(pred.rf2$predict))
sum(as.vector(pred.glm2$predict)==as.vector(pred.rf2$predict))

## csv파일로 내보내기
write.csv(as.vector(pred.glm2$predict),file='titanic.csv',row.names=F)



