
install.packages(c('ggplot2','ISLR','MASS','glmnet','randomForest','gbm','rpart','boot'))
install.packages('dplyr')
library(ggplot2)
library(ISLR)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(rpart)
library(boot)
library(dplyr)
select <- dplyr::select 

#데이터 읽어오기. 문자는 factor로 변환 안시킴. 결측값을 NA로 변경. 
titanic_train <- read.csv("../input/train.csv", stringsAsFactors = F, na.strings = c("NA", ""))
titanic_test <- read.csv("../input/test.csv", stringsAsFactors = F, na.strings = c("NA", ""))
?read.csv
#데이터 구조파악 
glimpse(titanic_train)
str(titanic_train)
glimpse(titanic_test) #Survived 변수가 test 데이터에는 없다. 
titanic_test$Survived <- NA #결측으로 survived 값을 만들어 준다. 

#survival : 0 = no, 1 = yes 
#pclass : 1 = 1st, 2 = 2nd, 3 = 3rd
#sibsp : 형제, 배우자의 수 
#Parch : 부모/아이의 수
#범주형자료: Name, Ticket, Sex, Cabin(148레벨), Embarked(4레벨) 
#C = Cherbourg, Q = Queenstown, S = Southampton

titanic_all <- rbind(titanic_train, titanic_test)
titanic_all$Sex <- as.factor(titanic_all$Sex)
titanic_all$Survived <- as.factor(titanic_all$Survived)
titanic_all$Pclass <- as.ordered(titanic_all$Pclass) #Pclass는 순서척도


#결측 데이터 확인 
sapply(titanic_all, function(x) {
  sum(is.na(x))}) #age랑 cabin이 결측데이터가 많다. 
  
sapply(titanic_train, function(x) {
  sum(is.na(x))}) #test 데이터의 survived는 결측이다 



#시각화
#생존자 수 
ggplot(titanic_train[!is.na(titanic_train$Survived),], aes(x = Survived, fill = Survived)) +
  geom_bar(stat='count') +
  labs(x = '타이타닉 생존') +
  geom_label(stat='count',aes(label=..count..), size=7) 



#성별에 따른 탑승인원,,   
ggplot(titanic_train, aes(x = Sex, fill = Sex)) +
  geom_bar(stat='count', position='dodge')  
  labs(x = 'train_성별') +
  geom_label(stat='count', aes(label=..count..)) 

#좌석등급별 탑승인원  
ggplot(titanic_train, aes(x = Pclass, fill = Pclass)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Pclass, train_좌석등급') + geom_label(stat='count', aes(label=..count..)) +
  theme(legend.position="none")      

install.packages("sqldf")
library(sqldf)

#성별별 생존자 수 
sqldf('select *, (Survived/Total)
       from( select A.Survived, B.Total, A.sex 
             from(select count(*) Survived, Sex
                  from titanic_train
                  where Survived = 1
                  group by Sex ) A
             inner join (select count(*) Total, Sex
                    from titanic_train
                    group by Sex) B
        on A.Sex = B.Sex 
       ) T
      ')
#여자 생존자: 233/314 
#남자 생존자: 109/577    //남자가 생존율이 더 낮다. 



sqldf('select *, (Survived/Total)
       from(select A.Survived, B.Total, A.Pclass 
            from(select count(*) Survived, Pclass
                 from titanic_train
                 where Survived = 1 
                 group by Pclass) A
                  inner join (select count(*) Total, Pclass
                              from titanic_train
                              group by Pclass) B
        on A.Pclass = B.Pclass 
      ) T
      ')
#1등석 생존자 : 136/216 
#2등석 생존자 : 87/184 
#3등석 생존자 : 119/491     //3등석일수록 생존율 낮다.

sqldf('select *
       from (select A.Survived, B.Total, A.Pclass, A.Sex 
            from (select count(*) Survived, Pclass, Sex
                  from titanic_train
                  where Survived = 1 
                  group by Sex, Pclass) A
                    inner join (select count(*) Total, Pclass, Sex
                                from titanic_train
                                group by Sex, Pclass) B
                  on A.Pclass = B.Pclass 
                  and A.Sex = B.Sex
            ) T
     ')
#여자면서 1,2 등석은 거의 다 생존
#남자면서 2,3 등석은 거의 다 죽음 



#sibsp : 형제, 배우자의 수 
#Parch : 부모/아이의 수 
#에 따른 생존율을 비교해본다 
sqldf('select A.sibsp, Survived, Total
       from (select sibsp, count(*) Survived 
             from titanic_train 
             where Survived = 1
             group by sibsp) A 
       inner join ( select sibsp, count(*) Total 
                     from titanic_train 
                     group by sibsp
                    ) B
        on A.sibsp = B.sibsp 
      ')

sqldf('select A.Parch, Survived, Total
       from (select Parch, count(*) Survived 
             from titanic_train 
             where Survived = 1
             group by Parch) A 
       inner join ( select Parch, count(*) Total 
                    from titanic_train 
                    group by Parch
                  ) B
       on A.Parch = B.Parch 
      ')
#둘다 직접적인 지표는 아닌듯..? age랑 결합하면 좀더 좋은 자료가 될 것 같다.


#Embarked 와 생존율을 고려해보자.
sqldf('select A.Embarked, Survived, Total
       from (select Embarked, count(*) Survived 
            from titanic_train 
            where Survived = 1
            group by Embarked) A 
            inner join ( select Embarked, count(*) Total 
                          from titanic_train 
                          group by Embarked
                          ) B
      on A.Embarked = B.Embarked 
      ') 
#직접적인 지표는 아닌 것 같다. 


# #범주형 자료 모델화 
# x_mod_first <- model.matrix(~ Sex + Embarked + Pclass + SibSp + Parch,titanic_train)
# View(x_mod_first)
# dim(x_mod_first)



#훈련, 검증세트를 구분하자. 
#set.seed 안의 숫자는 의미없음. 계속 같은 난수(재현가능)로 테스트하기 위함
set.seed(1777) 
n <- nrow(titanic_train)
idx <- 1:n #adult의 row 수 만큼 idx 를 할당 
?sample
#sample()
training_idx <- sample(idx, n * .80) #idx 를 비복원 추출. n*0.8만큼. 
validate_idx <- setdiff(idx, training_idx) #training_idx에서 뽑힌것을 제외함.
length(training_idx)
length(validate_idx)
training <- titanic_train[training_idx, ]
validation <- titanic_train[validate_idx, ]



glm_first <- glm(Survived ~ Age + Pclass + Sex + SibSp + Parch + Fare, data = training, family = binomial) 
summary(glm_first)
glm_predictions <- predict(glm_first, newdata = validation, type = "response")

validation$pred_survived <- as.numeric(glm_predictions > .5)
validation$pred_survived <- ifelse(is.na(validation$pred_survived), 0, validation$pred_survived)
print(validation$pred_survived)

#with는 칼럼명에 바로 접근하는 함수. 
got_right <- with(validation, pred_survived == Survived)
got_wrong <- with(validation, pred_survived != Survived)
n_right <- sum(got_right, na.rm = TRUE)
n_wrong <- sum(got_wrong, na.rm = TRUE)
accuracy <- n_right / (n_right + n_wrong)
cat("accuracy: ", accuracy, "\n") #검증셋으로 정확도가 높게나옴. 



titanic_test$Survived <- ifelse(predict(glm_first, titanic_test, type="response")>0.5,1,0)
titanic_test$Survived <- ifelse(is.na(titanic_test$Survived),0,titanic_test$Survived)

submission <- titanic_test[, c("PassengerId", "Survived")]
glimpse(submission)
write.csv(submission, "submission.csv", row.names = FALSE)
print(list.files())


