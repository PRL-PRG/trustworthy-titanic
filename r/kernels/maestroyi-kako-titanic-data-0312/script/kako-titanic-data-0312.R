## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data input, assement: 데이터 로드, 확인
library(tidyverse)     # R의 통합 패키지로 여기서는 'readr'패키지를 쓰기 위함이다.
library(descr)         # descr::CrossTable() - 범주별 빈도수, 비율 수치로 확인

# Visualization
library(VIM)           #Missing values assement use by VIM::aggr()
#library(ggplot2)      #Used in almost visualzation 원문에선 따로 패키지를 부르지만, 
                       #tidyverse내에 포함된 패키지로 여기선 주석으로 처리한다.
library(RColorBrewer)  # plot의 color 설정
library(scales)        # plot setting - x, y 축 설정

# Feature Engineering, Data Pre-processing
#library(tidyverse)    # 아래 패키지는 모두 포함하기에 주석 처리한다.
#library(dplyr)        # Feature Engineering & Data Pre-processing
#library(purrr)        # Check missing values
#library(tidyr)        # tidyr::gather()

# Model generation
library(randomForest)  # For Random Forest Modeling

# Model validation :   원문 커널에선 생략된 내용이다.
#library(caret)        # caret::confusionMatrix()
#library(ROCR)         # Plotting ROC Curve


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  library(grid)
  
  # Make a list from the... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL< then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i, j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
 }


## ----warning=FALSE, message=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- readr::read_csv("../input/train.csv")

test  <- readr::read_csv("../input/test.csv")

full  <- dplyr::bind_rows(train, test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full <- full %>% 
  dplyr::mutate(Survived = factor(Survived),
                Pclass = factor(Pclass, ordered = T),
                Name = factor(Name),
                Sex = factor(Sex),
                Ticket = factor(Ticket),
                Cabin = factor(Cabin),
                Embarked = factor(Embarked))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(full, 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
VIM::aggr(full, prop = FALSE, combined = TRUE, numbers = TRUE, 
          sortVars = TRUE, sortCombs = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full %>% 
  dplyr::summarise_all(funs(sum(is.na(.))/n()))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 각 feature의 결측치 비율 계산 -> Data Frame 속성 but 1행 12열 구조로 되어있다.
missing_values <- full %>% 
  dplyr::summarise_all(funs(sum(is.na(.))/n()))

# 위에서 구한 missing_values를 12x2 data frame으로 생성
missing_values <- tidyr::gather(missing_values,
                                key = 'feature', value = 'missing_pct')

# missing_values를 활용한 시각화
missing_values %>% 
  # Aesthetic setting: missing_pct 내림차순으로 정렬
  ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) +
  # Bar plot
  geom_bar(stat = 'identity', fill = 'red') +
  # Title generation
  ggtitle('Rate of missing values in each features') +
  # Title detail setting
  theme(plot.title = element_text(face = 'bold',    #글씨체
                                  hjust = 0.5,       # Horizon(가로비율) 
                                  size = 15, 
                                  color = 'darkblue')) +
  # x, y axis label setting
  labs(x = 'Feature names', y = 'Rate') + 
  # Plot의 x, y 축 변환
  coord_flip()


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
age.p1 <- full %>% 
  ggplot(aes(Age)) +
  # 히스토그램 작성, 설정
  geom_histogram(breaks = seq(0, 80, by = 1), #간격 설정
                 col = 'red',     # 막대 경계선 색깔
                 fill = 'green',  # 막대 채우기 색깔
                 alpha = .5) +     # 막대 투명도 50%
  # Plot title
  ggtitle('All Titanic passengers age histogram') +
  theme(plot.title = element_text(face = 'bold',   #글씨체
                                    hjust = 0.5,  #Horizon(가로비율)
                                    size = 15,
                                    color = 'darkblue'))
  
age.p2 <- full %>% 
  # test data set의 Survived == NA 인 값들 제외
  filter(!is.na(Survived)) %>% 
  ggplot(aes(Age, fill = Survived)) +
  geom_density(alpha = .5) +
  ggtitle('Titanic passengers age density plot') +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5,
                                  size = 15, color = 'darkblue'))
  
  # multiplot layout 형식 지정
  multi.layout = matrix(c(1, 1, 2, 2), 2, 2, byrow = T)
  
  # 위에서 생성한 2개의 그래프를 한 화면에 축력
  multiplot(age.p1, age.p2, layout = multi.layout)


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full %>% 
  # dplyr::group_by(), summarize() 를 이용해서 Pclass 빈도수 구하기
  group_by(Pclass) %>% 
  summarize(N = n()) %>% 
  # Aesthetic setting
  ggplot(aes(Pclass, N)) +
  geom_col() +
  # Pclass 빈도수 plot에 출력
  geom_text(aes(label = N),  #Plot의 y에 해당하는 N(빈도수) 매핑
            size = 5,   #글씨 크기
            vjust = 1.2,  #Vertical(가로) 위치 설정
            color = '#FFFFFF') +   #글씨 색깔 : 흰색
  # Plot title
  ggtitle("Number of each Pclass's passenger") +
  # Title setting
  theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 15)) +
  # x, y axis name change
  labs(x = 'Pclass', y = 'Count')


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Histogram
  Fare.p1 <- full %>% 
    ggplot(aes(Fare)) +
    geom_histogram(col = 'yellow',
                   fill = 'blue',
                   alpha = .5) +
  ggtitle('Histogram of passengers Fare') +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 15))

  # Boxplot
  Fare.p2 <- full %>% 
    filter(!is.na(Survived)) %>% 
    ggplot(aes(Survived, Fare)) +
    # 관측치는 회색점, 중복되는 부분은 흩뿌려서 나타냄
    geom_jitter(col='gray') +
    # boxplot : xnaudeh 50%
    geom_boxplot(alpha = .5) +
    ggtitle('Boxplot of passengers Fare') +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5, size= 15))
  
  # multiplot layout 형식 지정
  mulit.layout = matrix(c(1, 1, 2, 2), 2, 2)
  
  # 위에서 생성한 2개 그래프를 한 화면에 출력
  multiplot(Fare.p1, Fare.p2, layout = multi.layout)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sex.p1 <- full %>% 
  dplyr::group_by(Sex) %>% 
  summarize(N = n()) %>% 
  ggplot(aes(Sex, N)) +
  geom_col() +
  geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") + 
  ggtitle("Bar plot of Sex") +
  labs(x = "Sex", y = "Count")

sex.p2 <- full[1:891, ] %>% # 892행 이후부터는 'test' data로 생존율이 NA값임.
  ggplot(aes(Sex, fill = Survived)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = percent) +
  ggtitle('Survival Rate by Sex') +
  labs(x = 'Sex', y = 'Rate')

multi.layout = matrix(rep(c(1, 2), times = 2), 2, 2, byrow = T)

multiplot(sex.p1, sex.p2, layout = multi.layout)

mosaicplot(Survived ~ Sex,
           data = full[1:891, ], col = TRUE,
           main = 'Survival rate by passengers gender')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full <- full %>%
  # 결측치 제외한 값들의 평균으로 결측치를 채움
  mutate(Age = ifelse(is.na(Age), mean(full$Age, na.rm = TRUE), Age),
         # Age 값에 따라 범주형 파생 변수 Age.Group 생성)
         Age.Group = case_when(Age < 13 ~ 'Age.0012',
                               Age >= 13 & Age < 18 ~ 'Age.1317',
                               Age >= 18 & Age < 60 ~ 'Age.1859',
                               Age >= 60 ~ 'Age.60inf'),
         # Chr 속성을 Factor로 변환
         Age.Group = factor(Age.Group))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full <- full %>% 
  # SibSp, Parch와 1(본인)을 더해서 FamilySize라는 파생변수를 먼저 생성
  mutate(FamilySize = .$SibSp + .$Parch + 1,
         # FamilySize 의 값에 따라서 범주형 파생 변수 FamilySized를 생성
         FamilySized = dplyr::case_when(FamilySize == 1 ~ "Single",
                                        FamilySize >= 2 & FamilySize < 5 ~ "Small",
                                        FamilySize >= 5 ~ "Big"),
         # Chr 속성인 FamilySized를 factor로 변환
         # 집단 규모 크기에 따라 levels를 새로 지정
         FamilySized = factor(FamilySized, levels = c("Single", "Small", "Big")))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Name 열 추출
title <- full$Name

# 정규표현식과 gsub()을 이요해서 성별과 관련성이 높은 이름만 추출해서 title 벡터로 저장
title <- gsub("^.*, (.*?)\\..*$", "\\1", title)

# 위에서 저장한 title 벡터를 full에 파생변수로 저장
full$title <- title


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
unique(full$title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 범주별 빈도수, 비율 확인
descr::CrossTable(full$title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 5개 범주로 단순화
full <- full %>% 
  # '%in%' 대신 '=='을 쓸 시 Recycling Rule 때문에 원하는대로 되지 않음.
  mutate(title = ifelse(title %in% c('Mlle', 'Ms', 'Lady', 'Dona'), 'Miss', title),
         title = ifelse(title == 'Mme', 'Mrs', title),
         title = ifelse(title %in% c('Capt', "col",'Major', 'Dr', 'Rev', 'Don',
                                     'Sir', "the Countess", 'Jonkheer'), "officer", title),
         title = factor(title))

# 파생변수 생성 후 각 범주별 빈도수, 비율 확인
descr::CrossTable(full$title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 'unique'한 범주의 갯수만 파악하려고 length()를 사용
length(unique(full$Ticket))

# 모두 출력하면 지저분하니 10개만 출력
head(summary(full$Ticket), 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full %>% 
  # 티켓이 일치하는 11명의 승객들 필터링
  filter(Ticket == 'CA. 2343') %>% 
  # 몇 가지 변수 확인
  select(Pclass, Name, Age, FamilySized)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ticket.unique가 모두 0이라고 저장
ticket.unique <- rep(0, nrow(full))

# Ticket Feature에서 unique한 것들만 추출해서 tickets 벡터에 저장
tickets <- unique(full$Ticket)

# 반복문을 중첩 활용해서 티켓이 같은 승객들만 선별 후, 각 티켓의 길이를 추출하여 저장
for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(full$Ticket == current.ticket)
    # For loop 중첩
    for (k in 1:length(party.indexes)) {
      ticket.unique[party.indexes[k]] <- length(party.indexes)
    }
  }

# 위에서 계산한 ticket.unique를 파생변수로 저장
full$ticket.unique <- ticket.unique

# ticket.unique에 따라 세가지 범주로 나눠서 ticket.size 변수 생성
full <- full %>% 
  mutate(ticket.size = case_when(ticket.unique == 1 ~ 'Single',
                                 ticket.unique < 5 & ticket.unique >= 2 ~ 'Small',
                                 ticket.unique >= 5 ~ 'Big'),
         ticket.size = factor(ticket.size, 
                              levels = c('Single','Small','Big')))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Embarked <- replace(full$Embarked,               # 치환할 Data$feature 지정
                         which(is.na(full$Embarked)), # 결측치 찾기
                         'S')                         # 치환할 값 지정


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Fare <- replace(full$Fare, which(is.na(full$Fare)), 0)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# feature selection 전이므로 모든 변수들을 선택
train <- full[1:891, ]

test <- full[892:1309, ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>% 
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = 'fill') +
  # plot 테마 설정 : 조금 더 선명한 색깔로 변환
  scale_fill_brewer(palette = 'Set1')  +
  # Y axis setting
  scale_y_continuous(labels = percent) +
  # x, y 축 이름과 plot의 main title, sub title 설정
  labs(x = 'Pclass', y = "Rate",
       title = "Bar plot", subtitle = 'How many people survived in each Pclass?')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mosaicplot(Survived ~ Sex,
           data = train, col = TRUE,
           main = 'Survival rate by passengers gender')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>% 
  ggplot(aes(Embarked, fill = Survived)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(labels = percent) +
  labs(x = 'Embarked', y = 'Rate',
       title = 'Bar plot', subtitle = 'How many people survied in each Embarked?')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>% 
  ggplot(aes(FamilySized, fill = Survived)) + 
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(labels = percent) +
  labs(x = "FamilySized", y = 'Rate',
       title = 'Bar plot', subtitle = 'Survival rate by FamilySized')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>% 
  ggplot(aes(Age.Group, fill = Survived)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(labels = percent) +
  labs(x = 'Age group', y = 'Rate',
       title = 'Bar plot', subtitle = 'Survivla rate by Age group')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>% 
  ggplot(aes(title, fill = Survived)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(labels = percent) + 
  labs(x = 'title', y = 'Rate',
       title = 'Bar plot', subtitle = 'Survival rate by passengers title')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>% 
  ggplot(aes(ticket.size, fill = Survived)) +
  geom_bar(position = 'fill') + 
  scale_fill_brewer(palette = 'Set1') + 
  scale_y_continuous(labels = percent) +
  labs(x = 'ticket.size', y = 'Rate',
       title = 'Bar plot', subtitle = 'Survival rate by ticket.size')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Id number 제외하고 실제로 사용할 7개 입력 변수와 1개 타겟 변수를 선택, 저장
train <- train %>% 
  select('Pclass', 'Sex', 'Embarked', 'FamilySized', 'Age.Group', 'title',
         'ticket.size', 'Survived')

# Submit을 위해 Id 열 추출하여 ID에 저장
ID <- test$PassengerId

# Id와 Survived를 제외한 나머지 6개 변수 선택, 저장
test <- test %>% 
  select('Pclass', 'Sex', 'Embarked', 'FamilySized', 'Age.Group','title','ticket.size')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 재현성을 위해 seed number를 설정
set.seed(1901)

titanic.rf <- randomForest(Survived ~ ., data = train, importance = T, ntree = 2000)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
importance(titanic.rf)

varImpPlot(titanic.rf)


## ----eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## # Prediction
## pred.rf <- predict(object = titanic.rf, newdata = test, type = "class")
## 
## # Data frame generation
## submit <-  data.frame(PassengerID = ID, Survived = pred.rf)
## 
## # Write the submit data frame to file: setwd()로 지정해놓은 폴더에 csv로 생성됨.
## write.csv(submit, file = './titanic_submit.csv', row.names = F)

