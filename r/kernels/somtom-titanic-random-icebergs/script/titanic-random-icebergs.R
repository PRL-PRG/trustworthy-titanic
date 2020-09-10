## ---- warning = F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
require(tidyverse)
require(reshape2)
require(caret)
require(kknn)
require(randomForest)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
test$Survived <- NA

train$dataSet <- "train"
test$dataSet <- "test"

dt <- bind_rows(train, test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt <- dt %>% 
  separate(Name, "[,.]", into = c("Surename", "Title", "Name"), extra = "drop") %>% 
  mutate(Title = str_trim(Title, side = "left"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(dt$Title) %>% melt %>% arrange(desc(value))


## ---- echo = T-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt %>% 
  group_by(Title, Survived) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  group_by(Title) %>% 
  mutate(total = sum(freq),
         prop = freq/total) %>% 
  arrange(total) %>% 
  ggplot(aes(x = reorder(Title, total),  y = prop, group = Survived)) +
  geom_col(aes(fill = factor(Survived))) +
  geom_text(aes(label = freq), position = position_fill(0.5)) +
  guides(fill = guide_legend(title = "Survived"), color = guide_legend(title = "n-observations")) +
  xlab("Titel") +
  ylab("proportion") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
newTitles <- data.frame(Mr = dt$Title %in% c("Don", "Capt", "Jonkheer", "Sir", "Major", "Col"),
                        Mrs = dt$Title %in% c("Dona", "Lady", "Mme", "the Countess"),
                        Ms = dt$Title == "Mlle")

dt$Title[newTitles$Mr] <- "Mr"
dt$Title[newTitles$Ms] <- "Ms"
dt$Title[newTitles$Mrs] <- "Mrs"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt <- dt %>% 
  mutate(famSize = Parch + SibSp + 1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt %>% 
  #filter(!is.na(Survived)) %>% 
  group_by(famSize, Survived) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  group_by(famSize) %>% 
  mutate(total = sum(freq),
         prop = freq/total) %>% 
  ggplot(aes(x = famSize, y = prop, group = Survived))+
  geom_col(aes(fill = factor(Survived))) +
  geom_text(aes(label = freq), color = "white", cex = 4, position = position_fill(0.5)) +
  geom_text(label = "Total:",x = 0,  y = 1.1) +
  geom_text(aes(label = total, y = 1.1)) +
  guides(fill = guide_legend(title = "Survived"), color = guide_legend(title = "n-observations")) +
  scale_x_continuous(breaks = 1:11, lim = c(0,11.5)) 

dt %>% 
  filter(!is.na(Survived)) %>% 
  group_by(famSize, Survived) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  group_by(famSize) %>% 
  mutate(total = sum(freq),
         prop = freq/total) %>% 
  ggplot(aes(x = famSize, y = prop, group = Survived))+
  geom_col(aes(fill = factor(Survived)), width = 1, color = "white") +
  geom_text(aes(label = freq), color = "white", cex = 4, position = position_fill(0.5)) +
  guides(fill = guide_legend(title = "Survived")) +
  scale_x_continuous(breaks = 1:11) +
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 7.5)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt <- dt %>% 
  mutate(famSize = ifelse(famSize == 1,
                          "alone",
                          ifelse(famSize %in% 2:4, 
                                 "small",
                                 ifelse(famSize %in% 5:7,
                                        "medium",
                                        "big"))))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt %>%
  filter(!is.na(Survived)) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(famSize, Survived), fill = factor(Survived)), position = "fill") +
  guides(fill = guide_legend(title = "Survived")) +
  ylab("Proportion") +
  xlab("Family Size") 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt <- dt %>%
  mutate_if(is.character, factor) %>% 
  mutate(Survived = factor(Survived))


## ---- echo = F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# function from my custom library "somtomlib" to plot missing values
ggplot_missing_col <- function(x){
  require(reshape2)
  require(ggplot2)
  require(dplyr)
  require(purrr)

  if (is.vector(x)) { x <- data.frame(vector = x)}

  classes <- x %>% map(class) %>% melt %>% rename(Var = L1, Class = value)
  col.order <- x %>%
    map(function(s) sum(is.na(s))) %>%
    melt %>%
    arrange(desc(value)) %>%
    select(L1) %>%
    unlist

  x %>%
    is.na %>%
    melt %>%
    rename(Observation = Var1, Var = Var2) %>%
    mutate(Var = as.character(Var)) %>%
    left_join(classes, by = "Var") %>% mutate(Var = factor(Var, levels = col.order)) %>%
    mutate(Class = ifelse(value == TRUE, NA, as.character(Class))) %>%
    ggplot(data = .,
           aes(x = Var)) +
    geom_raster(aes(y = Observation, fill = Class)) +
    scale_fill_discrete(name = "", na.value = "black") + 
    theme_minimal() +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) +
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt$Cabin <- ifelse(dt$Cabin == "", NA, dt$Cabin)


## ---- fig.align='center'---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot_missing_col(dt)


## ---- warnings = F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt %>% 
  map_df(~sum(is.na(.))) %>%
  melt(id.vars = NULL) %>% 
  filter(value > 0) %>% 
  arrange(desc(value))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt[is.na(dt$Fare),]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt[is.na(dt$Fare),]$Fare <- median(subset(dt$Fare, dt$Pclass == 3 & dt$Age > 50), na.rm = T)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tmp <- dt %>% 
  filter(!is.na(Age)) %>%
  select(-Cabin, -Survived, - PassengerId, -Surename, -Name, -dataSet, -Ticket)


set.seed(231)
cv <- caret::train(Age ~. ,data = tmp, tuneGrid = expand.grid(kmax = 5:20,
                                                               distance = 2,
                                                               kernel = "optimal"),
                   trControl = trainControl(method = "cv", number = 10), method = "kknn")
 
plot(cv)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kAge <- kknn(Age~., train = tmp, test = dt %>% filter(is.na(Age)), k = 15)
dt[is.na(dt$Age),]$Age <- kAge$fitted.values


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt <- dt %>% select(-Cabin)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dt <- dt %>% select(-Ticket, -Name, -Surename)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- dt %>% filter(dataSet == "train") %>% select(- dataSet, - PassengerId)
test <- dt %>% filter(dataSet == "test") %>% select(- dataSet)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rf.grid <- expand.grid(mtry = 2:9)


set.seed(234)
rf.cv <- caret::train(Survived ~., data = train, tuneGrid = rf.grid,
                      method = "rf")
plot(rf.cv)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
confusionMatrix.train(rf.cv)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(848)
rf.fit <- randomForest(Survived ~., data = train, mtry = 9, ntree = 500)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
submission <- data.frame(PassengerId = test$PassengerId,
                         Survived = predict(rf.fit, test))
write.csv(submission, "./submission.csv", row.names = F)

