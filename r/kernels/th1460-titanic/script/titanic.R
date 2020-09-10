## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ---- eval=FALSE, include=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## 
## # download
## 
## system("kaggle competitions download -c titanic")
## 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

require(dplyr)
require(magrittr)

train <- 
  read.csv("../input/train.csv",
           stringsAsFactors = FALSE) %>% as_tibble()



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# tables and figures numbers

require(captioner)

fig_nums <- captioner(prefix = "Figure")
table_nums <- captioner(prefix = "Table")

# character to factor

train %<>% 
  mutate(Survived = factor(x = Survived, labels = c("no", "yes")),
         Pclass = factor(x = Pclass, labels = c("1st", "2nd", "3rd")),
         Sex = factor(x = Sex))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

require(ggplot2)

train %>% count(Pclass, Survived) %>% 
  group_by(Pclass) %>% 
  mutate(total = sum(n)) %>% 
  ggplot(aes(Pclass, n * 100/total, fill = Survived)) + geom_col() +
  theme_light() +
  labs(x = "Ticket class", y = "%", fill = "Survived", 
       caption = fig_nums("pclass", "Percentual distribution of survivors according to ticket class"))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

require(knitr)
require(kableExtra)

model <-
  glm(Survived ~ Pclass, family = binomial, data = train)

model %>%
  summary() %$% coefficients %>% 
  kable(caption = table_nums("pclass", "Model estimation")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))
  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

train %>% count(Sex, Survived) %>% 
  group_by(Sex) %>% 
  mutate(total = sum(n)) %>% 
  ggplot(aes(Sex, n * 100/total, fill = Survived)) + geom_col() +
  theme_light() +
  labs(x = "Sex", y = "%", fill = "Survived", 
       caption = fig_nums("sex", "Percentual distribution of survivors according to sex"))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

model <-
  glm(Survived ~ Sex, family = binomial, data = train)

model %>%
  summary() %$% coefficients %>% 
  kable(caption = table_nums("sex", "Estimativas do modelo")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))
  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

model <-
  glm(Survived ~ Pclass*Sex, family = binomial, data = train)

model %>%
  summary() %$% coefficients %>% 
  kable(caption = table_nums("pclass_sex", "Model estimation")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# table with all possibilities

newdata <- 
  expand.grid(Pclass = c("1st", "2nd", "3rd"),
              Sex = c("male", "female")) %>% as_tibble()

# predictions (probability)

newdata %<>% 
  mutate(Pihat = model %>% predict(newdata = newdata, type = "response"))

newdata %>% 
  ggplot(aes(Sex, Pihat, group = Pclass, colour = Pclass)) + geom_line() + geom_point() +
  theme_light() +
  labs(x = "Sex", y = expression(pi(Survived == yes)), colour = "Ticket Class",
       caption = fig_nums("pclass_sex", "Interaction between sex and ticket class"))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

train %>% 
  ggplot(aes(Survived, Age)) + geom_boxplot() +
  theme_light() +
  labs(caption = fig_nums("age", "Distribution of age according to survived status"))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

model <-
  glm(Survived ~ Age, family = binomial, data = train)

model %>%
  summary() %$% coefficients %>% 
  kable(caption = table_nums("age", "Model estimation")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))
  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

model <-
  glm(Survived ~ Pclass*Age, family = binomial, data = train)

model %>%
  summary() %$% coefficients %>% 
  kable(caption = table_nums("pclass_age", "Model estimation")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))


