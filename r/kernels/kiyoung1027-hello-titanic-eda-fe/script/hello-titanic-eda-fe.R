## ----message = F, warning = F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Data manipulation
library(readr) # Loading dataset faster than default function
library(dplyr) # Manipulating dataset more easily
library(tidyr) # Changing dataset's structure
library(stringr) # Manipulating string datatype

## Visualization
library(ggplot2) # Many kinds of plots such as barchart and histogram 
library(gridExtra) # Multiple plots in one page
library(ggcorrplot) # Corelation plot
library(ggridges) # Density plots in ggplot2
library(rpart.plot) # Nice plot for decision tree

## Machine learning
library(rpart) # Decision tree
library(caret) # Useful package for modeling


## ----message = F-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read_csv('../input/train.csv')
test <- read_csv('../input/test.csv')

names(train) <- tolower(names(train)) # It is for avoiding confusion of coding.
names(test) <- tolower(names(test))

combine <- bind_rows(train, test)
str(combine)


## ----message = F, fig.align = 'center'-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
temp_train <- as.data.frame(sapply(train, function(x) sum(is.na(x))))
names(temp_train) <- 'freq'

p1 <- ggplot(temp_train, aes(rownames(temp_train), freq)) + 
  geom_bar(stat = 'identity') + 
  geom_label(aes(label = freq)) + 
  scale_y_continuous(limit = c(0, 700)) + 
  labs(x = 'Column Name', y = 'Num of NA', title = 'Train') + 
  theme_bw()

temp_test <- as.data.frame(sapply(test, function(x) sum(is.na(x))))
names(temp_test) <- 'freq'

p2 <- ggplot(temp_test, aes(rownames(temp_test), freq)) + 
  geom_bar(stat = 'identity') + 
  geom_label(aes(label = freq)) + 
  scale_y_continuous(limit = c(0, 700)) + 
  labs(x = 'Column Name', y = 'Num of NA', title = 'Test') + 
  theme_bw()

grid.arrange(p1, p2)


## ----message = F-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine <- combine %>% mutate(
  survived = as.factor(survived), 
  embarked = as.factor(embarked), 
  sex = as.factor(sex), 
  title = as.factor(str_extract(name, '[A-Z][a-z]*\\.')), 
  deck = if_else(is.na(cabin), 'U', str_sub(cabin, 1, 1)) # U: Unknown deck
)

temp <- combine %>% 
  group_by(ticket) %>% 
  summarise(ticket_freq = n())

combine <- left_join(combine, temp, by = 'ticket')

train <- combine %>% 
  filter(!is.na(survived))

test <- combine %>% 
  filter(!is.na(survived))


## ----message = F, warning = F, fig.width = 15, fig.height = 5, fig.align = 'center'----------------------------------------------------------------------------------------------------------------------------------------------
plot_list <- list()

# Variable: survived
plot_list[[1]] <- ggplot(train, aes(survived, fill = survived)) + 
  geom_bar() + 
  labs(x = 'Survived', y = 'Count') + 
  guides(fill = F) + 
  theme_bw()

# Variable: pclass
plot_list[[2]] <- ggplot(train, aes(pclass, fill = as.factor(pclass))) + 
  geom_bar() + 
  labs(x = 'Pclass', y = 'Count', fill = 'Type') + 
  guides(fill = F) + 
  theme_bw()

# Variable: name
plot_list[[3]] <- train %>% 
  group_by(title) %>% 
  count() %>% 
  ggplot(aes(title, n, fill = title)) + 
  geom_bar(stat = 'identity') + 
  labs(x = 'Titles', y = 'Count') + 
  guides(fill = F) + 
  theme_bw() + 
  coord_flip()

# Variable: name
plot_list[[4]] <- ggplot(train, aes(title, fill = sex)) + 
  geom_bar(position = 'fill') + 
  labs(x = NULL, y = 'Ratio', fill = 'Sex') + 
  coord_flip() + 
  theme_bw()

grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 4)

# Variable: sex
plot_list[[5]] <- ggplot(train, aes(sex, fill = sex)) + 
  labs(x = 'Sex', y = 'Count', fill = 'Sex') + 
  geom_bar() + 
  labs(x = 'Sex') + 
  guides(fill = F) + 
  theme_bw()

# Variable: age
plot_list[[6]] <- ggplot(train, aes(age)) + 
  geom_histogram() + 
  labs(x = 'Age', y = 'Count') + 
  theme_bw()

# Variables: sibsp
plot_list[[7]] <- ggplot(train, aes(sibsp, fill = as.factor(sibsp))) + 
  geom_bar() + 
  scale_y_continuous(limit = c(0, 700)) + 
  labs(x = 'SibSp', y = 'Count') + 
  guides(fill = F) + 
  theme_bw()

# Variables: parch
plot_list[[8]] <- ggplot(train, aes(parch, fill = as.factor(parch))) + 
  geom_bar() + 
  labs(x = 'ParCh', y = 'Count') + 
  scale_y_continuous(limit = c(0, 700)) + 
  guides(fill = F) + 
  theme_bw()

grid.arrange(plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]], ncol = 4)

# Variable: ticket_freq
plot_list[[9]] <- ggplot(train, aes(ticket_freq, fill = as.factor(ticket_freq))) + 
  geom_bar() + 
  scale_x_continuous(breaks = 0:10) + 
  labs(x = 'Ticket_Freq', y = 'Count') + 
  guides(fill = F) + 
  theme_bw()

# Variable: fare
plot_list[[10]] <- ggplot(train, aes(fare)) + 
  geom_histogram() + 
  labs(x = 'Fare', y = 'Count') + 
  theme_bw()

# Variable: deck
plot_list[[11]] <- train %>% 
  group_by(deck) %>% 
  count() %>% 
  ggplot(aes(reorder(deck, -n, fun = max), n, fill = deck)) + 
  geom_bar(stat = 'identity') + 
  labs(x = 'Deck', y = 'Count') + 
  guides(fill = F) + 
  theme_bw()

grid.arrange(plot_list[[9]], plot_list[[10]], plot_list[[11]], ncol = 3)


## ----message = F, fig.align = 'center'-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>% 
  select(-passengerid, -name, -cabin, -ticket, -title, -deck, -embarked) %>%
  mutate(sex = if_else(sex == 'male', 1, 0)) %>% 
  mutate(sex = as.integer(sex),
         pclass = as.integer(pclass),
         survived = as.integer(survived)) %>%
  cor(use = 'complete.obs') %>% 
  ggcorrplot(type = 'lower', lab = T, outline.col = 'black')


## ----message = F-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
get.binCI <- function(x, n) {
  as.list(setNames(binom.test(x, n)$conf.int, c('lwr', 'upr')))}


## ----message = F, warning = F, fig.width = 15, fig.height = 5, fig.align = 'center'----------------------------------------------------------------------------------------------------------------------------------------------
# age vs survived
plot_list[[1]] <- ggplot(train, aes(age, fill = survived)) + 
  geom_density(alpha = 0.25) + 
  labs(x = 'Age', y = 'Density', fill = 'Surv') + 
  theme_bw()

# fare vs survived
plot_list[[2]] <- ggplot(train, aes(fare, fill = survived)) + 
  geom_density(alpha = 0.25) + 
  scale_x_log10() + 
  labs(x = 'Fare', y = 'Density', fill = 'Surv') + 
  theme_bw()

grid.arrange(plot_list[[1]], plot_list[[2]], ncol = 1)

# pclass vs survived
plot_list[[3]] <- train %>% 
  group_by(pclass, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(pclass, ratio, fill = as.factor(pclass))) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  labs(x = 'Pclass', y = 'Survial Rate') + 
  guides(fill = F) + 
  theme_bw()

# sex vs survived
plot_list[[4]] <- train %>% 
  group_by(sex, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(sex, ratio, fill = sex)) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  labs(x = 'Sex', y = 'Survial Rate') + 
  guides(fill = F) + 
  theme_bw()

# sibsp vs survived
plot_list[[5]] <- train %>% 
  group_by(sibsp, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(sibsp, ratio, fill = as.factor(sibsp))) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  labs(x = 'SibSp', y = 'Survial Rate') + 
  guides(fill = F) + 
  theme_bw()

# parch vs survived
plot_list[[6]] <- train %>% 
  group_by(parch, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(parch, ratio, fill = as.factor(parch))) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  labs(x = 'ParCh', y = 'Survial Rate') + 
  guides(fill = F) + 
  theme_bw()

grid.arrange(plot_list[[3]], plot_list[[4]], plot_list[[5]], plot_list[[6]], ncol = 4)

# ticket_freq vs survived
plot_list[[7]] <- train %>% 
  group_by(ticket_freq, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(ticket_freq, ratio, fill = as.factor(ticket_freq))) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  labs(x = 'Ticket_Freq', y = 'Survial Rate') + 
  guides(fill = F) + 
  theme_bw()

# deck vs survived
plot_list[[8]] <- train %>% 
  group_by(deck, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(deck, ratio, fill = deck)) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  labs(x = 'Deck', y = 'Survial Rate') + 
  guides(fill = F) + 
  theme_bw()

# embarked vs survived
plot_list[[9]] <- train %>% 
  group_by(embarked, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(embarked, ratio, fill = embarked)) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  labs(x = 'Embarked', y = 'Survial Rate') + 
  guides(fill = F) + 
  theme_bw()

grid.arrange(plot_list[[7]], plot_list[[8]], plot_list[[9]], ncol = 3)

# title vs survived
train %>% 
  group_by(title, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(title, ratio, fill = title)) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  labs(x = 'Title', y = 'Survival Rate') + 
  guides(fill = F) + 
  theme_bw()


## ----message = F, warning = F, fig.width = 10, fig.height = 5, fig.align = 'center'----------------------------------------------------------------------------------------------------------------------------------------------
# sibsp vs parch vs survived
p1 <- train %>% 
  ggplot(aes(survived, fill = survived)) + 
  geom_bar() + 
  labs(x = 'X-axis: SibSp, Y-axis: ParCh', y = 'Count', fill = 'Surv') + 
  theme_bw() + 
  facet_grid(parch ~ sibsp)

p2 <- train %>% 
  ggplot(aes('', fill = survived)) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(breaks = c(0, 0.5, 1)) + 
  labs(x = 'X-axis: SibSp, Y-axis: ParCh', y = 'Ratio', fill = 'Surv') + 
  theme_bw() + 
  facet_grid(parch ~ sibsp)

grid.arrange(p1, p2, ncol = 2)

# pclass vs fare vs survived
train %>% 
  filter(fare != 0) %>% 
  ggplot(aes(as.factor(pclass), fare)) + 
  geom_boxplot(aes(fill = survived)) + 
  scale_y_log10() + 
  labs(x = 'Pclass', y = 'Fare', fill = 'Surv') + 
  theme_bw()

# sex vs pclass vs survived
train %>% 
  group_by(sex, pclass, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(sex, ratio, fill = sex)) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  facet_grid(~ pclass) + 
  labs(x = 'Sex, X-axis: Pclass', y = 'Survival Rate') + 
  guides(fill = F) + 
  theme_bw()

# deck vs pclass vs survived
train %>% 
  group_by(deck, pclass, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(deck, ratio, fill = deck)) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  labs(x = 'Deck, Y-axis: Pclass', y = 'Survival Rate') + 
  facet_wrap(~ pclass, ncol = 1) + 
  guides(fill = F) + 
  theme_bw()

# age vs sex vs survived
train %>% 
  ggplot(aes(age, fill = survived)) + 
  geom_density(alpha = 0.5) + 
  labs(x = 'Age, Y-axis: Sex', y = 'Density', fill = 'Surv') + 
  facet_wrap(~ sex, ncol = 1) + 
  theme_bw()


## ----message = F, warning = F, fig.width = 12.5, fig.height = 5, fig.align = 'center'--------------------------------------------------------------------------------------------------------------------------------------------
# age_group vs survived, sex == male
temp <- train %>% 
  filter(sex == 'male')

temp$age_group <- NA

for(i in 1:20) {
  if(i == 1) {
    temp$age_group[temp$age > 5*(i - 1) & temp$age <= 5*i] <- '01 ~ 05'
  } else if(i == 2) {
    temp$age_group[temp$age > 5*(i - 1) & temp$age <= 5*i] <- '06 ~ 10'
  }
  else{
    temp$age_group[temp$age > 5*(i - 1) & temp$age <= 5*i] <- paste0(5*(i - 1) + 1, ' ~ ', 5*i)
  }
}

temp_count <- temp %>% 
  group_by(age_group) %>% 
  summarise(count = n())

temp_ratio <- temp %>% 
  group_by(age_group, survived) %>% 
  summarise(count = n()) %>% 
  mutate(percent = round(count/sum(count)*100, 1))

ggplot(temp) + 
  geom_bar(aes(age_group, fill = survived)) + 
  geom_text(data = temp_count, aes(age_group, count, label = count), vjust = -0.25, fontface = 'bold') + 
  geom_label(data = temp_ratio, aes(age_group, count, label = paste0(percent, '%'), group = survived), 
             position = position_stack(vjust = 0.5)) + 
  labs(x = 'Age_group', y = 'Count', fill = 'Surv') + 
  theme_bw()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine <- combine %>% 
  mutate(family = sibsp + parch + 1)


## ---- echo = F, message = F, warning = F, fig.width = 10, fig.height = 5, fig.align = 'center'-----------------------------------------------------------------------------------------------------------------------------------
p1 <- combine %>% 
  filter(!is.na(survived)) %>% 
  ggplot(aes(as.factor(family), fill = as.factor(family))) + 
  geom_bar() + 
  labs(x = 'Family', y = 'Count') + 
  guides(fill = F) + 
  theme_bw()

p2 <- combine %>% 
  filter(!is.na(survived)) %>% 
  group_by(family, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(as.factor(family), ratio)) + 
  geom_bar(stat = 'identity', aes(fill = as.factor(family))) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  labs(x = 'Family', y = 'Survival Rate') + 
  guides(fill = F) + 
  theme_bw()

grid.arrange(p1, p2, ncol = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine <- combine %>% 
  mutate(fare_eff = fare/ticket_freq)


## ----message = F, warning = F, fig.width = 10, fig.height = 5, fig.align = 'center'----------------------------------------------------------------------------------------------------------------------------------------------
p1 <- combine %>% 
  filter(!is.na(survived), fare_eff != 0) %>% 
  ggplot(aes(fare, pclass, fill = as.factor(pclass))) + 
  geom_density_ridges() + 
  scale_x_log10() + 
  labs(x = 'Fare', y = 'Pclass') + 
  guides(fill = F) + 
  theme_bw()

p2 <- combine %>% 
  filter(!is.na(survived), fare_eff != 0) %>% 
  ggplot(aes(fare_eff, pclass, fill = as.factor(pclass))) + 
  geom_density_ridges() + 
  scale_x_log10() + 
  labs(x = 'Fare_eff', y = 'Pclass') + 
  guides(fill = F) + 
  theme_bw()

grid.arrange(p1, p2, ncol = 2)


## ----message = F, warning = F, fig.width = 16, fig.height = 5, fig.align = 'center'----------------------------------------------------------------------------------------------------------------------------------------------
combine %>% 
  filter(!is.na(survived), fare_eff != 0) %>% 
  ggplot(aes(pclass, fare, fill = as.factor(pclass))) + 
  geom_boxplot() + 
  facet_wrap(~ ticket_freq, ncol = 9) + 
  scale_y_log10() + 
  labs(x = 'Fare_eff', y = 'Pclass') + 
  guides(fill = F) + 
  theme_bw()


## ----message = F-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine <- combine %>% 
  mutate(fare_free = if_else(fare == 0, T, F))


## ----message = F-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine %>% 
  filter(fare_free == T, !is.na(survived)) %>% 
  group_by(survived) %>% 
  summarise(count = n())

combine %>% 
  filter(fare_free == T) %>% 
  select(-c(survived, passengerid, name, cabin, title, fare_free, fare, sibsp, parch)) %>% 
  as.data.frame()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine <- combine %>% 
  mutate(age_known = !is.na(age))


## ----fig.align = 'center'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine %>% 
  filter(!is.na(survived)) %>% 
  group_by(age_known, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(age_known, ratio)) + 
  geom_bar(stat = 'identity', aes(fill = age_known)) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  labs(x = 'Age_known', y = 'Survival Rate') + 
  guides(fill = F) + 
  theme_bw()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine <- combine %>% 
  mutate(cabin_known = !is.na(cabin))


## ---- echo = F, message = F, warning = F, fig.align = 'center'-------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine %>% 
  filter(!is.na(survived)) %>% 
  group_by(cabin_known, survived) %>% 
  summarise(count = n()) %>% 
  spread(survived, count) %>% 
  mutate(ratio = `1`/(`0` + `1`), 
         lwr = get.binCI(`1`, (`0` + `1`))[[1]], 
         upr = get.binCI(`1`, (`0` + `1`))[[2]]) %>% 
  ggplot(aes(cabin_known, ratio)) + 
  geom_bar(stat = 'identity', aes(fill = cabin_known)) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 1, width = 0.5) + 
  labs(x = 'Cabin_known', y = 'Survival Rate') + 
  guides(fill = F) + 
  theme_bw()


## ---- echo = F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- combine %>% # It's for adding new features to train dataset
  filter(!is.na(survived))

test <- combine %>% # It's for adding new features to test dataset
  filter(is.na(survived)) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>% 
  filter(is.na(embarked)) %>% 
  as.data.frame()


## ----fig.align = 'center'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine %>% 
  filter(!is.na(embarked), ticket_freq == 2, pclass == 1) %>% 
  ggplot(aes(fare)) + 
  geom_density() + 
  geom_vline(xintercept = 80, linetype='dashed', colour = 'red', size = 1) + 
  facet_grid(~ embarked) + 
  labs(x = 'Fare', y = 'Density') + 
  theme_bw()

train[is.na(train$embarked), 'embarked'] <- 'C'


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test %>% 
  filter(is.na(fare)) %>% 
  as.data.frame()


## ----message = F, warning = F, fig.align = 'center'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tr_ctrl <- trainControl(method = 'repeatedcv', 
                        number = 10, 
                        repeats = 10)

model_dt <- train(fare ~ pclass + ticket_freq + embarked, 
                  data = combine %>% filter(!is.na(fare)), 
                  trControl = tr_ctrl, 
                  na.action = na.pass, 
                  method = 'rpart', tuneLength = 10)

temp <- predict(model_dt, test[which(is.na(test$fare)), c('pclass', 'ticket_freq', 'embarked')]) %>% round(2)
test[which(is.na(test$fare)), 'fare'] <- temp

rpart.plot(model_dt$finalModel)

model_dt$results

