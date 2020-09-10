
#data wrangling library
library(tidyverse)
library(tibble)

#collecting data
files_directory <- list.files(path = "../input")

train_data <- as_data_frame(read_csv('../input/train.csv'))
test_data <- as_data_frame(read_csv('../input/test.csv'))
reality <- as_data_frame(read_csv('../input/gender_submission.csv'))

train_data$set <- "train"
test_data$set  <- "test"
test_data$Survived <- NA

full_data <- rbind(train_data, test_data)
full_data_unedited <- rbind(train_data, test_data)

#check for missing value for possible feature (non-output)

full_data %>% 
    select(-Survived) %>%
        summarize_all(funs(sum(is.na(.))/n())) -> miss_valperc

miss_valperc <- gather(miss_valperc, key="feature", value="missing_pct")

#loading the ggplot2
library(ggplot2)

#visualize missing data
ggplot(miss_valperc, aes(x = feature, y = missing_pct)) + geom_bar(stat = "identity", fill = "blue") + coord_flip()

full_data %>% select(-Cabin) -> full_data

glimpse(full_data)

full_data %>% select(-PassengerId) -> full_data

#counting the percentage of output 0 and 1
full_data %>% 
    filter(set == 'train') %>%
        count(Survived) %>%
            mutate(percentage = n/sum(n)) -> y_ratio

y_ratio

full_data %>% 
    count(Pclass) 

full_data <- full_data %>% 
    mutate(Pclass = factor(Pclass, levels = c(1, 2, 3), labels = c("First", "Second", "Third"))) 

class(full_data$Pclass)

full_data %>%
    mutate(title = gsub("^.*, (.*?)\\..*$", "\\1", Name)) -> full_data

full_data %>%
    count(title)

# Assigning obvious title to relevant major category
full_data$title[full_data$title == 'Mlle'] <- 'Miss' 
full_data$title[full_data$title == 'Ms']  <- 'Miss'
full_data$title[full_data$title == 'Mme'] <- 'Mrs' 

#Assign the rest to Honored People / Important People
ordinary_ppl <- list('Mlle','Ms','Mme','Mr','Mrs','Miss') 
full_data$title[!(full_data$title %in% ordinary_ppl)] <- 'Honored_Important'

#count the distribution of title
full_data %>% count(title)

full_data %>% 
    mutate(title = factor(title, levels = c('Honored_Important','Mr','Mrs','Miss'))) -> full_data

full_data %>%
    count(Sex)

full_data %>% 
    mutate(Sex = factor(Sex, levels = c('male', 'female'))) -> full_data

full_data %>%
    mutate(Age = ifelse(is.na(Age), round(mean(Age, na.rm = TRUE)), Age)) -> full_data

#categorizing data
full_data <- full_data %>%
    mutate(age_cat = factor(case_when(Age < 13 ~ "Children",
                              Age >= 13 & Age < 18 ~ "Teen",
                              Age >= 18 & Age <= 55 ~ "Adult",
                              Age > 55 ~ "Elder"), levels = c('Children', 'Teen', 'Adult', 'Elder'))) 

#take a look at its distribution
full_data %>% count(age_cat)

full_data %>%
    mutate(Family_inship = SibSp + Parch + 1) -> full_data

#categorizing
full_data <- full_data %>%
    mutate(Family_inship = factor(case_when(Family_inship == 1 ~ "Alone",
                                           Family_inship >= 2 & Family_inship <=5 ~ "Small",
                                           Family_inship >5 ~ "Big"), levels = c("Alone", "Small", "Big")))

#how its distributed
full_data %>%
    count(Family_inship)

#categorizing

# make category for ticket by count
ticket_count <- full_data %>%
        count(Ticket) %>%
            mutate(ticket_for = factor(case_when(n == 1 ~ "Personal",
                                                  n >= 2 & n<=5 ~"Small group",
                                                n>5 ~ "Big group"),
                                      levels = c('Personal', 'Small group', 'Big group')))
#merge
full_data <- full_data %>%
        inner_join(ticket_count, by = 'Ticket')

#how its distributed
full_data %>%
    count(ticket_for)

full_data %>% select(-Fare) -> full_data

full_data <- full_data %>%
        mutate(Embarked = ifelse(is.na(Embarked),'S',Embarked))

#categorizing
full_data <- full_data %>%
    mutate(Embarked = factor(Embarked, levels = c('C', 'Q', 'S')))

#See how its distributed
full_data %>%
    count(Embarked)

full_data <- full_data %>%
        select(Survived,Pclass, title, Sex, age_cat, Family_inship, ticket_for, Embarked, set )

#take a look at summary of our result in wrangling the data
summary(full_data)

#changing output (Survived) variable into categorical
full_data %>%
    mutate(Survived = factor(Survived, levels = c(0, 1, NA), labels = c("Not Survived", "Survived"))) -> full_data

#subset
full_data %>% filter(set == 'train') -> train_data

#using chi square to evaluate
Pclass_eff <- chisq.test(train_data$Pclass, train_data$Survived)
Pclass_eff

#visualizing both variable using barplot
train_data %>%
    ggplot(aes(x = Pclass, fill = Survived)) + geom_bar(position = "fill") + ylab("Pclass %")

#using chi square to evaluate
title_eff <- chisq.test(train_data$title, train_data$Survived)
title_eff

#visualizing both variable using barplot
train_data %>%
    ggplot(aes(x = title, fill = Survived)) + geom_bar(position = "fill") + ylab("Title %")

#using chi square to evaluate
sex_eff <- chisq.test(train_data$title, train_data$Survived)
sex_eff

#visualizing both variable using barplot
train_data %>%
    ggplot(aes(x = Sex, fill = Survived)) + geom_bar(position = "fill") + ylab("Sex %")

#using chi square to evaluate
acat_eff <- chisq.test(train_data$age_cat, train_data$Survived)
acat_eff

#visualizing both variable using barplot
train_data %>%
    ggplot(aes(x = age_cat, fill = Survived)) + geom_bar(position = "fill") + ylab("Age Category %")

#inspecting why elder is least prioritized 
#by their title and Sex
train_data %>% filter(age_cat == 'Adult' | age_cat == 'Elder') %>%
    group_by(age_cat) %>%
        count(title, Sex) %>% select(age_cat, title, Sex, n) %>% mutate(n_perc = n/sum(n))

#using chi square to evaluate
famship_eff <- chisq.test(train_data$Family_inship, train_data$Survived)
famship_eff

#visualizing both variable using barplot
train_data %>%
    ggplot(aes(x = Family_inship, fill = Survived)) + geom_bar(position = "fill") + ylab("Number of Family member in ship %")

#using chi square to evaluate
ticket_eff <- chisq.test(train_data$ticket_for, train_data$Survived)
ticket_eff

#visualizing both variable using barplot
train_data %>%
    ggplot(aes(x = ticket_for, fill = Survived)) + geom_bar(position = "fill") + ylab("Type ticket purchased %")

#using chi square to evaluate
emb_eff <- chisq.test(train_data$Embarked, train_data$Survived)
emb_eff

#visualizing both variable using barplot
train_data %>%
    ggplot(aes(x = Embarked, fill = Survived)) + geom_bar(position = "fill") + ylab("Embarked on port %")

#omitting Embarked variable
full_data <- full_data %>% select(-Embarked)

#this is the ration between minority and majority class
y_ratio

#load necessary package
library(caret)

#subset to get the training data
train_data <- full_data %>% filter(set == 'train')

#we will use k-fold cross validation
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model
model <- train(Survived ~ Pclass + title + Sex + age_cat + Family_inship + ticket_for, 
               data = train_data, method = "rf", trControl = train.control)

# Summarize the results
print(model)

test_data <- full_data %>% filter (set == 'test')
result <- predict(model, test_data)

# Write CSV in R
write.csv(result, file = "newest_titanic.csv")


