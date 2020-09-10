
suppressWarnings(suppressMessages(library("tidyverse")))
suppressWarnings(suppressMessages(library("caret")))
suppressWarnings(suppressMessages(library("ranger"))) # Faster RF modeling
suppressWarnings(suppressMessages(library("wru"))) # Bayesian prediction of ethnicity

# Custom function
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

# Random seed
set.seed(43287)
options(repr.plot.width=6, repr.plot.height=4)

# Load data
test.csv <- read.csv('../input/test.csv', stringsAsFactors = FALSE)
train.csv <- read.csv('../input/train.csv', stringsAsFactors = FALSE)
train.csv$Survived <- as.factor(train.csv$Survived)
train <- train.csv
test <- test.csv

# Impute missing ages in Test data
test.csv %>% select(-Ticket, -Name, -Cabin, -Embarked) -> test.ages
pre.proc <- preProcess(test.ages, method = "bagImpute")
test.ages <- predict(pre.proc, test.ages)
test$Age <- test.ages$Age

# Impute missing ages in Training data
train.csv %>% select(-Ticket, -Name, -Cabin, -Embarked) -> train.ages
pre.proc <- preProcess(train.ages, method = "bagImpute")
train.ages <- predict(pre.proc, train.ages)
train$Age <- train.ages$Age

train %>%
  rowwise %>%
  mutate(title = gsub('(.*, )|(\\..*)', '', Name)) -> train

# Titles
train %>%
  ggplot(., aes(title, fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by titles")

# Special titles
train %>%
  filter(title %not in% c("Mr", "Ms", "Mrs", "Miss", "Master")) %>%
  ggplot(., aes(title, fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by special titles by sex") + facet_wrap(~ Sex)

train %>%
  mutate(child = ifelse(Age < 15, 1, 0),
         title.class = case_when(
            title %in% c("Master") ~ "Boy",
            title %in% c("Miss") & child == 1 ~ "Girl",
            title %in% c("Mrs") ~ "Woman",
            title %in% c("Miss", "Mme", "Mlle", "Ms") ~ "Lady",
            title %in% c("Dr") & Sex == "female" ~ "Woman",
            title %in% c("Mr") ~ "Man",
            title %in% c("Capt", "Don", "Col", "Major", "Dr", "Rev", 
                         "Dona", "Jonkheer", "the Countess", "Lady", "Sir") ~ "Special",
            TRUE ~ "Other")) -> train

train %>%
  ggplot(., aes(x=title.class, fill=Survived)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by title and sex") + facet_wrap(~ Sex)

train %>%
  ggplot(., aes(x=title.class, fill=Survived)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by title and Pclass") + facet_wrap(~ Pclass)

train %>% ungroup %>% count(title.class)

train %>%
  mutate(family.size = SibSp + Parch,
         family.size.simple = case_when(
            family.size == 0 ~ "0",
            between(family.size, 1, 3) ~ "1-3",
            family.size > 3 ~ ">3")) -> train

train %>%
  ggplot(., aes(family.size)) +
  geom_histogram(bins=30)

train %>%
  ggplot(., aes(family.size, fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by family size and sex") + facet_wrap(~ Sex)

train %>%
  ggplot(., aes(family.size.simple, fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by family size and sex") + facet_wrap(~ Sex)

train %>%
  ggplot(., aes(family.size.simple, fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by family size and Pclass") + facet_wrap(~ Pclass)

train %>% ungroup %>% count(family.size.simple)

train %>%
  ggplot(., aes(Fare)) +
  geom_histogram(bins=30)

train %>%
  rowwise %>%
  mutate(Fare.grps = case_when(
            between(Fare, 0, 100) ~ "0-100",
            Fare > 100 ~ ">100")) -> train

train %>%
  ggplot(., aes(Fare.grps, fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by fare groups and class") + facet_wrap(~ Pclass)

train %>% ungroup %>% count(Fare.grps)

train %>%
  ggplot(., aes(Pclass, fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by passenger class")

train %>%
  ggplot(., aes(Age)) +
  geom_histogram(bins=30)

train %>%
  mutate(Age.grps = case_when(
            between(Age, 0, 12) ~ "young",
            between(Age, 12, 50) ~ "middle",
            Age > 50 ~ "old")) -> train

train %>%
  ggplot(., aes(Age.grps, fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Age groups survival by Pclass") + facet_wrap(~ Pclass)

train %>%
  ggplot(., aes(Age.grps, fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Age groups survival by Sex") + facet_wrap(~ Sex)

train %>%
  ggplot(., aes(Age.grps, fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Age groups survival by Sex and Pclass") + facet_wrap(~ Sex*Pclass)

train %>% count(Age.grps)

train %>%
    rowwise %>%
    mutate(ticket.prefix = ifelse(grepl(" ", Ticket), sub(" .*", "", Ticket), ""),
           ticket.prefix = toupper(gsub("(\\.)|(\\/)", "", ticket.prefix)),
           ticket.prefix = case_when(
            ticket.prefix == "STONO" ~ "SOTONO",
            ticket.prefix == "STONO2" ~ "SOTONO",
            ticket.prefix == "SOTONOQ" ~ "SOTONO",
            ticket.prefix == "SOTONO2" ~ "SOTONO",
            ticket.prefix == "SCAH" ~ "SCA",
            ticket.prefix == "SCA4" ~ "SCA",
            ticket.prefix == "PPP" ~ "PP",
            ticket.prefix == "CASOTON" ~ "CA",
            ticket.prefix == "AS" ~ "A",
            ticket.prefix == "A4" ~ "A",
            ticket.prefix == "A5" ~ "A",
            ticket.prefix == "SOP" ~ "SOPP",
            ticket.prefix == "SWPP" ~ "",
            ticket.prefix == "FA" ~ "",
            TRUE ~ ticket.prefix)) -> train

train %>%
  ggplot(., aes(ticket.prefix, fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by ticket prefix") +
  coord_flip()

train %>%
    rowwise %>%
    mutate(surname = gsub('( .*)|(*,*)', '', Name)) -> train

train %>%
  select(-Embarked) %>%
  rowwise %>%
  cbind(Embarked = train.csv$Embarked) %>%
  mutate(surname.E = paste0(Embarked,".",surname)) -> train

train %>%
  ungroup %>%
  group_by(surname.E) %>%
  summarise(n.fam = n(),
            dead.family = sum(1 - as.numeric(as.character(Survived))),
            all.died = ifelse(dead.family/n.fam == 1, 1, 0)) -> dead.family.df

train %>%
  left_join(., dead.family.df, by = 'surname.E') -> train

train %>%
  ggplot(., aes(factor(dead.family), fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by dead relatives by Sex") + facet_wrap(~ Sex)

train %>%
  ggplot(., aes(factor(dead.family), fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by dead relatives by Sex and Pclass") + facet_wrap(~ Sex*Pclass)

predict_race(train, surname.only = T) -> train

train %>%
  rowwise %>%
  mutate(race = case_when(
            pred.whi == max(pred.whi, pred.bla, pred.his, pred.asi, pred.oth) ~ "White",
            TRUE ~ "Non-White"
          )) ->train

train %>%
  ggplot(., aes(race, fill=Survived)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Survival by race and sex") + facet_wrap(~ Sex)

trctrl <- trainControl(
  method = "repeatedcv", 
    number = 10,
    repeats = 10)

tgctrl <- expand.grid(
    .mtry = 7,
    .splitrule = "gini",
    .min.node.size = 10)

fit.rf <- train(Survived ~ 
                    Sex + 
                    title.class + 
                    family.size.simple + 
                    Fare.grps + 
                    Pclass + 
                    Age.grps + 
                    Embarked + 
                    ticket.prefix + 
                    race,
                    #all.died, # We won't train on this, but we'll use it later to flip predictions
             data = train, 
             trControl = trctrl,
             metric = "Accuracy",
             importance = "impurity",
             tuneGrid = tgctrl,
             num.trees = 2000,
             method = "ranger")
fit.rf

test %>% 
  mutate(Fare = ifelse(PassengerId == 1044, 
                       median((test %>% filter(!is.na(Fare), 
                                               Pclass == 3, 
                                               PassengerId != 1044))$Fare), Fare)) -> test

test %>%
  rowwise %>%
  mutate(title = gsub('(.*, )|(\\..*)', '', Name),
         child = ifelse(Age < 15, 1, 0),
         title.class = case_when(
            title %in% c("Master") ~ "Boy",
            title %in% c("Miss") & child == 1 ~ "Girl",
            title %in% c("Mrs") ~ "Woman",
            title %in% c("Miss", "Mme", "Mlle", "Ms") ~ "Lady",
            title %in% c("Dr") & Sex == "female" ~ "Woman",
            title %in% c("Mr") ~ "Man",
            title %in% c("Capt", "Don", "Col", "Major", "Dr", "Rev", 
                         "Dona", "Jonkheer", "the Countess", "Lady", "Sir") ~ "Special",
            TRUE ~ "Other"),
         family.size = SibSp + Parch,
         family.size.simple = case_when(
            family.size == 0 ~ "0",
            between(family.size, 1, 3) ~ "1-3",
            family.size > 3 ~ ">3"
          ),
         Fare.grps = case_when(
            between(Fare, 0, 100) ~ "0-100",
            Fare > 100 ~ ">100",
          ),
         Age.grps = case_when(
            between(Age, 0, 12) ~ "young",
            between(Age, 12, 50) ~ "middle",
            Age > 50 ~ "old"
          ),
         ticket.prefix = ifelse(grepl(" ", Ticket), sub(" .*", "", Ticket), ""),
         ticket.prefix = toupper(gsub("(\\.)|(\\/)", "", ticket.prefix)),
           ticket.prefix = case_when(
            ticket.prefix == "STONO" ~ "SOTONO",
            ticket.prefix == "STONOQ" ~ "SOTONO",
            ticket.prefix == "STONO2" ~ "SOTONO",
            ticket.prefix == "SOTONOQ" ~ "SOTONO",
            ticket.prefix == "SOTONO2" ~ "SOTONO",
            ticket.prefix == "SCAH" ~ "SCA",
            ticket.prefix == "SCA4" ~ "SCA",
            ticket.prefix == "SCA3" ~ "SCA",
            ticket.prefix == "PPP" ~ "PP",
            ticket.prefix == "CASOTON" ~ "CA",
            ticket.prefix == "AS" ~ "A",
            ticket.prefix == "A4" ~ "A",
            ticket.prefix == "A5" ~ "A",
            ticket.prefix == "SOP" ~ "SOPP",
            ticket.prefix == "SWPP" ~ "",
            ticket.prefix == "FA" ~ "",
            ticket.prefix == "AQ3" ~ "",
            ticket.prefix == "AQ4" ~ "",
            ticket.prefix == "LP" ~ "",
            TRUE ~ ticket.prefix),
         surname = gsub('( .*)|(*,*)', '', Name),
         surname.E = paste0(Embarked, ".", surname)) -> test.final

# Ethnicity
predict_race(test.final, surname.only = T) -> test.final

test.final %>%
  rowwise %>%
  mutate(race = case_when(
            pred.whi == max(pred.whi, pred.bla, pred.his, pred.asi, pred.oth) ~ "White",
            TRUE ~ "Non-White")) -> test.final

# Dead family members
test.final %>%
  left_join(., dead.family.df, by = 'surname.E') -> test.final

Survived <- predict(fit.rf, test.final)

Survived <- as.numeric(as.character(Survived))

test.final %>%
    cbind(., Survived) -> test.pred

test.pred %>%
    mutate(Survived = case_when(
                          is.na(n.fam) ~ Survived,
                          all.died == 1 & n.fam > 1 & Survived == 1 ~ 0,
                          TRUE ~ Survived
                        )
           ) -> test.pred

pred.rf <- data.frame(
  cbind(
    PassengerId = as.integer(as.character(test.final$PassengerId)),
    Survived = as.integer(as.character(test.pred$Survived))
    )
  )        

write_csv(pred.rf, 'rf.output.csv')
