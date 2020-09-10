
options(stringsAsFactors = FALSE)
library(reshape2)
library(dplyr)
library(ggplot2)
library(stringi)
library(rpart)
library(boot)
library(stringr)
library(randomForest)
library(e1071)

train.df <- read.csv("../input/train.csv")
submission.df <- read.csv("../input/test.csv")
combined.df <- bind_rows(train.df, submission.df)

# Sex feature
categories <- factor(combined.df$Sex)
combined.df <- cbind(combined.df, 
                     model.matrix(~categories + 0))
colnames(combined.df) <- sub("categories", "", colnames(combined.df))
rm(categories)

# Pclass feature
combined.df$Pclass <- ifelse(combined.df$Pclass == 1, "Pclass.Upper", 
                             ifelse(combined.df$Pclass == 2, "Pclass.Middle", 
                                    ifelse(combined.df$Pclass == 3, "Pclass.Lower", "Pclass.Other")))
categories <- factor(combined.df$Pclass)
combined.df <- cbind(combined.df, 
                     model.matrix(~categories + 0))
colnames(combined.df) <- sub("categories", "", colnames(combined.df))
rm(categories)

# Port of Embarkation feature
combined.df$Embarked <- ifelse(combined.df$Embarked == "C", "Cherbourg", 
                               ifelse(combined.df$Embarked == "Q", "Queenstown", 
                                      ifelse(combined.df$Embarked == "S", "Southampton", "Unknown")))
categories <- factor(combined.df$Embarked)
combined.df <- cbind(combined.df, 
                     model.matrix(~categories + 0))
colnames(combined.df) <- sub("categories", "Embarked.", colnames(combined.df))
rm(categories)

# Fare feature
combined.df$Fare[is.na(combined.df$Fare)] <- median(combined.df$Fare, na.rm = TRUE)
combined.df$Fare.Unskewed <- log(combined.df$Fare+1)

# Cabin feature
combined.df$Cabin <- ifelse(combined.df$Cabin == "", "Unknown", combined.df$Cabin)

Cabin.Deck <- as.data.frame(
  stri_extract_all_words(
    gsub("[0-9]","" , 
         combined.df$Cabin,
         ignore.case = TRUE), 
    simplify = T), 
  stringsAsFactors = F)
names(Cabin.Deck) <- c("Cabin.Deck", "Cabin.Deck2", "Cabin.Deck3", "Cabin.Deck4")
Cabin.Deck$Cabin <- combined.df$Cabin

Cabin.Deck <- 
  Cabin.Deck[c("Cabin", "Cabin.Deck")] %>%
  group_by(Cabin, Cabin.Deck) %>%
  summarise(n = n()) %>%
  select(Cabin, Cabin.Deck) %>%
  ungroup()

categories <- factor(Cabin.Deck$Cabin.Deck)
Cabin.Deck <- cbind(Cabin.Deck, 
                    model.matrix(~categories + 0))
colnames(Cabin.Deck) <- sub("categories", "Cabin.Deck.", colnames(Cabin.Deck))

combined.df <- 
  combined.df %>%
  left_join(Cabin.Deck, by = c("Cabin" = "Cabin"))
rm(categories, Cabin.Deck)

# Ticket feature
Ticket.Number <- stri_extract_last_words(combined.df$Ticket)
combined.df$Ticket2 <- gsub("[^0-9A-Za-z///' ]","" , combined.df$Ticket ,ignore.case = TRUE)
combined.df$Ticket2 <- sub("/", "-", combined.df$Ticket2)
boundary.count <- stri_count_boundaries(combined.df$Ticket2)
Ticket.Parsed <- as.data.frame(stri_extract_all_words(combined.df$Ticket2, simplify = T), stringsAsFactors = F)
Ticket.Prefix <- 
  cbind(combined.df$Ticket, boundary.count, Ticket.Parsed) %>%
  mutate(Ticket.Prefix = ifelse(boundary.count==1, "None", 
                                ifelse(boundary.count==2, V1, 
                                       ifelse(boundary.count==3, paste0(V1, "-", V2), paste0(V1, "-", V2,"-", V3))))) %>%
  select(Ticket.Prefix)
combined.df <-   cbind(combined.df, Ticket.Number, Ticket.Prefix)
combined.df$Ticket2 <- NULL
combined.df$Ticket.Prefix <- toupper(combined.df$Ticket.Prefix)
combined.df$Ticket.Prefix <- sub("SOTON-", "", combined.df$Ticket.Prefix)
combined.df$Ticket.Prefix <- sub("STON-", "", combined.df$Ticket.Prefix)
combined.df$Ticket.Prefix <- sub("-BASLE", "", combined.df$Ticket.Prefix)
combined.df$Ticket.Prefix <- sub("-SOTON", "", combined.df$Ticket.Prefix)
combined.df$Ticket.Prefix <- sub("-PARIS", "", combined.df$Ticket.Prefix)
combined.df$Ticket.Prefix <- sub("O-2", "O2", combined.df$Ticket.Prefix)
combined.df$Ticket.Prefix <- sub("WE-P", "WEP", combined.df$Ticket.Prefix)
cabin.id.df <- unique(combined.df[c("Ticket.Prefix", "Ticket.Number")]) 
cabin.id.df$CabinId <- seq.int(nrow(cabin.id.df))
combined.df <- 
  combined.df %>%
  left_join(cabin.id.df, by = c("Ticket.Number" = "Ticket.Number", "Ticket.Prefix" = "Ticket.Prefix"))
rm(Ticket.Number, boundary.count, Ticket.Parsed, Ticket.Prefix, cabin.id.df)

# Name feature; title
Name2 <- trimws(do.call(rbind, strsplit(combined.df$Name, ","))[,2])
Title <- trimws(do.call(rbind, stri_split_fixed(str = Name2, pattern = ".", n = 2))[,1])
rm(Name2)
Title <- sub("the ", "", Title)
Title <- sub("Mme", "Mrs", Title)
Title <- sub("Dona", "Mrs", Title)
Title <- sub("Don", "Mr", Title)
Title <- sub("Mlle", "Miss", Title)
Title <- sub("Countess", "Nobility", Title)
Title <- sub("Jonkheer", "Nobility", Title)
Title <- sub("Lady", "Nobility", Title)
Title <- sub("Sir", "Nobility", Title)
Title <- sub("Capt", "Military", Title)
Title <- sub("Col", "Military", Title)
Title <- sub("Major", "Military", Title)
combined.df <- cbind(combined.df, Title)

categories <- factor(combined.df$Title)
combined.df <- cbind(combined.df, 
                     model.matrix(~categories + 0))
colnames(combined.df) <- sub("categories", "Name.Title.", colnames(combined.df))
rm(Title, categories)

# Name feature; last name
combined.df$Name.Last <- trimws(do.call(rbind, strsplit(combined.df$Name, ","))[,1])

summary(combined.df$Age)

# Scale the numerical variables only for age predictors
age.df <- combined.df[c("PassengerId", "Age")]
age.names <- c("SibSp", "Parch", "Fare", "female", "male", "Pclass.Lower", "Pclass.Middle", 
               "Pclass.Upper", "Embarked.Cherbourg", "Embarked.Queenstown", "Embarked.Southampton", 
               "Embarked.Unknown", "Name.Title.Dr", "Name.Title.Master", "Name.Title.Military", 
               "Name.Title.Miss", "Name.Title.Mr", "Name.Title.Mrs", "Name.Title.Ms", 
               "Name.Title.Nobility", "Name.Title.Rev")

scaled.age.num <-
  combined.df[ , (names(combined.df) %in% age.names)] %>%
  scale()
scaled.age.df <- cbind(age.df, scaled.age.num)
rm(age.df, scaled.age.num)

set.seed(1489) 
# Split between train, test and predict
age.predict.df <- scaled.age.df[is.na(scaled.age.df$Age), ] 

age.estimated.df <- 
  scaled.age.df[!is.na(scaled.age.df$Age), ] %>%
  filter(Age-floor(Age)>0)

age.test.train.df <- 
  scaled.age.df[!is.na(scaled.age.df$Age), ] %>%
  filter(Age >= 1 & floor(Age)-Age==0)

sample <- sample.int(n = nrow(age.test.train.df), 
                     size = floor(.8*nrow(age.test.train.df)), 
                     replace = F)
age.train.df <- age.test.train.df[sample, ] 
age.test.df  <- age.test.train.df[-sample, ] 

# Random forest model
age_train_y <- age.train.df[, "Age"]
age_train_x <- age.train.df[ , (names(age.train.df) %in% age.names)] 
age_test_y <- age.test.df[, "Age"]
age_test_x <- age.test.df[ , (names(age.test.df) %in% age.names)] 

age_rf_model <- randomForest(age_train_x, 
                             y = age_train_y , 
                             ntree = 500, 
                             importance = TRUE)

sqrt(mean(as.numeric((predict(age_rf_model) - age_train_y)^2)))
sqrt(mean(((predict(age_rf_model, age_test_x) - age_test_y)^2)))

PassengerId <- age.predict.df[, "PassengerId"]
age_predict_x <- age.predict.df[ , (names(age.predict.df) %in% age.names)] 
age_predicted <- predict(age_rf_model, age_predict_x)
age.predicted.df <- data.frame(cbind(PassengerId, age_predicted))

combined.df <-
  combined.df %>%
  dplyr::left_join(age.predicted.df, by = c("PassengerId" = "PassengerId")) %>%
  mutate(Age.Estimated = ifelse(is.na(Age), 1, 
                                ifelse(Age >= 1 & floor(Age) != Age, 1, 0)),
         Age = ifelse(is.na(Age), age_predicted, Age))
combined.df$age_predicted <- NULL

summary(combined.df$Age)

# Add prefix average fare
ticket.prefix.fare.df <-
  combined.df %>%
  group_by(Ticket.Prefix, Ticket.Number) %>%
  summarise(Fare = sum(Fare)) %>%
  group_by(Ticket.Prefix) %>%
  summarise(Fare = sum(Fare),
            Count = n(),
            Prefix.Avg.Fare = sum(Fare)/n()) %>%
  select(Ticket.Prefix, Prefix.Avg.Fare)

# Cabin Level Metrics
cabin.proxy.df <-
  combined.df %>%
  group_by(CabinId) %>%
  summarise(Cabin.Passenger.Count = n(),
            Cabin.Fare = sum(Fare),
            Cabin.Distinct.Last.Names = n_distinct(Name.Last)) 
cabin.proxy.df$Cabin.Fare.Unskewed <- log(cabin.proxy.df$Cabin.Fare+1)

# Cabin Family Level Metrics
cabin.family.fare.df <-
  combined.df %>%
  group_by(CabinId, Name.Last) %>%
  summarise(Cabin.Family.Count = n(),
            Cabin.Family.Fare = sum(Fare),
            Cabin.Family.Age = mean(Age))
cabin.family.fare.df$Cabin.Family.Fare.Unskewed <- log(cabin.family.fare.df$Cabin.Family.Fare+1)

# Create function to add # of surviving family members for each passenger
combined.df$Survived.Proxy <- ifelse(is.na(combined.df$Survived), 0, combined.df$Survived)
surviving.family.fun <- function(x) {
  cabin <- as.list(unique(combined.df %>% filter(PassengerId == x) %>% select(CabinId)))
  Surviving.Family.df <-
    combined.df %>% 
    filter(CabinId == cabin & PassengerId != x) %>%
    group_by(CabinId, Name.Last) %>%
    summarise(Passenger.Known.Surviving.Family = sum(Survived.Proxy))
  Passenger.Surviving.Family.df <-
    combined.df[c("PassengerId", "CabinId", "Name.Last")] %>% 
    filter(PassengerId == x) %>%
    left_join(Surviving.Family.df, by = c("CabinId" = "CabinId", "Name.Last" = "Name.Last")) %>%
    select(PassengerId, Passenger.Known.Surviving.Family)
  return(Passenger.Surviving.Family.df)}

Passenger.Surviving.Family.df <- 
  lapply(as.list(combined.df$PassengerId), surviving.family.fun) %>% 
  bind_rows() %>%
  mutate(Passenger.Known.Surviving.Family = 
           ifelse(is.na(Passenger.Known.Surviving.Family), 0, Passenger.Known.Surviving.Family))

# Add features to combined data
combined.df <- 
  combined.df %>%
  left_join(ticket.prefix.fare.df, by = c("Ticket.Prefix" = "Ticket.Prefix")) %>%
  left_join(cabin.proxy.df, by = c("CabinId" = "CabinId")) %>%
  left_join(cabin.family.fare.df, by = c("CabinId" = "CabinId", "Name.Last" = "Name.Last")) %>%
  left_join(Passenger.Surviving.Family.df, by = c("PassengerId" = "PassengerId"))
combined.df$Survived.Proxy <- NULL
combined.df$Passenger.Known.Surviving.Family.Pct <- 
  ifelse(combined.df$Cabin.Family.Count-1==0, 0, 
         combined.df$Passenger.Known.Surviving.Family/(combined.df$Cabin.Family.Count-1))

# Classify Cabins
combined.df$Cabin.Group <- ifelse(combined.df$Cabin.Passenger.Count==1, "Single", 
                                  ifelse(combined.df$Cabin.Distinct.Last.Names==1, "Family", "Other"))
categories <- factor(combined.df$Cabin.Group)
combined.df <- cbind(combined.df, 
                     model.matrix(~categories + 0))
colnames(combined.df) <- sub("categories", "Cabin.", colnames(combined.df))
rm(categories)

# Traveling with mom?
mom.df <-
  combined.df[c("CabinId", "PassengerId", "Name", "Name.Last", "Title", "Sex", "Age", "SibSp", "Parch", "Cabin.Family.Count")] %>%
  filter(Sex == "female" & Cabin.Family.Count>1 & Parch>0 & Title =="Mrs") %>%
  mutate(Travel.with.Mom = 1)

passengers.wmom.df <-
  combined.df[c("CabinId", "Name.Last", "PassengerId")] %>%
  filter(!PassengerId %in% as.list(mom.df$PassengerId)) %>%
  left_join(mom.df[c("CabinId", "Name.Last", "Travel.with.Mom")], by = c("CabinId" = "CabinId", "Name.Last" = "Name.Last")) %>%
  select(PassengerId, Travel.with.Mom) %>%
  filter(!is.na(Travel.with.Mom))

combined.df <-
  combined.df %>%
  left_join(passengers.wmom.df, by = c("PassengerId" = "PassengerId"))
combined.df$Travel.with.Mom <- ifelse(is.na(combined.df$Travel.with.Mom), 0, combined.df$Travel.with.Mom)

# Scale the numerical variables only for age predictors used in model
passenger.id <- c("PassengerId", "Survived")
scaled.df <-
  combined.df[,sapply(combined.df, is.numeric)] %>%
  scale() %>%
  data.frame()
scaled.survival.df <- cbind(combined.df[ , (names(combined.df) %in% passenger.id)],
                            scaled.df[ , (!names(scaled.df) %in% passenger.id)])
                            
# Resplit df back into test/train and submission
test.train.df <- scaled.survival.df[!is.na(scaled.survival.df$Survived), ] #891
submission.df <- scaled.survival.df[is.na(scaled.survival.df$Survived), ] #418
submission.df$Survived <- NULL

# Split between train and test
set.seed(1489)
sample <- sample.int(n = nrow(test.train.df), 
                     size = floor(.8*nrow(test.train.df)), 
                     replace = F)
survival.train.df <- test.train.df[sample, ]
survival.test.df  <- test.train.df[-sample, ] 

features.selected <- c("Pclass.Lower", "Pclass.Middle", "Pclass.Upper", "Embarked.Cherbourg", 
                       "Embarked.Queenstown", "Embarked.Southampton", "Embarked.Unknown", 
                       "Name.Title.Dr", "Name.Title.Master", "Name.Title.Military", 
                       "Name.Title.Miss", "Name.Title.Mr", "Name.Title.Mrs", 
                       "Name.Title.Ms", "Name.Title.Nobility", "Name.Title.Rev", "Age", 
                       "Fare.Unskewed", "female", "male", "Age.Estimated", "SibSp", "Parch", 
                       "Cabin.Passenger.Count", "Cabin.Fare.Unskewed", 
                       "Cabin.Distinct.Last.Names", "Cabin.Family", "Cabin.Other", "Cabin.Single", 
                       "Prefix.Avg.Fare", "Cabin.Deck.A", "Cabin.Deck.B", "Cabin.Deck.C", 
                       "Cabin.Deck.D", "Cabin.Deck.E", "Cabin.Deck.F", "Cabin.Deck.Unknown", 
                       "Passenger.Known.Surviving.Family", "Passenger.Known.Surviving.Family.Pct")

# Random forest model
survival_train_y <- survival.train.df[, "Survived"]
survival_train_x <- survival.train.df[ , (names(survival.train.df) %in% features.selected)] 
survival_test_y <- survival.test.df[, "Survived"]
survival_test_x <- survival.test.df[ , (names(survival.test.df) %in% features.selected)] 

survival_rf_model <- randomForest(survival_train_x, 
                                  y = survival_train_y , 
                                  ntree = 500, 
                                  importance = TRUE)

# Threshold function
threshold.fun <- function(x) {
  oob_results <-
    cbind(Outcome=survival_train_y, 
          Prediction=ifelse(predict(survival_rf_model) > x, 1, 0)) %>%
    data.frame() %>%
    mutate(Set = "Train",
           Threshold = x,
           Pred_Desc = ifelse(Outcome==Prediction, "Correct Prediction",
                              ifelse(Outcome==0 & Prediction ==1, "False Pos (Drwn)", "False Neg (Surv)"))) %>%
    group_by(Set, Threshold, Pred_Desc) %>%
    summarise(Passenger.Count = n())
  
  test_results <-
    cbind(Outcome=survival_test_y,
          Prediction=ifelse(predict(survival_rf_model, survival_test_x) > x, 1, 0)) %>%
    data.frame() %>%
    mutate(Set = "Test",
           Threshold = x,
           Pred_Desc = 
             ifelse(Outcome==Prediction, "Correct Prediction",
                    ifelse(Outcome==0 & Prediction ==1, "False Pos (Drwn)", "False Neg (Surv)"))) %>%
    group_by(Set, Threshold, Pred_Desc) %>%
    summarise(Passenger.Count = n())
  
  results.df <- rbind(oob_results, test_results)
  return(results.df)}

results.df <- 
  lapply(seq(from = 0, to = 1, by = 0.01), 
         threshold.fun) %>% 
  bind_rows() 

ggplot(results.df %>% filter(Set == "Train"), aes(x = Threshold, y = Passenger.Count)) +
  geom_line(aes(color = Pred_Desc), size = 1)

ggplot(results.df %>% filter(Set == "Test"), aes(x = Threshold, y = Passenger.Count)) +
  geom_line(aes(color = Pred_Desc), size = 1)

head(results.df %>% filter(Set == "Train" & Pred_Desc == "Correct Prediction") %>%
  arrange(desc(Passenger.Count)))

survival.threshold <- 0.62

# Predict on submission data
survival_submission_x <- submission.df[ , (names(submission.df) %in% features.selected)] 
survival_submision_pred <- predict(survival_rf_model, survival_submission_x)

PassengerId <- submission.df[, "PassengerId"]
Survived <- ifelse(survival_submision_pred > survival.threshold, 1, 0)
kaggle.submission <- cbind(PassengerId, Survived)
write.csv(kaggle.submission, 
          file = paste0("submission_", survival.threshold*100, "threshold.csv"), 
          row.names = F)
