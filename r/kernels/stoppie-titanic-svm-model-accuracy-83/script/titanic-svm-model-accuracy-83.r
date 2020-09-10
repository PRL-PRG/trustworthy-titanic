
library(tidyverse) # metapackage with lots of helpful functions
library(mlr) # wrapper for machine learning algorithms
library(kernlab) # svm learner
# library(parallel) # parallel computations
# library(parallelMap) # parallel computations

# train
titanic_train <- read_csv(
    '../input/train.csv',
    col_types = cols(
        PassengerId = col_character(),
        Survived = col_character(),
        Pclass = col_character(),
        Name = col_character(),
        Sex = col_character(),
        Age = col_double(),
        SibSp = col_integer(),
        Parch = col_integer(),
        Ticket = col_character(),
        Fare = col_double(),
        Cabin = col_character(),
        Embarked = col_character()
    )
) %>%
mutate(label = 'train') %>%
select(-PassengerId)

# test
titanic_test <- read_csv(
    '../input/test.csv',
    col_types = cols(
        PassengerId = col_character(),
        Pclass = col_character(),
        Name = col_character(),
        Sex = col_character(),
        Age = col_double(),
        SibSp = col_integer(),
        Parch = col_integer(),
        Ticket = col_character(),
        Fare = col_double(),
        Cabin = col_character(),
        Embarked = col_character()
    )
) %>%
mutate(
    label = 'test',
    Survived = '0'
) %>%
select(-PassengerId)

# merge
titanic <- rbind(titanic_train, titanic_test)

# head
head(titanic, 10)

summarizeColumns(titanic)

titanic <- titanic %>%
mutate(
    Title = str_extract(Name, '\\w+(?=\\.)'),
    Nickname = str_extract(Name, '\\(.+\\)'),
    HasNickname = if_else(is.na(Nickname), '0', '1')
) %>%
select(-one_of('Nickname', 'Name'))

table(titanic$Title) %>% data.frame %>% setNames(c('Title', 'Frequency'))

titanic <- titanic %>%
mutate(
    Title = case_when(
        Title %in% c('Capt', 'Col', 'Major') ~ 'Officer',
        Title %in% c('Countess', 'Don', 'Dona', 'Jonkheer', 'Lady', 'Ms', 'Sir') ~ 'Honorific',
        Title == 'Mlle' ~ 'Miss',
        Title == 'Mme' ~ 'Mrs',
        TRUE ~ as.character(Title)
    )
)

ggplot(titanic, aes(x = Age)) +
geom_histogram(bins = 20) +
theme_light() +
ggtitle('Age distribution')

titanic %>%
group_by(Title) %>%
summarize(
    min_age = min(Age, na.rm = T),
    max_age = max(Age, na.rm = T),
    missing_count = sum(is.na(Age)) / n()
)

titanic <- titanic %>%
mutate(
    Family = SibSp + Parch
)

titanic <- titanic %>%
group_by(Ticket) %>%
mutate(TicketSize = n()) %>%
ungroup

titanic <- titanic %>%
mutate(
    TicketGroup = word(Ticket) %>% str_replace_all("[[:punct:]]", ""),
    TicketGroup = if_else(is.na(as.numeric(TicketGroup)), TicketGroup, 'NONE')
) %>%
select(-Ticket)

# check TicketGroup distribution
table(titanic$TicketGroup) %>% data.frame %>% setNames(c('TicketGroup', 'Frequency'))

titanic <- titanic %>%
group_by(TicketGroup) %>%
mutate(
    group_size = n()
) %>%
ungroup %>%
mutate(
    TicketGroup = if_else(
        group_size < 7L,
        'OTHER',
        TicketGroup
    )
) %>%
select(-group_size)

# distribution of TicketGroup
table(titanic$TicketGroup) %>% data.frame %>% setNames(c('TicketGroup', 'Frequency'))

ggplot(titanic, aes(x = Fare)) +
geom_histogram(bins = 15) +
theme_light() +
ggtitle('Fare distribution')

ggplot(titanic, aes(x = log(Fare + 1))) +
geom_histogram(bins = 15) +
theme_light() +
ggtitle('Log(Fare + 1) distribution')

titanic$Fare <- log(titanic$Fare + 1)

titanic <- titanic %>%
mutate(
    CabinGroup =
        str_split(Cabin, '\\s') %>% # split Cabins by any white space character
        lapply(substr, start = 1, stop = 1) %>% # get first character of each Cabin
        # get the mode of all cabins
        lapply(function(vec) {
            uniq_vec <- unique(vec) # unique values
            uniq_vec[which.max(tabulate(match(vec, uniq_vec)))] # mode
        }) %>%
        as.character
) %>%
select(-Cabin)

# check CabinGroup distribution
table(titanic$CabinGroup) %>% data.frame %>% setNames(c('CabinGroup', 'Frequency'))

titanic <- titanic %>%
mutate(
    CabinGroup = if_else(
        CabinGroup %in% c('G', 'T'),
        NA_character_,
        CabinGroup
    )
)

table(titanic$Embarked) %>% data.frame %>% setNames(c('Embarked', 'Frequency'))

head(titanic, 20)

titanic <- titanic %>%
mutate_if(sapply(titanic, is.character), factor)

summarizeColumns(titanic)

train_data <- titanic %>%
# filter(label.train == 1) %>%
# select(-one_of(c('label.train', 'label.test'))) %>%
filter(label == 'train') %>%
select(-label)

head(train_data, 20)

train_data <- train_data %>%
replace_na(
    replace = list(
        Age = mean(train_data$Age, na.rm = TRUE),
        Fare = mean(train_data$Fare, na.rm = TRUE),
        Embarked = 'S',
        CabinGroup = 'C'
    )
)

train_task <- makeClassifTask(
    data = train_data,
    target = 'Survived',
    positive = '1'
)

svm_learner <- makeLearner(
    'classif.ksvm',
    predict.type = 'prob',
    par.vals = list(
        scaled = TRUE,
        type = 'nu-svc'
    )
)

c50_learner <- makeLearner(
    'classif.C50',
    predict.type = 'prob',
    par.vals = list(
        winnow = TRUE
    )
)

logreg_learner <- makeLearner(
    'classif.glmnet',
    predict.type = 'prob',
    par.vals = list(
        standardize = TRUE
    )
)

# getLearnerParamSet('classif.ksvm')
# getLearnerParamSet('classif.C50')
# getLearnerParamSet('classif.glmnet')

# svm_resample <- makeResampleDesc('Bootstrap', iters = 20)

# svm_control <- makeTuneControlIrace(show.irace.output = TRUE, maxExperiments = 500)

# svm_paramset <- makeParamSet(
#   makeDiscreteParam('kernel', values = c('rbfdot', 'laplacedot')),
#   makeNumericParam('sigma', lower = 0, upper = 5),
#   makeNumericParam('nu', lower = 0.05, upper = 0.6)
# )

# svm_paramset <- makeParamSet(
#     makeIntegerParam('trials', lower = 1, upper = 100),
#     makeNumericParam('CF', lower = 0.8, upper = 0.99),
#     makeIntegerParam('minCases', lower = 3, upper = 100)
# )

# svm_paramset <- makeParamSet(
#     makeNumericParam('alpha', lower = 0, upper = 1),
#     makeNumericParam('s', lower = 0, upper = 0.03)
# )

# parallelStart(mode = 'socket', cpus = 4)

# svm_tune <- tuneParams(
#   learner = logreg_learner,
#   task = train_task,
#   resampling = svm_resample,
#   measures = acc,
#   par.set = svm_paramset,
#   control = svm_control
# )

titanic_learner <- svm_learner <- setHyperPars(
    svm_learner,
    par.vals = list(
        kernel = 'laplacedot',
        sigma = 0.0214442,
        nu = 0.4805411
    )
)

# c50_learner <- setHyperPars(
#     c50_learner,
#     par.vals = list(
#         trials = 41,
#         CF = 0.9257832,
#         minCases = 6
#     )
# )

# logreg_learner <- setHyperPars(
#     logreg_learner,
#     par.vals = list(
#         alpha = 0.3632723,
#         s = 0.02710750
#     )
# )

# wrapper
# titanic_learner <- makeStackedLearner(
#     base.learners = list(
#         svm_learner,
#         c50_learner,
#         logreg_learner
#     ),
# #     super.learner = makeLearner('classif.glmnet', predict.type = 'response'),
#     predict.type = 'prob',
#     method = 'hill.climb',
# #     resampling = makeResampleDesc('CV', iters = 20)
# )

titanic_model <- train(titanic_learner, train_task)

titanic_performance <- resample(
  learner = titanic_learner,
  task = train_task,
  resampling = makeResampleDesc('CV', iters = 20),
  measures = list(acc, tpr, fpr, kappa)
)

titanic_performance$aggr %>% data.frame %>% setNames('Measure')

test_data <- titanic %>%
# filter(label.test == 1) %>%
# select(-one_of(c('label.train', 'label.test'))) %>%
filter(label == 'test') %>%
select(-label)

test_data$Survived <- factor(sample(0:1, nrow(test_data), replace = TRUE))

head(test_data, 20)

test_data <- test_data %>%
replace_na(
    replace = list(
        Age = mean(train_data$Age),
        Fare = mean(train_data$Fare),
        Embarked = 'S',
        CabinGroup = 'C'
    )
)

test_task <- makeClassifTask(
    data = test_data,
    target = 'Survived',
    positive = '1'
)

test_predict <- predict(titanic_model, test_task)

prediction <- read_csv(
    '../input/gender_submission.csv',
    col_types = cols(
        PassengerId = col_character(),
        Survived = col_character()
    )
)

prediction$Survived <- test_predict$data$response
write_csv(prediction, 'titanic_svm_prediction.csv')
