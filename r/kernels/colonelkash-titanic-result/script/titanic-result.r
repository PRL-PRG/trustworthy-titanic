
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
Train <- read_csv("../input/train.csv") #Read the train set
Test <- read_csv("../input/test.csv")
str(Train) # see the stucturure


#remove non availabe age rows
        library(rpart)
#Survival_data <- rpart(Survived ~ Fare, )
        Survived_data <- rpart( Survived ~ Pclass + Sex + Age + Fare + Embarked , data = Train , method = "class")
        plot(Survived_data, uniform = TRUE)
        text(Survived_data,cex=0.8)

        Prediction <- predict( Survived_data , Test, type = "class")

        Survival_prediction <- data.frame(PassengerId = Test$PassengerId, Survived = Prediction)
        Survival_prediction
        write.csv(Survival_prediction, file = "Survival_prediction" , row.names = FALSE)


