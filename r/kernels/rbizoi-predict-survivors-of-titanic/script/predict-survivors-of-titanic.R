## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(tidyverse))

rm(list=ls())
seed <- 123456789
set.seed(seed)
apprentissage <- read_csv('../input/train.csv')

pourletest    <- read_csv('../input/test.csv')
titanic  <- bind_rows(apprentissage, pourletest)
rm("apprentissage","pourletest")
summary(titanic)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$Name %>%
  head


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(gsubfn))

titanic %>% 
  filter(!is.na(Survived)) %>%
  transmute(Titre = as.factor(sapply(gsub('(.*, )|(\\..*)', '', Name),
                        switch,Mlle="Miss",Ms="Miss",Mrs="Mrs",
                        Mr="Mr",Miss="Miss","TitreRare")),
            Survivant = ordered(as.character(lapply(as.character(Survived),switch,`0`="Non",`1`="Oui")) 
                                     ,levels=c("Oui","Non"))      ) %>%
  ggplot() + 
    geom_histogram(mapping = aes(Titre, fill=Survivant), color='black' , alpha=.6 ,stat="count",position = "dodge")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(gsubfn))

titanic %>% 
  filter(!is.na(Survived)) %>%
  transmute( TailleFamille = as.factor(sapply(as.character(SibSp + Parch + 1),switch,
                                          `1`="Celibataire",`2`="Petite",`3`="Petite",`4`="Petite","Grande")),
             Survivant = ordered(as.character(lapply(as.character(Survived),switch,`0`="Non",`1`="Oui")) 
                                     ,levels=c("Oui","Non"))      ) %>%
  ggplot() + 
    geom_histogram(mapping = aes(TailleFamille, fill=Survivant), color='black' , alpha=.6 ,stat="count",position = "dodge")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(gsubfn))

titanic %>% 
  mutate(Titre = as.factor(sapply(gsub('(.*, )|(\\..*)', '', Name),
                        switch,Mlle="Miss",Ms="Miss",Mrs="Mrs",
                        Mr="Mr",Miss="Miss","TitreRare")),
         Sex = as.factor(sapply(Sex,switch,male="Homme",female="Femme")),
         Homme = as.factor(sapply(Sex,switch,male="Oui",female="Non")),
         Femme  = as.character(sapply(Sex,switch,male="Non",female="SansEnfant")),
         Survivant = ordered(as.character(lapply(as.character(Survived),switch,`0`="Non",`1`="Oui")) 
                                     ,levels=c("Oui","Non"))) %>%
  as.data.frame     -> temp


temp$Femme[temp$Femme == "SansEnfant" & temp$Parch  > 0 & temp$Titre != "Miss"] <- "Mere"
temp$Femme <- as.factor(temp$Femme)


ggplot(temp) + 
    geom_histogram(mapping = aes(Homme, fill=Survivant), color='black' , alpha=.6 ,stat="count",position = "dodge")
ggplot(temp) + 
    geom_histogram(mapping = aes(Femme, fill=Survivant), color='black' , alpha=.6 ,stat="count",position = "dodge")
rm('temp')


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(gsubfn))

titanic %>% 
  mutate(Titre = as.factor(sapply(gsub('(.*, )|(\\..*)', '', Name),
                        switch,Mlle="Miss",Ms="Miss",Mrs="Mrs",
                        Mr="Mr",Miss="Miss","TitreRare")),
         TailleFamille = as.factor(sapply(as.character(SibSp + Parch + 1),switch,
                                          `1`="Celibataire",`2`="Petite",`3`="Petite",`4`="Petite","Grande")),
         Sex = as.factor(sapply(Sex,switch,male="Homme",female="Femme")),
         Homme = as.factor(sapply(Sex,switch,male="Oui",female="Non")),
         Femme  = as.character(sapply(Sex,switch,male="Non",female="SansEnfant")),
         Survivant = Survived,
         Classe = Pclass,
         Ticket = Fare,
         Port = Embarked) %>%
  as.data.frame     -> donnees

donnees$Femme[donnees$Femme == "SansEnfant" & donnees$Parch  > 0 & donnees$Titre != "Miss"] <- "Mère"
donnees$Femme <- as.factor(donnees$Femme)
donnees$Port[is.na(donnees$Port)] <- "_"
donnees$Port <- as.factor(donnees$Port)

row.names(donnees) <- donnees$PassengerId

donnees %>% 
  select (Survivant, Titre, Homme, Femme, Age, TailleFamille, Classe, Port, Ticket) -> donnees

rm('titanic','temp')
summary(donnees)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

transformQualiValeur <- function(donnees,colonne){
    valColonne <- as.factor(donnees[,colonne])
    noms <- names(donnees)
    colonnes <- paste(colonne,levels(as.factor(as.integer(valColonne))),sep = "_")
    valeurs <- levels(valColonne)
    for ( i in 1:length(valeurs)){
      donnees[,colonnes[i]] <- as.factor(as.integer(valColonne == valeurs[i]))
    }
    donnees %>% 
      select ( -colonne) ->> donnees
}


transformQualiNom <- function(donnees,colonne){
    valColonne <- as.factor(donnees[,colonne])
    noms <- names(donnees)
    colonnes <- paste(colonne,levels(valColonne),sep = "_")
    valeurs <- levels(valColonne)
    for ( i in 1:length(valeurs)){
      donnees[,colonnes[i]] <- as.factor(as.integer(valColonne == valeurs[i]))
    }
    donnees %>% 
      select ( -colonne) ->> donnees
}

transformQualiNom(donnees,"TailleFamille")
transformQualiNom(donnees,"Titre")
transformQualiNom(donnees,"Homme")
transformQualiNom(donnees,"Femme")

donnees %>%
   select (-Homme_Non,-Femme_Non) -> donnees

str(donnees)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library('ggplot2'))
suppressMessages(library('scales'))

donnees %>% 
  filter ( Ticket !=0 & !is.na(Ticket) & (Port == 'S' | Port == 'C')) %>% 
  ggplot(aes(x = Port, y = Ticket, fill = factor(Classe))) +
      geom_boxplot() +
      geom_hline(aes(yintercept=80), 
                 colour='red', linetype='dashed', lwd=1) +
      scale_y_continuous(labels=dollar_format()) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
donnees %>% 
  filter ( Ticket !=0 & !is.na(Ticket) & Classe == '3' & Port == 'S') %>% 
  ggplot(aes(x = Ticket)) +
      geom_density(fill = "yellow", alpha=0.1) + 
      geom_vline(aes(xintercept=median(Ticket, na.rm=T)),
                 colour='red', linetype='dashed', lwd=1) +
      scale_x_continuous(labels=dollar_format()) 


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
donnees %>% 
  filter ( Ticket !=0 & !is.na(Ticket) & Classe == '3' & Port == 'S') %>% 
  select( Ticket) -> valeur

donnees$Ticket[is.na(donnees$Ticket)] <- median(valeur$Ticket)
rm('valeur')


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(timereg)
donnees$Ticket <- qcut(donnees$Ticket)


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(donnees$Ticket)


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transformQualiValeur(donnees,"Ticket")
transformQualiNom(donnees,"Port")
transformQualiNom(donnees,"Classe")
summary(donnees)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(mice))
suppressMessages(library(randomForest))

donnees -> donneesCalc

set.seed(seed)

mice_mod <- mice(donneesCalc, method='rf')

donnees$AgeMice <- complete(mice_mod)$Age
rm("mice_mod")


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(donnees) + 
  geom_histogram(mapping = aes(Age)    , color='red' , alpha=.4)+
  geom_histogram(mapping = aes(AgeMice), color='blue', alpha=.2)


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(donnees$AgeMice)


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(timereg)

donnees %>% 
  mutate(Age = round(donnees$AgeMice))%>%
  select(-AgeMice) -> donnees

rm(donneesCalc)
summary(donnees$Age)


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pourletest    <- donnees[is.na(donnees$Survivant),-1]

donnees <- donnees[!is.na(donnees$Survivant),]
donnees$Survivant <- ordered(as.character(lapply(as.character(donnees$Survivant),switch,`0`="Non",`1`="Oui")) 
                                     ,levels=c("Oui","Non"))
names(donnees)[1] <- "Cible"


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(caret))
suppressMessages(library(tidyverse))
suppressMessages(library(plotROC))

models <- NULL 
prediction <- NULL
prediction.Probabilite<-NULL

donneesModelROCALL <- NULL
MatriceConfusion <- data.frame()

fun.auc.ggplot <- function (ModelPred, ModelProb, ModelCible, Title = '', echantillon = '', df = donneesModelROCALL){

  donneesModel <- data.frame(Cible = as.integer(ModelCible), 
                             Cible.Nom = as.character(ModelCible),
                             ModelProb = ModelProb)
  
  matconf <- caret::confusionMatrix(ModelPred, ModelCible,mode="everything")  
  
  basicplot <- ggplot(donneesModel, aes(d = Cible, m = 1-ModelProb)) + 
                   geom_roc(n.cuts = 20, labelsize = 3, labelround = 4)

  basicplot<- basicplot  +
    style_roc(xlab='False Positives Rate(1 - Specificity)',
              ylab='True Positives Rate(Sensitivity)',
              theme = theme_grey)+
    ggtitle( paste('AUC',round(calc_auc(basicplot)$AUC,8),"% Accuracy",
                   round(matconf$overall[1],4))) + 
    coord_fixed(ratio = 1)
  
  
  donneesModel$Label <- rep(paste(Title," : ",round(calc_auc(basicplot)$AUC,4),"%"),length(ModelCible))
  donneesModel$Echantillon <- echantillon

  MatriceConfusion <<- rbind( MatriceConfusion,
                              data.frame(Nom=Title,
                                         AUC        =calc_auc(basicplot)$AUC,
                                         Accuracy   =matconf$overall[1],
                                         Kappa      =matconf$overall[2], 
                                         VP         =matconf$table[1,1],
                                         FP         =matconf$table[1,2],
                                         VN         =matconf$table[2,2],
                                         FN         =matconf$table[2,1],
                                         Sensitivity=matconf$byClass[1],  
                                         Specificity=matconf$byClass[2],
                                         Precision  =matconf$byClass[5],
                                         FScore1    =matconf$byClass[7],
                                         Prevalence =matconf$byClass[8],
                                         PPV        =matconf$byClass[3], 
                                         NPV        =matconf$byClass[4],
                                         row.names =NULL))

  donneesModelROCALL <<- rbind(df,donneesModel)

  basicplot
}


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(seed)
validationIndex <- createDataPartition(donnees$Cible, p=0.885, list=FALSE)
echantillonApprentissage <- donnees[validationIndex,]
echantillonValidation <- donnees[-validationIndex,]


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() + 
  geom_histogram(data = echantillonApprentissage, mapping = aes(Cible) , color='red' , alpha=.4, stat="count")+
  geom_histogram(data = echantillonValidation   , mapping = aes(Cible) , color='blue', alpha=.2, stat="count")


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(e1071))
models$naiveBayes <- naiveBayes(Cible~., data=echantillonApprentissage, laplace = 3)
prediction$naiveBayes <- predict(models$naiveBayes, echantillonValidation,type="class")
prediction.Probabilite$naiveBayes <- predict(models$naiveBayes, echantillonValidation,type="raw")


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
caret::confusionMatrix(prediction$naiveBayes, echantillonValidation$Cible,mode="everything")


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fun.auc.ggplot(prediction$naiveBayes,prediction.Probabilite$naiveBayes[,1], echantillonValidation$Cible,'Naive Bayes')


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(C50))
models$C5.0 <- C5.0(Cible~., data=echantillonApprentissage, trials=10)


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prediction$C5.0 <- predict(models$C5.0, echantillonValidation)
prediction.Probabilite$C5.0 <- predict(models$C5.0, echantillonValidation,type='prob')


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
caret::confusionMatrix(prediction$C5.0, echantillonValidation$Cible,mode="everything")


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fun.auc.ggplot(prediction$C5.0,prediction.Probabilite$C5.0[,1], echantillonValidation$Cible,'C5.0')


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(randomForest))
models$randomForest <- randomForest(Cible~., data=echantillonApprentissage, ntree=3000,mtry = 22,maxdepth = 9)


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(models$randomForest, ylim=c(0,0.36))
legend('topright', colnames(models$randomForest$err.rate), col=1:3, fill=1:3)


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
importance    <- importance(models$randomForest) 

data.frame(Variables = row.names(importance),
           Importance = round(importance[ ,'MeanDecreaseGini'],2)) %>% 
                      mutate(Rank = paste0('#',dense_rank(desc(Importance)))) %>%
  ggplot(aes(x = reorder(Variables, Importance), 
                             y = Importance, fill = Importance)) +
      geom_bar(stat='identity') + 
      geom_text(aes(x = Variables, y = 0.5, label = Rank),
                hjust=0, vjust=0.55, size = 4, colour = 'red') +
      labs(x = 'Variables') +
      coord_flip() 


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prediction$randomForest <- predict(models$randomForest, echantillonValidation,type="class")
prediction.Probabilite$randomForest <- predict(models$randomForest, echantillonValidation,type="prob")


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
caret::confusionMatrix(prediction$randomForest, echantillonValidation$Cible,mode="everything")


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fun.auc.ggplot(prediction$randomForest,prediction.Probabilite$randomForest[,1], echantillonValidation$Cible,'Random Forest')


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(donneesModelROCALL, aes(d = Cible, m = 1-ModelProb, color = Label)) + 
                   geom_roc(n.cuts = 0) +
      style_roc(xlab='False Positives Rate(1 - Specificity)',
              ylab='True Positives Rate(Sensitivity)',
              theme = theme_grey)+
      ggtitle( 'Receiver Operating Caracteristic ') + 
      coord_fixed(ratio = 1) 


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggrepel)
ggplot(data = MatriceConfusion, mapping = aes(x = VP, y = FP)) + 
  geom_point(aes(color = Nom),size = 10, alpha=.4)+
  geom_label_repel(aes(label = paste(Nom,"-",round(Accuracy*100,2),"%")),
                  box.padding   = 0.4, 
                  point.padding = 0.6)+
  ggtitle( paste('VP/FP sample ',
                  mean(MatriceConfusion$VP+MatriceConfusion$FP+MatriceConfusion$VN+MatriceConfusion$FN)))


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggrepel)
ggplot(data = MatriceConfusion, mapping = aes(x = VN, y = FN)) + 
  geom_point(aes(color = Nom),size = 10, alpha=.4)+
  geom_label_repel(aes(label = paste(Nom,"-",round(Accuracy*100,2),"%")),
                  box.padding   = 0.4, 
                  point.padding = 0.6)+
  ggtitle( paste('VN/FN sample ',
                  mean(MatriceConfusion$VP+MatriceConfusion$FP+MatriceConfusion$VN+MatriceConfusion$FN)))


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggrepel)
ggplot(data = MatriceConfusion, mapping = aes(x = PPV, y = NPV)) + 
  geom_point(aes(color = Nom),size = 10, alpha=.4)+
  geom_label_repel(aes(label = paste(Nom,"-",round(Accuracy*100,2),"%")),
                  box.padding   = 0.4, 
                  point.padding = 0.6)+
  ggtitle( paste('PPV/NPV sample ',
                  mean(MatriceConfusion$VP+MatriceConfusion$FP+MatriceConfusion$VN+MatriceConfusion$FN)))


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
models <- NULL 
prediction <- NULL
prediction.Probabilite<-NULL

donneesModelROCALL <- NULL
MatriceConfusion <- data.frame()
set.seed(seed)

partitions <- 5
validationIndex <- createDataPartition(donnees$Cible, p=0.885, list=TRUE, times = partitions)


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(randomForest))


for (i in 1:partitions) {
  echantillon <- i
  echantillonApprentissage            <- donnees[validationIndex[[i]],]
  echantillonValidation               <- donnees[-validationIndex[[i]],]
  
  
  
  models$randomForest <- randomForest(Cible~., data=echantillonApprentissage, ntree=500,mtry = 22,maxdepth = 9)
  prediction$randomForest <- predict(models$randomForest, echantillonValidation,type="class")
  prediction.Probabilite$randomForest <- predict(models$randomForest, echantillonValidation,type="prob")


  fun.auc.ggplot(prediction$randomForest,
                 prediction.Probabilite$randomForest[,1], 
                 echantillonValidation$Cible,
                 paste('Random Forest-',echantillon),
                 echantillon)
}


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(donneesModelROCALL, aes(d = Cible, m = 1 - ModelProb, color = Label)) + 
                   geom_roc(n.cuts = 0) +
      style_roc(xlab='False Positives Rate(1 - Specificity)',
              ylab='True Positives Rate(Sensitivity)',
              theme = theme_grey)+
      ggtitle( 'Receiver Operating Caracteristic ') + 
      coord_fixed(ratio = 1) 


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for ( i in 1:partitions ){
  donneesModelROCALL[donneesModelROCALL$Echantillon == i,] %>% 
      ggplot(aes(d = Cible, m = 1-ModelProb)) + 
      geom_roc(n.cuts = 20, labelsize = 3, labelround = 4) +
      style_roc(xlab='False Positives Rate(1 - Specificity)',
              ylab='True Positives Rate(Sensitivity)',
              theme = theme_grey)+
      ggtitle( paste("AUC : ",median(MatriceConfusion$AUC),"±",sd(MatriceConfusion$AUC))) + 
      coord_fixed(ratio = 1) -> g
  print(g)
}


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(randomForest))
models$randomForest <- randomForest(Cible~., data=donnees, ntree=3000,mtry = 22,maxdepth = 9)
prediction$randomForest <- predict(models$randomForest, pourletest,type="class")


## ----fig.align = 'default', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gender_submission <- data.frame(list(PassengerId=row.names(pourletest),
                                     Survived=as.integer(sapply(prediction$randomForest,switch,Oui=1,Non=0))))
gender_submission %>% 
  ggplot() + 
  geom_histogram(mapping = aes(Survived) , color='red' , alpha=.4, stat="count")

