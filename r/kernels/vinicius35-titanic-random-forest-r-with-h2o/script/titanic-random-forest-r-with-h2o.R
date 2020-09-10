# Pacotes
library(h2o);library(dplyr);library(caret);library(GGally);
library(ggplot2);library(rpart);library(readr);library(stringr);

# Entrada de dados
treino = read.csv("../input/train.csv",header=T)
teste = read.csv("../input/test.csv",header=T)
teste = cbind(teste,Survived = NA)
full = rbind(treino,teste)
dim(full)

# Analise exploratoria de dados com foco na variável alvo
# Sex,Pclass and Survived
ggplot(data = treino, aes(x= Pclass,fill = as.factor(Survived)))+ 
       geom_bar() + facet_grid(~Sex) +  ggtitle("Sex,Pclass and Survived - raw data")

# Sex,Embarked and Survived
ggplot(data = treino, aes(x= Embarked,fill = as.factor(Survived )))+ 
       geom_bar()+facet_grid(~Sex) + ggtitle("Sex,Embarked and Survived - raw data")

# Fare and Survived
ggplot(data = treino , aes(x = as.factor(Survived), y = Fare)) +
       geom_boxplot()+scale_y_log10() + ggtitle("Fare and Survived - raw data")

# Age and Survived
ggplot(data = treino , aes(x = as.factor(Survived), y = Age)) +
       geom_boxplot()+scale_y_log10() + ggtitle("Age and Survived - raw data")
   
# Features Engineering
# Title
full$Name = as.character(full$Name)
full$Title = sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
full$Title = sub(' ', '', full$Title)
full$Title[full$Title %in% c('Mme', 'Mlle')] = 'Mlle'
full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] = 'Sir'
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] = 'Lady'
full$Title = as.factor(full$Title)

# Family Size
full$FamilySize = full$SibSp + full$Parch + 1
full$FamilySize = as.factor(full$FamilySize)

numParents = integer( nrow( full ) );
numChildren = integer( nrow( full ) );

# Parents and Children
MAX_CHILD_AGE = 14;

# Find all Parch > 0
posParch = full$Parch > 0;
idxParch = which( posParch );
counter = 0;
numCases = length( idxParch );
for (thisRow in idxParch){
  sibsp = full[thisRow,"Sibsp"];
  # if sibsp > 1, then the passenger is travelling with siblings (and therefore, likely parents)
  counter = counter + 1;
  numParch = full[thisRow,"Parch"];
  thisSurname = full[thisRow,"Surname"];
  thisTicket = full[thisRow,"Ticket"];
  thisTitle = as.character( full[thisRow,"Title"] );
  thisAge = full[thisRow,"Age"];
  if ( (thisTitle == "Master.") || (thisTitle == "Miss.") ){
    # if this passenger is a "Master." or Miss. and numParch <= 2, he/she must be someone's child
    numParents[thisRow] = numParch;
    #next;
  }
  if (!is.na( thisAge ) && thisAge <= MAX_CHILD_AGE ){
    # if this passenger is young, declare they cannot be parents, must be a child
    numParents[thisRow] = numParch;
    #next;
  }
  # get passenger rows on same ticket
  sameTickets = full$Ticket == thisTicket;
  sameSurnames = full$Surname == thisSurname;
  sameSurnameSameTicket = sameTickets & sameSurnames & posParch;
  ticketTitles = as.character( full[ sameSurnameSameTicket, "Title"] );
  ticketParches = full[ sameSurnameSameTicket, "Parch"];
  # now, look for passengers with the same surname on the ticket and check their titles and ages
  numSame = length( sameSurnameSameTicket );
  ages = sort( full[sameSurnameSameTicket,"Age"] );
  thisAgePos = which( thisAge == ages )[1];  # find index of thisAge in sorted ages
  gaps = diff( ages );
  numGenerationalGaps = length( which( gaps > MAX_CHILD_AGE) );
  if ( numGenerationalGaps == 0 ){
    if ( !is.na( thisAge )){
      if ( ( thisAge >= 40 ) || (thisTitle == "Mrs." ) ){
        numChildren[thisRow] = numParch;
      }
      else if (numParch > 2 ){
        numChildren[thisRow] = numParch;
      }
      else{
        numParents[thisRow] = numParch;
      }
    }
    else{
      # no age information. If travelling with "kids", and title isn't a kid, then parent
      if ( ! ( thisTitle %in% c("Master.","Miss.") ) ){
        if( any( c("Master.","Miss.") %in% ticketTitles ) ){
          # now, if there are two Mr. in this group, we need to choose the real father
          # If there are three or more children, then Parch will be greater than 2 and
          # will indicate this is the father.  Else, it will be a child
          if( thisTitle == "Mr."){
            maxParches = max( ticketParches );
            if ( ( maxParches > 2 ) && ( maxParches == numParch ) ){
              numChildren[thisRow] = numParch;
            }
            else{
              numParents[thisRow] = numParch;
            }
          }
          else{
            numChildren[thisRow] = numParch;
          }
        }
        else{
          # all we have is a non Mr. or Miss. title.  Make them a child
            numChildren[thisRow] = numParch;
        }
      }
      else{
        numParents[thisRow] = numParch;
      }
    }
  }
  else{
    if ( !is.na(thisAge) ){  # use age in comparison to generation gap to classify kids/parents
      maxGapPos = which.max( gaps ) + 0.5;  # the 0.5 puts it in the middle of the kids/parents
      if ( thisAgePos < maxGapPos ){
        numParents[thisRow] = numParch;
      }
      else{
        numChildren[thisRow] = numParch;
      }
    }
    else{
      # no age info, have to go with titles
    }
  }
  totalParch = numParents[thisRow] + numChildren[thisRow];
  if ( totalParch != numParch ){
    stop( "Number of Parents/Children assigned (", totalParch, ") does not equal the Parch variable (", numParch, ") for passenger ", data_combined[thisRow,"PassengerId"], "\n");
  }
  full$NumParents = numParents;
  full$NumChildren = numChildren;
}

full$NumParents = as.factor(full$NumParents)
full$NumChildren = as.factor(full$NumChildren)

# Embarked
full$Embarked[c(62,830)] = "S"
full$Embarked = as.factor(full$Embarked)

# Fare
full$Fare[1039] = median(full$Fare, na.rm=TRUE)
full$Fare = log(full$Fare)

# Age
age_miss = rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                 data=full[!is.na(full$Age),], method="anova")
               
full$Age[is.na(full$Age)] = predict(age_miss, full[is.na(full$Age),])
#full$Age = log(full$Age)

# Discretize age
full$Age_Group[full$Age <= 10] <- 'crianca'
full$Age_Group[full$Age <= 20 & full$Age > 10] <- 'adolescente'
full$Age_Group[full$Age <= 30 & full$Age > 20] <- 'jovem'
full$Age_Group[full$Age <= 40 & full$Age > 30] <- 'adulto'
full$Age_Group[full$Age <= 50 & full$Age > 40] <- 'senior'
full$Age_Group[full$Age > 50] <- 'idoso'
full$Age_Group = as.factor(full$Age_Group)

# Pclass
full$Pclass = as.factor(full$Pclass)

# Cabin Level e Cabin Room
CabinLevels = c("", "A","B","C","D","E", "F", "G", "T")
CabinLevel = gsub('^([A-G]).*$', '\\1', full$Cabin)
full$CabinLevel = factor(CabinLevel, levels=CabinLevels)
CabinRoom = gsub('^[^0-9]+([1-9]+).*$', '\\1', full$Cabin)
CabinRoom = ifelse(CabinRoom=="", "0", CabinRoom)
suppressWarnings(CabinRoom <- as.integer(CabinRoom))
full$CabinRoom = ifelse(is.na(CabinRoom), 0, CabinRoom)
full$CabinRoom = as.factor(full$CabinRoom)

# Ticket
extractTicket <- function(ticket){
    pattern <- c('\\/', '\\.', '\\s', '[[:digit:]]')
    for (p in pattern){
        # replace all chracter matches the pattern p with ""
        ticket <- gsub(p, "", ticket)
    }
    ticket <- substr(toupper(ticket), 1,1)
    ticket[ticket==""] <- 'N'
    ticket <- as.factor(ticket)
}
full$Ticket <- extractTicket(full$Ticket)

# Retirada de dados não modelaveis
full = full %>% select(-Name,-SibSp,-Parch,-Cabin,-CabinRoom,-CabinLevel,-FamilySize,-Embarked,-Age)
str(full)

# Variaveis Dummy
dummy = dummyVars("~ .", data = full)
full = data.frame(predict(dummy, full))

# Target
full$Survived = as.factor(full$Survived)
dim(full)
str(full)

# Analise exploratoria de dados com os novos recursos

# Seleções de variáveis
# Retirada de dados com variação próxima de zero
#full = full[, -nearZeroVar(full[3:137])]
#dim(full)
#str(full)

# Matriz de correlação dos novos recursos 
#ggcorr(full[,-1],label = F,geom="circle")
#dim(full)
#str(full)

# Divisão do dataset
treino = full[1:891,]
teste = full[892:1309,]

# Inicialização do H2O
h2o.init()

# Entrada de dados no H2O
treino.hex = as.h2o(treino, destination_frame="treino.hex")
teste.hex = as.h2o(teste, destination_frame="teste.hex")

# Grid Search, Seleção do modelo e Teste
# Lista de hiperparametros
ntrees_opts = c(200,500,1000)
max_depth_opts = c(10,20,30)

hyper_params = list(ntrees = ntrees_opts, 
                    max_depth = max_depth_opts)

search_criteria = list(strategy = "RandomDiscrete", 
                       max_runtime_secs = 600, 
                       max_models = 100, 
                       stopping_metric = "AUTO", 
                       stopping_tolerance = 0.00001, 
                       stopping_rounds = 5, 
                       seed = 123456)

# Grid Search GBM
dl_grid = h2o.grid("drf", x = 2:44, y = "Survived",
                    grid_id = "drf_grid",
                    training_frame = treino.hex,
                    nfolds = 5,
                    stopping_rounds = 2,
                    stopping_tolerance = 1e-3,
                    stopping_metric = "logloss",
                    score_tree_interval = 100,
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)

# Resultados do Grid
dl_gridperf = h2o.getGrid(grid_id = "drf_grid",sort_by = "logloss", decreasing = TRUE)
print(dl_gridperf)

# Selecionar o model_id para o modelo top GBM
best_dl_model_id = dl_gridperf@model_ids[[1]]
best_dl = h2o.getModel(best_dl_model_id)
print(best_dl)

# Gráficos do modelo GBM
plot(best_dl, timestep = "number_of_trees", metric = "logloss")
plot(best_dl, timestep = "number_of_trees", metric = "auc")
plot(best_dl, timestep = "number_of_trees", metric = "rmse")

# Verficação da importância das variáveis do modelo
h2o.varimp_plot(best_dl)

# Predições:
Survived = h2o.predict(best_dl,newdata = teste.hex)
print(Survived)

# Solução ao Kaggle
solucao = data.frame(PassengerId = teste[1], Survived = as.data.frame(Survived)$predict)
head(solucao)
write.csv(solucao, file="solucao_h2o.csv",row.names = FALSE)