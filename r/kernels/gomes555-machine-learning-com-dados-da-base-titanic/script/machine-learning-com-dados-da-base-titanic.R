## ----warning=F,eval=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## # Pacotes Utilizados:
## library(dplyr)       # Manipulacao de dados
## library(readr)       # Leitura da base csv
## library(ggplot2)     # Visualizacao elegante dos dados
## library(GGally)      # Suporte na visualizacao dos dados
## library(vcd)         # Calcular as medidas de associação da tabela
## library(tabplot)     # Visualizacao geral dos dados
## library(data.table)  # Objeto datatable semelhante ao dataframe
## library(gtools)      # Funções para auxiliar na programação R
## library(plotly)      # visualizacao interativa
## library(rpart)       # Ajustar arvore de decisoes
## library(rpart.plot)  # Visualizar arvore de decisoes
## # library(rattle)      # Visualizar arvore de decisoes
## library(caret)       # Biblioteca para machine learning
## library(formattable) # Criar tabelas elegantes
## library(FFTrees)     # Ajuste rápido e descritivo de arvore de decisoes


## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(dplyr))
suppressMessages(library(readr))
suppressMessages(library(ggplot2))
suppressMessages(library(GGally))


system("ls ../input", intern=TRUE)

train=read.csv("../input/train.csv",header=T)
test=read.csv("../input/test.csv",header=T)
full=bind_rows(train, test)

#Salvando a coluna para enviar os resultados depois
PassengerId=test$PassengerId


## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glimpse(full)


## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g1=ggplot(full, aes(x = as.factor(Survived),fill=as.factor(Survived))) + 
  geom_bar()+scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+theme_bw()+
  labs(x="Sobrevivente",title="Base Completa")

g2=ggplot(train, aes(x = as.factor(Survived),fill=as.factor(Survived))) + 
  geom_bar()+scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+theme_bw()+
  labs(x="Sobrevivente",title="Base de Treino")

gridExtra::grid.arrange(g1,g2,ncol=2)



## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.test(sum(full$Survived,na.rm = T),    # Proporcao de sobreviventes ignorando as linhas de NA
          length(na.omit(full$Survived)))  # Numero total de pessoas ignorando as linhas NA


## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(train$Pclass)
ggplot(train, aes(x = as.factor(Pclass),fill=as.factor(Survived))) + 
  geom_bar(position = "dodge")+scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+theme_bw()+
  labs(x="Sobrevivente",title="Base de Treino")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Title = gsub('(.*, )|(\\..*)', '', full$Name) #Obtendo apenas o titulo da pessoa

#Conferindo frequências:
sort(table(full$Title))


## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

train <- full[1:891,]
test <- full[892:1309,]

ggplot(train, aes(x = as.factor(Title),fill=as.factor(Survived))) + 
  geom_bar(position = "dodge")+scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+theme_bw()+
  labs(x="Sobrevivente",title="Base de Treino")


## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(train$Sex)
ggplot(train, aes(x = as.factor(Sex),fill=as.factor(Survived))) + 
  geom_bar(position = "dodge")+scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+theme_bw()+
  labs(x="Sobrevivente",title="Base de Treino")



## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g1=ggplot(train, aes(x=Age, color=factor(Survived), fill=factor(Survived))) + 
 geom_histogram(aes(y=..density..), alpha=0.5, 
                position="identity")+
 geom_density(alpha=.2) +theme_bw()

g2=ggplot(train,aes(x=Age, y=Survived))+
  geom_point()+
  geom_smooth()+theme_bw()

gridExtra::grid.arrange(g1,g2,ncol=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$SepAge=ifelse(full$Age<18,"child",ifelse(full$Age<60,"adult", "old"))
full$SepAge=as.factor(full$SepAge)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1


## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(vcd))

#Comando para tabela cruzada:
tab <- xtabs(~ Ticket + Pclass, data = train)
#Calcular as medidas de associação da tabela:
summary(assocstats(tab))


## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g1=qplot(y=train$Fare)+geom_boxplot(outlier.colour = "red", outlier.shape = 1)+theme_bw()+labs(title="Distribuicao com alta Tarifa")

g2=ggplot(train, aes(x=Fare, color=factor(Survived), fill=factor(Survived))) + 
 geom_histogram(aes(y=..density..), alpha=0.5, 
                position="identity")+
 geom_density(alpha=.2) +theme_bw()+labs(title="Com alta Tarifa")

#Removendo que tem Fare maior que 50

g3=qplot(y=train$Fare[train$Fare<50])+geom_boxplot(outlier.colour = "red", outlier.shape = 1)+theme_bw()+labs(title="Distribuicao sem alta Tarifa")

g4=ggplot(train[train$Fare<50,], aes(x=Fare, color=factor(Survived), fill=factor(Survived))) + 
 geom_histogram(aes(y=..density..), alpha=0.5, 
                position="identity")+
 geom_density(alpha=.2) +theme_bw()+labs(title="Sem alta Tarifa")

#Indentificando outliers:
gridExtra::grid.arrange(g1,g2,g3,g4,ncol=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$SupFare=ifelse(full$Fare>50,1,0)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Survived=as.factor(full$Survived)
full$Pclass=as.factor(full$Pclass)
full$Sex=as.factor(full$Sex)
full$Embarked=as.factor(full$Embarked)
full$Title=as.factor(full$Title)
full$SupFare=as.factor(full$SupFare)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full=full%>%
  select(Survived, Pclass, Sex, SepAge, Embarked, Fsize, SupFare,Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- full[1:891,]
test <- full[892:1309,]


## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(tabplot))
tableplot(train, sortCol = Survived)


## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(ggplot2))
ggpairs(train, mapping = aes(color = Survived),binwidth=.5)


## ----warning=F,echo=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(data.table)) 
suppressMessages(library(gtools))
suppressMessages(library(plotly))

cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x)[1] * (min(length(unique(x))[1],length(unique(y))[1]) - 1)))
  return(as.numeric(CV))
}

cor_all <- function(df){
  cat_var <- colnames(df)
  cat_var_grid <- data.table(combinations(n = length(cat_var), r = 2, v = cat_var, repeats.allowed = FALSE))
  
  do.call(rbind,
          apply(cat_var_grid, 1, function(x){
            tmp <- as.character(x)
            vec1 <- unlist(df[,tmp[1]])
            vec2 <- unlist(df[,tmp[2]])
            
            data.table(
              variable_x = tmp[1],
              variable_y = tmp[2],
              p.value = cv.test(vec1, vec2)
            )
          }))
  
}

results=train%>%
  select(Survived, Pclass, Sex, SepAge, Embarked, SupFare,Title)%>%
  as.data.frame()%>%
  na.omit()%>%
  cor_all()


g <- ggplot(results, aes(variable_x, variable_y)) +
  geom_tile(aes(fill = p.value), colour = "black") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  ggtitle("Cramer's V heatmap")

ggplotly(g)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$Survived=as.numeric(train$Survived)
train$Survived=ifelse(train$Survived==1,0,1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
amostra=sample(1:2, length(train$Survived), replace=T, prob=c(0.7,1-0.7))
train2=train[amostra==1,]
test2=train[amostra==2,]


## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Arvores de Decisao

#PAcotes que sera utilizados:
suppressMessages(library(rpart))
suppressMessages(library(rpart.plot))

#Importando os dados que serao utilizados no exemplo:
dados=train2

#Construindo uma arvore de decisao:
modelo_ad=rpart(Survived ~ .,                    #Formula que define qual eh o argumento classe e quais sao os atributos
             data=dados,                            #E o conjunto de dados armazenado em um data.frame
             method="class",                        #E o parametro que define se a arvore sera usada como classificacao ("class") ou regressao ("anova")
             control=rpart.control(minsplit=5),     #E a variavel na qual sao armazenados os parametros que controlam o crescimento da arvore
             parms=list(split="Information")        #E a variavel na qual eh definido o criterio de selecao de atributos
             )

#Usando uma arvore de decisao:
y_estimado=predict(modelo_ad,                       #Eh o resultado da construcao da arvore de decisao
                   test2[,-1],                    #Eh a variavel que contem os atributos descritivos dos exemplares de teste, considerando os mesmos atributos descritivos usados na geracao de modelo_ad
                   "class")                         #Especifica como sera apresentado o resultado da predicao: um vetor para valores numericos ("vector"), classe para valores categoricos ("class"), ou a probabilidade de cada classe ("prob")

#Visualizando uma arvore de decisao:
plot=rpart.plot(modelo_ad,                          #Eh o resultado da contrucao da arvore de decisao
                2,                                  #Eh a quantidade de casas decimais
                2)                                  #Especifica como sera apresentado o resultado da visualizacao, pode assumiar valores de 0 a 4


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Fazendo uma predicao:
previsao=as.vector(predict(modelo_ad, test2[,-1], type="class"))
previsao=as.numeric(previsao)
resposta=as.matrix(test2[,1])
resposta=as.numeric(resposta)

matrix_confusao=caret::confusionMatrix(previsao,resposta)
matrix_confusao


## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dados=train
dados$Survived=as.factor(dados$Survived)

library(caret)
#k - fold cress validation
trainControl = trainControl(method="cv", number=10)

#Treina uma arvore de decisao
tree = train(Survived ~ . , data = dados, method="rpart", trControl=trainControl, na.action = na.pass)

suppressMessages(library(formattable))
#Resultado do ajuste:
tree$results%>%
  as.data.frame()%>%
formattable( list(

  area(col = c(cp)) ~ normalize_bar("lightgrey", 0.2),
  area(col = c(Accuracy)) ~ normalize_bar("lightgrey", 0.2),
  area(col = c(Kappa)) ~ normalize_bar("lightgrey", 0.2),
  area(col = c(AccuracySD)) ~ normalize_bar("lightgrey", 0.2),
  area(col = c(KappaSD)) ~ normalize_bar("lightgrey", 0.2)
  
))


#O modelo sera:
#library(rattle)
#fancyRpartPlot(tree$finalModel)



## ----warning=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(FFTrees))



heart.fft <- FFTrees(formula = Survived ~.,                    # Criterion
                     data = train2,                            # Training data   
                     data.test = test2,do.comp = FALSE)        # Testing data


table(test2$Survived)
plot(heart.fft,                                                # FFTrees object
     main = "Arvore de decisoes sobre sobrevivência",          # Plot title
     decision.names = c("Not Survived", "Survived"),           # Decision names
     data = "test") 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

previsao=as.vector(predict(modelo_ad, test, type="class"))

# Salve a solução em um dataframe com duas colunas: PassengerId e Survived (predição)
solution <- data.frame(PassengerID = PassengerId, Survived = previsao)

# Escreva a solução para o arquivo
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

