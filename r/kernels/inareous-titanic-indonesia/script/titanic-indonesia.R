
library('caret')
library('dplyr')
train <- read.csv('../input/train.csv',stringsAsFactors=FALSE)
test <- read.csv('../input/test.csv',stringsAsFactors=FALSE)
colnames(train) <- c("ID", "Selamat","Kelas","Nama","JenisKelamin","Umur","Saudara","OrtuAnak","Tiket","Harga","Kabin","Pelabuhan")
colnames(test) <- c("ID","Kelas","Nama","JenisKelamin","Umur","Saudara","OrtuAnak","Tiket","Harga","Kabin","Pelabuhan")

full <- bind_rows(train,test)


str(full)

summary(full)

library('mice')
md.pattern(full)



full$Harga[is.na(full$Harga)] <- median(full$Harga, na.rm = TRUE)


full$Pelabuhan[is.na(full$Pelabuhan)] <- "S"


head(full$Nama)


full$Gelar <- gsub('(.*, )|(\\..*)','',full$Nama)

table(full$JenisKelamin, full$Gelar)


rare <-c('Capt','Col','Don','Dona','Dr','Jonkheer','Lady','Major','Rev','the Countess','Sir')
full$Gelar[full$Gelar %in% rare] <- 'Rare' 


full$Gelar[full$Gelar == 'Mlle']<- 'Miss'
full$Gelar[full$Gelar == 'Ms']  <- 'Miss'
full$Gelar[full$Gelar == 'Mme'] <- 'Mrs'

table(full$JenisKelamin, full$Gelar)

full$JumlahKeluarga <- full$Saudara + full$OrtuAnak + 1

library('ggplot2')
library('ggthemes')
library('scales')
ggplot(full[1:891,], aes(x = JumlahKeluarga, fill = factor(Selamat))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:15)) +
  labs(x = 'Jumlah Keluarga') +
  theme_few()

full$Sendiri[full$JumlahKeluarga == 1] <- 'Sendiri'
full$Sendiri[full$JumlahKeluarga != 1] <- 'Tidak Sendiri'
full$BesarKeluarga[full$JumlahKeluarga == 1] <- 'Jomblo'
full$BesarKeluarga[full$JumlahKeluarga >1 & full$JumlahKeluarga < 5] <- 'Keluarga Kecil'
full$BesarKeluarga[full$JumlahKeluarga > 4] <- 'Keluarga Besar'

#Imputasi Umur

library(rpart)
Imputasi_Umur <- rpart(Umur ~ Kelas + JenisKelamin + Saudara + OrtuAnak + Harga + Pelabuhan + Gelar + BesarKeluarga,
                       data = full[!is.na(full$Umur), ], method = "anova")
full$Umur[is.na(full$Umur)] <- predict(Imputasi_Umur, full[is.na(full$Umur), ])

factor_vars <- c('Kelas' , 'Umur', 
                 'JenisKelamin' ,'Selamat','Gelar',
                 'Pelabuhan','BesarKeluarga','Sendiri')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))



train <- full[1:891,]
test <- full[892:1309,]
extractFeatures <- function(data){
  features <- c(#'ID',
    'Selamat',
    'Kelas',
    #'Nama',
    'JenisKelamin',
    'Umur',
    'Saudara',
    'OrtuAnak',
    #'Tiket',
    #'Kabin',
    'BesarKeluarga',
    'Pelabuhan',
    'Gelar',
    'JumlahKeluarga',
    'Sendiri'
  )
  fea <- data[,features]
  return(fea)
}


library(party)

set.seed(240996)

fit <- cforest(Selamat ~.,
               data=extractFeatures(train),
               controls=cforest_unbiased(ntree=500, mtry=3))

fit
caret:::cforestStats(fit)


predictx <- predict(fit, extractFeatures(test), OOB = TRUE, type="response")

submission <- data.frame(PassengerId = test$ID, Survived = predictx)
write.csv(submission, file = "Hasil_cForest.csv", row.names=FALSE)