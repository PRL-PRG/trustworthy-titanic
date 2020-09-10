####### Getting Packages ############

required_packages = c("plyr","data.table","dplyr","DT","dbscan","ggplot2","mice","randomForest","vcd","ggpubr"
                      ,"rmarkdown","flexdashboard","grid","gridExtra","corrplot","dummies","xgboost")



## Check for installed packages and install missing ones
new_packages = required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

## Load necessary packages
lapply(required_packages, require, character.only = TRUE)
print("Loading Packages Done.")

###### Adding some usefuls functions #############
options(stringsAsFactors = FALSE)
detect_outlier<-function(datatable,n,features){
  #need library("plyr","data.table")
  outlier_indices<-c()
  dt_outlier_indices<-data.table()
  datatable$index<-rownames(datatable)
  for (val in features){
    Q1<-quantile(datatable[!is.na(datatable[[val]])][[val]],.25)
    Q3<-quantile(datatable[!is.na(datatable[[val]])][[val]],.75)
    #interquantile range
    IQR<-Q3-Q1
    #outlier step
    outlier_step<-1.5*IQR
    if(empty(datatable[datatable[[val]]<Q1-outlier_step | datatable[[val]]>Q3+outlier_step,c("index")])){
      outlier_list_col<-0
    } else {
      outlier_list_col<-datatable[datatable[[val]]<Q1-outlier_step | datatable[[val]]>Q3+outlier_step,c("index")]
    }
    outlier_indices<-c(outlier_indices,outlier_list_col)
    
  }
  dt_outlier_indices<-data.table(as.numeric(unlist(outlier_indices)))
  dt_outlier_indices<-dt_outlier_indices[,.N,by=V1][N>n] #choose to keep outliers in more than n features
  datatable[as.numeric(rownames(datatable)) %in% dt_outlier_indices$V1]
}

check_na_blank<-function(datatable){
  for(j in seq_along(datatable)){
    if(!empty(datatable[datatable[[j]]=="" | datatable[[j]]==" " | is.na(datatable[[j]]), ])){
      set(datatable, i=which(datatable[[j]]==""), j=j, value=NA)
      set(datatable, i=which(datatable[[j]]==" "), j=j, value=NA)
      print(paste("Replacing ' '/ '' to NA in",colnames(datatable)[j]))
    }
  }
}
print("Loading Lib Done.")



df_train<-data.table(read.csv('../input/train.csv',stringsAsFactors = F))
outliers<-detect_outlier(df_train,2,c("Age","SibSp","Parch","Fare"))
df_train<-df_train[!as.numeric(rownames(df_train)) %in% outliers$PassengerId]
df_test<-data.table(read.csv('../input/test.csv',stringsAsFactors = F))
full_data<-bind_rows(df_train,df_test)
check_na_blank(full_data)

#################################################################### Titles #####################################################
full_data$Titre<-gsub('(.*, )|(\\..*)', '',full_data$Name)
df_titles<-full_data[,.N,by="Titre"]
titles_noisy<-df_titles[df_titles$N<=10,]$Titre
full_data$Titre[full_data$Titre %in% titles_noisy]<-'Noisy title'
for (i in 1:length(unique(full_data$Titre)))
     {
       full_data[Titre==unique(full_data$Titre)[i]]$Titre<-as.character(i)
}
full_data$Titre<-as.integer(full_data$Titre)
full_data<-cbind(full_data,dummy(full_data$Titre,sep="_Titre_"))
#################################################################### Sex ########################################################
full_data$Sex<-unlist(lapply(full_data$Sex,function(x) sapply(x,return(ifelse(x=="male",1,0)))))
################################################################### SibSp Parch #################################################
full_data$Family_Size<-full_data$SibSp+full_data$Parch+1
full_data$Single<-unlist(lapply(full_data$Family_Size,function(x) sapply(x,return(ifelse(x==1,1,0)))))
full_data$Small<-unlist(lapply(full_data$Family_Size,function(x) sapply(x,return(ifelse(x==2,1,0)))))
full_data$Medium<-unlist(lapply(full_data$Family_Size,function(x) sapply(x,return(ifelse((3<=x | x<=4 ),1,0)))))
full_data$Large<-unlist(lapply(full_data$Family_Size,function(x) sapply(x,return(ifelse(5<=x,1,0)))))
#################################################################### Ticket #####################################################
Extract_Ticket<-c()
for ( i in full_data$Ticket){
  if (is.na(as.numeric(i))){
    i<-gsub(" .*$","",i)
    i<-gsub(pattern='((?<![0-9])\\.)|(\\.(?![0-9]))','',i, perl=TRUE)
    i<-gsub("/","",i)
    print(i)
    Extract_Ticket<-c(Extract_Ticket,gsub("/","",i))
  }
  else {
    Extract_Ticket<-c(Extract_Ticket,"X")
  }
  Extract_Ticket
}
full_data<-cbind(full_data,Extract_Ticket)
full_data<-cbind(full_data,dummy(full_data$Extract_Ticket,sep="_Ext_"))

################################################################## Cabin ########################################################
full_data$Cabin_New<-factor(sapply(full_data$Cabin,function(x) strsplit(x, NULL)[[1]][1]))
full_data[is.na(full_data$Cabin_New)]$Cabin_New<-"X"
unique(full_data$Cabin_New)
full_data<-cbind(full_data,dummy(full_data$Cabin_New,sep="_Cabin_"))
colSums(is.na(full_data))
full_data[grep("NA",full_data$Cabin_New),]$Cabin_New
#################################################################### Missing values : Embarked ########################################
full_data[is.na(full_data$Fare)]$Fare <- median(full_data[full_data$Pclass == '3' & full_data$Embarked == 'S', ]$Fare, na.rm = TRUE)
colSums(is.na(full_data))

full_data$Embarked[full_data$Embarked=="S"]<-1
full_data$Embarked[full_data$Embarked=="C"]<-2
full_data$Embarked[full_data$Embarked=="Q"]<-3

########################### missing values Embarked #############
full_data$Embarked[full_data$PassengerId==62]<-2
full_data$Embarked[full_data$PassengerId==830]<-1
full_data$Embarked<-as.numeric(full_data$Embarked)

full_data<-cbind(full_data,dummy(full_data$Embarked,sep="_Embarked_"))

################################################################### Missing values : Age ###############################################
full_data<-cbind(as.integer(rownames(full_data)),full_data)

for (i in full_data[is.na(Age),which=TRUE]){
     full_data[V1==i]$Age<-median(full_data[SibSp==full_data[V1==i,]$SibSp & Parch==full_data[V1==i,]$Parch & Pclass==full_data[V1==i,]$Pclass & !is.na(full_data$Age)]$Age)
}
#a discuter 
#age_median_value<-median(full_data[!is.na(full_data$Age),]$Age)
#full_data[V1==1223]$Age<-age_median_value
#full_data[V1==1246]$Age<-age_median_value

full_data$Pclass<-as.integer(full_data$Pclass)
full_data$Sex<-as.integer(full_data$Sex)
full_data$Embarked<-as.integer(full_data$Embarked)

full_data$Cabin_New<-factor(full_data$Cabin_New)
train <- full_data[1:880,]
test <- full_data[881:1298,]



rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + NA_Titre_1 + NA_Titre_2 + NA_Titre_3  + NA_Titre_4
                           + NA_Titre_5 + Single + Small + Medium + Large +  NA_Embarked_1
                           + NA_Embarked_2 + NA_Embarked_3 
                           + NA_Cabin_A + NA_Cabin_B + NA_Cabin_C + NA_Cabin_D
                           + NA_Cabin_E + NA_Cabin_F + NA_Cabin_G + NA_Cabin_T
                           + NA_Cabin_X
                           + NA_Ext_A + NA_Ext_A4 + NA_Ext_A5 + NA_Ext_AQ3
                           + NA_Ext_AS + NA_Ext_C + NA_Ext_CA + NA_Ext_CASOTON
                           + NA_Ext_Fa + NA_Ext_FC + NA_Ext_FCC + NA_Ext_LINE 
                           + NA_Ext_LP + NA_Ext_PC + NA_Ext_PP + NA_Ext_PPP
                           + NA_Ext_SC + NA_Ext_SCA3 + NA_Ext_SCA4 + NA_Ext_SCAH + NA_Ext_SCOW 
                           + NA_Ext_SCParis + NA_Ext_SCPARIS + NA_Ext_SOC + NA_Ext_SOP + NA_Ext_SOPP
                           + NA_Ext_SOTONO2 + NA_Ext_SOTONOQ + NA_Ext_SP + NA_Ext_STONO + NA_Ext_STONO2
                           + NA_Ext_STONOQ + NA_Ext_SWPP + NA_Ext_WC + NA_Ext_WEP + NA_Ext_X
                           ,data=train)
                           
print(rf_model)                            
print("Survived:0 = No, 1 = Yes")
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = 'rf_mod_Solution_1.csv', row.names = F)


