## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# パッケージの読み込み
library("ggplot2") # グラフィック
library("gridExtra")
library("ggthemes")

library("dplyr") # データ操作
library("scales")
library("tibble")

library("mice") # 多重代入法
library("VIM") # 欠損値の可視化

library("randomForest") # ランダムフォレスト


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# データの読み込み
data("Titanic")
train=titanic::titanic_train
test=titanic::titanic_test

# すべてのデータ
full=bind_rows(train, test)

# 確認
str(full)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Cabinの空白をNAに
full$Cabin=ifelse(full$Cabin=="",NA,full$Cabin)
#変数の欠損率を求める
missing_values=full %>% summarize_all(funs(sum(is.na(.))/n()))
missing_values=tidyr::gather(missing_values, key="feature", value="missing_pct")

ggplot(missing_values,aes(x=reorder(feature,-missing_pct),y=missing_pct))+
  geom_bar(stat="identity",fill="red")+
  coord_flip()+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図1：各変数の欠損率")


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 正規表現で敬称を抽出
full$Title=gsub('(.*, )|(\\..*)', '', full$Name)

# 集計表の出力
df_title=data.frame(table(full$Sex, full$Title))
df_title=df_title[order(df_title$Freq, decreasing=T),]
(df_title=df_title[df_title$Freq > 0,])


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 少数のTitleをまとめる
minor=c("Dona","Lady","the Countess","Capt","Col","Don","Dr","Major","Rev","Sir","Jonkheer")

# フランスの敬称をMiss,Mrsに割り当てる
full$Title[full$Title=="Mlle"]="Miss"
full$Title[full$Title=="Mme"]="Mrs"

full$Title[full$Title %in% minor]="minor"
full$Title[full$Title=="Ms"]="Miss"


## ----message=FALSE,warning=FALSE,results='hide'----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#確認
table(full$Sex, full$Title)
ggplot(full[1:891,],aes(x = Title,fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図2：敬称ごとの生存結果")


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 兄弟、配偶者の数 + 両親、子供の数 + 乗客本人の数 = 家族の規模
full$Family=full$SibSp+full$Parch+1

# 確認
ggplot(full[1:891,],aes(x = Family,fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  scale_x_continuous(breaks = c(1:11))+
  geom_vline(xintercept=1.5,linetype="dashed",colour="blue")+ # グループを分ける
  geom_vline(xintercept=4.5,linetype="dashed",colour="blue")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図3：家族の規模ごとの生存結果")


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# single, small, largeに分類する
full$FamilyG[full$Family==1]="single"
full$FamilyG[1<full$Family & full$Family<5]="small"
full$FamilyG[full$Family>4]="large"

#モザイクプロットの作成
par(family = "HiraKakuProN-W3") # 日本語表示
mosaicplot(table(full$FamilyG, full$Survived), main="図4：家族規模のグループ×生存結果", shade=TRUE)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(sum(is.na(full$Age))/nrow(full), 3)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
n_full=bind_rows(train,test)
n_full$Cabin=ifelse(n_full$Cabin=="",NA,n_full$Cabin)

# 左図：単一のデータ項目に対する欠損値の棒グラフ（個数）右図：複数のデータ項目に対する欠損値のパターン
aggr(n_full, prop=FALSE, number=TRUE)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 変数をfactor型に変換
factor_vars=c("Pclass","Name","Sex","Ticket","Embarked","Title","FamilyG")
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# 推定に必要な変数以外を除いて、欠損値を補完したデータセットを作成
mice_model=mice(full[, !names(full) %in% c("PassengerId","Name","Ticket","Cabin","Family","Survived")], method = "rf", seed = 119, print = FALSE)

# 欠損値の補完を行ったデータを保存
mice_output <- complete(mice_model)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2), family = "HiraKakuProN-W3")
hist(full$Age, freq=F, main="図5：もとのデータのAge", 
  col="violetred4", ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main="MICEによる出力データのAge", 
  col="violet", ylim=c(0,0.04))


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Age=mice_output$Age
# Ageの欠損率を確認する
round(sum(is.na(full$Age))/nrow(full), 3)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# プロットする
ggplot(full[1:891,],aes(x = Pclass, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  geom_vline(xintercept=1.5,linetype="dashed",colour="blue")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図6：等級ごとの生存結果")


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 密度推定を行いプロットする
ggplot(full[1:891,],aes(x = Age, fill=factor(Survived)))+
  geom_density(alpha=0.5, aes(fill=factor(Survived)))+
  scale_x_continuous(breaks = seq(0,80,5))+
  geom_vline(xintercept=15,linetype="dashed",colour="blue")+
  geom_vline(xintercept=32,linetype="dashed",colour="blue")+
  geom_vline(xintercept=58,linetype="dashed",colour="blue")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図7：年齢あたりの生存結果の密度")


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# under15, young, middle, oldに分類する
full$AgeG[full$Age<16]="under15"
full$AgeG[15<full$Age & full$Age<33]="young"
full$AgeG[32<full$Age & full$Age<58]="middle"
full$AgeG[57<full$Age]="old"
full$AgeG=as.factor(full$AgeG)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Pclass=1のグループ
g.sex.p1=ggplot(subset(full[1:891,], Pclass==1),aes(x = Sex, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図8：性別ごとの生存結果（Pclass=1）")
g.ageg.p1=ggplot(subset(full[1:891,], Pclass==1),aes(x = AgeG, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図9：年齢層（AgeG）ごとの生存結果（Pclass=1）")
grid.arrange(g.sex.p1, g.ageg.p1, nrow=2)
# Pclass=2のグループ
g.sex.p2=ggplot(subset(full[1:891,], Pclass==2),aes(x = Sex, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図10：性別ごとの生存結果（Pclass=2）")
g.ageg.p2=ggplot(subset(full[1:891,], Pclass==2),aes(x = AgeG, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図11：年齢層（AgeG）ごとの生存結果（Pclass=2）")
grid.arrange(g.sex.p2, g.ageg.p2, nrow=2)
# Pclass=2,3のグループ
g.sex.p3=ggplot(subset(full[1:891,], Pclass==3),aes(x = Sex, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図12：性別ごとの生存結果（Pclass==3）")
g.ageg.p3=ggplot(subset(full[1:891,], Pclass==3),aes(x = AgeG, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図13：年齢層（AgeG）ごとの生存結果（Pclass=3）")
grid.arrange(g.sex.p3, g.ageg.p3, nrow=2)


## ----message=FALSE,warning=FALSE,echo=FALSE,include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 念のために他の変数との関係を見てみる。  
# Pclass=1,2かつ女性のグループ
p12.f=subset(full[1:891,], Pclass %in% c(1,2) & Sex=="female")

g.p12.f.fg=ggplot(p12.f,aes(x = FamilyG, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図14：各変数ごとの生存結果（Pclass=1,2かつ女性）")
g.p12.f.ag=ggplot(p12.f,aes(x = AgeG, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="")
g.p12.f.t=ggplot(p12.f,aes(x = Title, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="")
g.p12.f.em=ggplot(p12.f,aes(x = Embarked, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="")
grid.arrange(g.p12.f.fg, g.p12.f.ag, g.p12.f.t, g.p12.f.em,
             nrow=2, ncol=2)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# AgeG=under15のグループ
ggplot(subset(full[1:891,], AgeG=="under15"),aes(x = Pclass, fill=factor(Survived)))+
  geom_bar(position = "fill")+
  geom_vline(xintercept=2.5,linetype="dashed",colour="blue")+
  scale_y_continuous(labels = percent)+
  coord_flip()+ # x軸とy軸を入れ替える
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図15：生存結果の比率（AgeG=under15）")


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Pclass=1,2かつAgeG=under15のグループ
p12.u15=subset(full[1:891,], Pclass %in% c(1,2) & AgeG=="under15") 

# Pclass=1,2かつAgeG=under15のグループの生存確率の比率
round(nrow(p12.u15[p12.u15$Survived==1,])/nrow(p12.u15), 3)


## ----message=FALSE,warning=FALSE,echo=FALSE,include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 念のために他の変数との関係を見てみる。
g.p12.u15.fg=ggplot(p12.u15,aes(x = FamilyG, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図16：各変数ごとの生存結果（Pclass=1,2かつAgeG=under15）")
g.p12.u15.t=ggplot(p12.u15,aes(x = Title, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="")
g.p12.u15.em=ggplot(p12.u15,aes(x = Embarked, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="")
grid.arrange(g.p12.u15.fg, g.p12.u15.t, g.p12.u15.em,
             nrow=2, ncol=2)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Child_surv=ifelse(full$Pclass %in% c(1,2) & full$AgeG=="under15", 1, 0)
full$Child_surv=ifelse(is.na(full$Child_surv),0,full$Child_surv)
full$Female_surv=ifelse(full$Pclass %in% c(1,2) & full$Sex=="female", 1, 0)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full[1044, ]


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
similar=subset(full, Pclass==3 & Sex=="male" & FamilyG=="single" & AgeG=="old")
similar[,c("Pclass","Sex","Ticket","Fare","Cabin","Embarked","FamilyG","AgeG")]


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full[1044, ]$Fare=mean(similar[-9,]$Fare)
full[1044, ]


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ticketの頻度を求める
Ticket.Freq=aggregate(full$Ticket, by=list(full$Ticket), length)
colnames(Ticket.Freq)=c("Ticket", "TFreq")
TFreq2=Ticket.Freq[Ticket.Freq$Freq>1,]

full$TFreq=numeric(1309) # 領域の確保
for (i in 1:1309) {
  full[i,]$TFreq=Ticket.Freq[Ticket.Freq$Ticket==full[i,]$Ticket,]$TFreq
}

ggplot(full[1:891,],aes(x = TFreq, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  geom_vline(xintercept=2.5,linetype="dashed",colour="blue")+
  geom_vline(xintercept=4.5,linetype="dashed",colour="blue")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図17：チケット番号の頻度ごとの生存結果")


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# TFreqのグループごとのPclassを確認する
g.tf1=ggplot(subset(full[1:891,], TFreq<3),aes(x = Pclass, fill=factor(Survived)))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = percent)+
  coord_flip()+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図18：Pclassごとの生存結果の割合（TFreq<3）")
g.tf2=ggplot(subset(full[1:891,], TFreq>2 & TFreq<5),aes(x = Pclass, fill=factor(Survived)))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = percent)+
  coord_flip()+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図19：Pclassごとの生存結果の割合（TFreq=3,4）")
g.tf3=ggplot(subset(full[1:891,], TFreq>4),aes(x = Pclass, fill=factor(Survived)))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = percent)+
  coord_flip()+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図20：Pclassごとの生存結果の割合（TFreq>4）")
grid.arrange(g.tf1, g.tf2, g.tf3, nrow=3)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Fare
ggplot(subset(full[1:891,], TFreq<3 | TFreq>4),aes(x = factor(Pclass), y = Fare, fill = factor(Pclass)))+
  geom_boxplot()+
  scale_y_continuous(breaks=seq(0,250,50), labels=dollar_format()) +
  geom_hline(yintercept=50,linetype="dashed",colour="blue")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図21：Pclassごとの乗船料金")


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data.frame("Pclass"=c(1,2,3), "parsent"=c(nrow(full[full$Pclass==1 & full$Fare<50,])/nrow(full),nrow(full[full$Pclass==2 & full$Fare<50,])/nrow(full),nrow(full[full$Pclass==3 & full$Fare<50,])/nrow(full)))


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g.f1=ggplot(subset(full[1:891,], TFreq<3 & Fare>0 & Fare<50),aes(x = Title, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図22：敬称ごとの生存結果（TFreq<3 Fare<50）")
g.f2=ggplot(subset(full[1:891,], TFreq>2 & TFreq<5 & Fare>0 & Fare<50),aes(x = Title, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="（TFreq=3,4 Fare<50）")
g.f3=ggplot(subset(full[1:891,], TFreq>4 & Fare>0 & Fare<50),aes(x = Title, fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="（4<TFreq Fare<50）")
grid.arrange(g.f1, g.f2, g.f3, nrow=3)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Fam34_surv=ifelse(full$TFreq %in% c(3,4) & full$Fare<50 & full$Title %in% c("Master","Miss","Mrs"), 1, 0)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full[c(62, 830),]


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
similar=subset(full, Pclass==1 & Sex=="female" & FamilyG=="single" & TFreq==2)
similar[,c("Pclass","Sex","Ticket","Fare","Cabin","Embarked","FamilyG","AgeG")]


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full[c(62, 830),]$Embarked="C"


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Fare>100は外れ値なので除いて表示する
ggplot(subset(full, Fare<100), aes(x = Embarked, y = Fare, fill = factor(Survived))) +
  geom_boxplot()+
  scale_y_continuous(labels=dollar_format())+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図23：乗船した港と生存結果ごとの乗船料金の箱ひげ図")


## ----warning=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 欠損率を求める
na.par=c(
  round(sum(is.na(full[full$Embarked=="C",]$Survived))/nrow(full[full$Embarked=="C",]),3),
  round(sum(is.na(full[full$Embarked=="Q",]$Survived))/nrow(full[full$Embarked=="Q",]),3),
  round(sum(is.na(full[full$Embarked=="S",]$Survived))/nrow(full[full$Embarked=="S",]),3)
)
(data.frame("Pclass"=c("C","Q","S"), "欠損率"=na.par))


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 生存結果が欠損していないデータを訓練データとする
train=full[1:891,]
# 生存結果が欠損しているデータをテストデータとする
test=full[892:1309,]


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# シード
set.seed(119)

# モデルの構築
rf_model=randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                                          Fare + Embarked + Title + 
                                          Family + FamilyG + AgeG + 
                                          Child_surv + Female_surv + TFreq + Fam34_surv,
                                          data = train)

# 構築したモデルの誤判別率をプロットする
plot(rf_model, ylim=c(0,0.36))
legend("topright", colnames(rf_model$err.rate), col=1:3, fill=1:3)


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 変数の重要度の取得
importance=importance(rf_model)
varImportance=data.frame(Variables = row.names(importance), 
                          Importance = round(importance[ ,"MeanDecreaseGini"],2))

# 取得した重要度を素に順位を作成する
rankImportance=varImportance %>%
                mutate(Rank = paste0("#",dense_rank(desc(Importance))))

# 変数の重要度と順位をプロットする
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
    y = Importance, fill = Importance)) +
  geom_bar(stat="identity") + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
    hjust=0, vjust=0.55, size = 4, colour = "orangered") +
  labs(x = "Variables") +
  coord_flip()+
  theme(text = element_text(family = "HiraKakuProN-W3"))+
  labs(title="図24：各変数の重要度")


## ----message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 予測を行う
prediction=predict(rf_model, test)

# 提出用のフォーマットを作成する
solution=data_frame(PassengerID = test$PassengerId, Survived = prediction)
# CSVデータとして書き出す
write.csv(solution, file = "predict_rf.csv", row.names = F)

