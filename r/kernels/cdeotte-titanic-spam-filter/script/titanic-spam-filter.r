
train <- read.csv("../input/train.csv",stringsAsFactors=F)
test <- read.csv("../input/test.csv",stringsAsFactors=F)
test$Survived <- NA
data <- rbind(train,test)

# create dictionary from training names
words = paste(data$Name[1:891],collapse=' ')
words = gsub('[.,"()/]','',words)
words = gsub(' . |  ',' ',words)
words = strsplit(words,' ')[[1]]
freq = ave(1:length(words),words,FUN=length)
dictionary = data.frame(Words=words,Freq=freq,stringsAsFactors=F)
dictionary = dictionary[!duplicated(dictionary$Words) & dictionary$Freq>1,]
dictionary <- dictionary[order(-dictionary$Freq),]

# create vectorization from dictionary
data2 <- data.frame(Mr=rep(0,1309))
for (i in dictionary$Words) data2[,i] <- 0
for (i in 1:nrow(data)){
    n = gsub('[.,"()/]','',data$Name[i])
    n = gsub(' . |  ',' ',n)
    n = strsplit(n,' ')[[1]]
    for (j in n){
        if (j %in% dictionary$Words){
            data2[i,j] = 1
        }
    }
}

# calculate survival probability
s=1:891
dictionary$Survival <- NA
for (i in 1:length(dictionary$Words)){
    x = intersect(which(data2[,dictionary$Words[i]]>0),s)
    dictionary$Survival[i] <- mean(data$Survive[x],na.rm=T)
}
# display words and survival
row.names(dictionary) <- 1:nrow(dictionary)
cat("15 most frequent Words and their survival rate:\n")
head(data.frame(n=1:nrow(dictionary),dictionary[order(-dictionary$Freq),]),15)
cat(sprintf('%d Words in dictionary\n',nrow(dictionary)))
dictionary$Words[order(dictionary$Words)]

data3 <- as.matrix(data2)
# calculate mean and sd
m = rep(0,ncol(data3))
s = rep(0,ncol(data3))
for (i in 1:ncol(data3)){
    m[i] = mean(data3[,i])
    s[i] = sd(data3[,i])
}
# calculate covariance
data4 = (1/1309)*t(data3) %*% data3 - m %*% t(m)
# calculate correlation
data5 = data4 / (s %*% t(s))
# calculate principal components
ev <- eigen(data4,symmetric=T)
ev2 <- data.frame(values=ev[[1]],vectors=ev[[2]])
cat('Principal components and correlations have been calculated.\n\n')
# find word pairs with frequency>=4 and high correlation
cat("Correlations above 0.3 of Words with freq>=4 are:\n")
for (i in 1:length(which(dictionary$Freq>=4)))
for (j in (i+1):length(which(dictionary$Freq>=4))){
    if (i!=j & abs(data5[i,j])>=0.3)
        cat(sprintf("%s and %s have correlation r = %f\n",row.names(data5)[i],row.names(data5)[j],data5[i,j]))	
}
#cat("\nCorrelations above 0.5 of Words with freq<=3\n")
#for (i in length(which(dictionary$Freq>=4)):nrow(data5)-1)
#for (j in (i+1):nrow(data5)-1){
#    if (i!=j & abs(data5[i,j])>=0.5)
#        cat(sprintf("%s and %s have correlation r = %f\n",row.names(data5)[i],row.names(data5)[j],data5[i,j]))
#}

library(ggplot2)
library(gridExtra)

#ggplot(data=data2, aes(x=data2$Charles,y=data2$Mr)) + 
#    geom_jitter(width=0.1,height=0.1) +
#    geom_smooth(method='lm') +
#    labs(x='Charles',y='Mr',title='Corr(Charles,Mr) = 0.0828.\nLinear regression R^2 = 0.00686')

data3Transformed = (t(ev[[2]]))[,1:nrow(ev[[2]])] %*% t(data3)
dataPC6 = data.frame(n=1:1309,Survived=data$Survived,t(data3Transformed)[,1:6])
colnames(dataPC6) <- c('PassengerId','Survived',paste('PC',1:6,' wgt',sep=''))
rownames(dataPC6) <- 1:1309
head(data[,c('PassengerId','Name','Sex','Age')])
cat('First 6 principal component weights for the first 6 passengers:\n')
head(dataPC6[,-2])

library(caret)
accuracy <- matrix(nrow=5,ncol=5)
rownames(accuracy) <- paste('d=',3:7,sep='')
colnames(accuracy) <- paste('k=',c(7,9,11,13,15),sep='')
for (d in 1:5){
    xt = (t(ev[[2]]))[1:(d+2),1:nrow(ev[[2]])] %*% t(data3)
    dataPC = data.frame(Survived=data$Survived,t(xt))
    for (k in 1:5){
        trials = 100
        total = 0
        for (i in 1:trials){
            s = sample(1:891,802)
            s2 = (1:891)[-s]
            model <- knn3(factor(Survived) ~ .,dataPC[s,],k=2*k+5)
            p <- predict(model,newdata=dataPC[s2,])
            p <- ifelse(p[,2]>=0.5,1,0)
            # calculate one minus misclassification rate
            x = 1-sum(abs(dataPC$Survived[s2]-p))/length(s2)
            #if (i%%10==0) cat(sprintf("Trial %d has CV accuracy %f\n",i,x))
            total = total + x
    }
        #cat(sprintf("For d=%d, k=%d, average CV accuracy of %d trials is %f\n"
        #    ,d+2,2*k+5,trials,total/trials))
    accuracy[d,k] <- total/trials
    }
}
cat('Cross validation accuracy using 10-fold CV:\n')
accuracy

d=5; k=11;
xt = (t(ev[[2]]))[1:d,1:nrow(ev[[2]])] %*% t(data3)
dataPC = data.frame(Survived=data$Survived,t(xt))
model <- knn3(factor(Survived) ~ .,dataPC[1:891,],k=k)
p <- predict(model,newdata=dataPC[892:1309,])
p <- ifelse(p[,2]>=0.5,1,0)
submit = data.frame(PassengerId=892:1309,Survived=p)
write.csv(submit,'PCA5kNN11.csv',row.names=F)

pimage <- function(x){
    return(ggplot(data=ev2) + 
           geom_line(aes(x=1:length(ev2[[x+1]]),y=ev2[[x+1]])) + 
           labs(x='',y='',title=paste('PC',x,sep='')))
}
x=0
grid.arrange(pimage(x+1),pimage(x+2),pimage(x+3),pimage(x+4),pimage(x+5),pimage(x+6),
    pimage(x+7),pimage(x+8),pimage(x+9),pimage(x+10),pimage(x+11),pimage(x+12),as.table=F)

x=99
grid.arrange(pimage(x+1),pimage(x+2),pimage(x+3),pimage(x+4),pimage(x+5),pimage(x+6),
    pimage(x+7),pimage(x+8),pimage(x+9),pimage(x+10),pimage(x+11),pimage(x+12),as.table=F)

prep <- function(data,s){
    x=c(); y=c(); z=c()
    for (i in 1:nrow(data))
    for (j in 1:ncol(data)){
        x=c(x,s*j)
        y=c(y,s*i)
        z=c(z,data[i,j])
    }
    return (data.frame(x=x,y=y,z=z))
}

x=diag(nrow(data5))
g1 = ggplot(prep( x[4*1:100,4*1:100] ,4),aes(x,y)) +
 geom_raster(aes(fill=z)) +
 ylim(400,0) +
 labs(title='Words in dictionary',x='',y='')
d = 5
x=diag(nrow(data5))
xt = (t(ev[[2]]))[1:d,1:nrow(ev[[2]])] %*% x
x2 = ev[[2]][1:nrow(ev[[2]]),1:d] %*% xt
g2 = ggplot(prep( x2[1:10,1:10] ,1),aes(x,y)) +
 geom_raster(aes(fill=z)) +
 ylim(10,0) +
 labs(title='Words recreated from 5 PC',x='',y='')
d=10
xt = (t(ev[[2]]))[1:d,1:nrow(ev[[2]])] %*% x
x2 = ev[[2]][1:nrow(ev[[2]]),1:d] %*% xt
g3 = ggplot(prep( x2[1:20,1:20] ,1),aes(x,y)) +
 geom_raster(aes(fill=z)) +
 ylim(20,0) +
 labs(title='Words recreated from 10 PC',x='',y='')
d=25
xt = (t(ev[[2]]))[1:d,1:nrow(ev[[2]])] %*% x
x2 = ev[[2]][1:nrow(ev[[2]]),1:d] %*% xt
g4 = ggplot(prep( x2[1:50,1:50] ,1),aes(x,y)) +
 geom_raster(aes(fill=z)) +
 ylim(50,0) +
 labs(title='Words recreated from 25 PC',x='',y='')
grid.arrange(g1,g2,g3,g4,nrow=2,ncol=2)

d=50
xt = (t(ev[[2]]))[1:d,1:nrow(ev[[2]])] %*% x
x2 = ev[[2]][1:nrow(ev[[2]]),1:d] %*% xt
g5 = ggplot(prep( x2[1:100,1:100] ,1),aes(x,y)) +
 geom_raster(aes(fill=z)) +
 ylim(100,0) +
 labs(title='Words recreated from 50 PC',x='',y='')
d=100
xt = (t(ev[[2]]))[1:d,1:nrow(ev[[2]])] %*% x
x2 = ev[[2]][1:nrow(ev[[2]]),1:d] %*% xt
g6 = ggplot(prep( x2[2*1:100,2*1:100] ,2),aes(x,y)) +
 geom_raster(aes(fill=z)) +
 ylim(200,0) +
 labs(title='Words recreated from 100 PC',x='',y='')
d=200
xt = (t(ev[[2]]))[1:d,1:nrow(ev[[2]])] %*% x
x2 = ev[[2]][1:nrow(ev[[2]]),1:d] %*% xt
g7 = ggplot(prep( x2[4*1:100,4*1:100] ,4),aes(x,y)) +
 geom_raster(aes(fill=z)) +
 ylim(400,0) +
 labs(title='Words recreated from 200 PC',x='',y='')
d=300
xt = (t(ev[[2]]))[1:d,1:nrow(ev[[2]])] %*% x
x2 = ev[[2]][1:nrow(ev[[2]]),1:d] %*% xt
g8 = ggplot(prep( x2[4*1:100,4*1:100] ,4),aes(x,y)) +
 geom_raster(aes(fill=z)) +
 ylim(400,0) +
 labs(title='Words recreated from 300 PC',x='',y='')
grid.arrange(g5,g6,g7,g8,nrow=2,ncol=2)

# returns mode of a set of numbers
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

# calculate average (mode) position of each word
Position <- vector("list",length=nrow(dictionary))
for (i in 1:nrow(data)){
    n = gsub('[.,"()/]','',data$Name[i])
    n = gsub(' . |  ',' ',n)
    n = strsplit(n,' ')[[1]]
    for (j in 1:length(n)){
        k = which(dictionary$Words==n[j])[1]
        if (!is.na(k)) Position[[k]] = c(Position[[k]],j)
    }
}
dictionary$Position <- NULL
for (i in 1:nrow(dictionary)) dictionary$Position[i] <- getmode(Position[[i]])
dictionary$Position <- pmin(4,dictionary$Position)

ggplot(data=dictionary) +
geom_jitter(height=0.005,aes(x=1:nrow(dictionary),y=dictionary$Survival,color=factor(dictionary$Position))) +
scale_colour_manual(values = c("red", "black", "blue","white"),labels=c('1','2','3','4+')) +
geom_hline(yintercept=0.384, linetype='dotted') +
geom_vline(xintercept=45) +
geom_vline(xintercept=57) +
geom_vline(xintercept=80) +
geom_vline(xintercept=115) +
geom_vline(xintercept=187) +
geom_vline(xintercept=6) +
annotate('text',x=-15,y=0.87,label='n<=6\nfreq\n>=35') +
annotate('text',x=98,y=0.87,label='freq\n>=4',color='gray70') +
#annotate('text',x=68,y=0.87,label='freq\n>=5',color='gray70') +
annotate('text',x=25,y=0.87,label='freq\n>=7',color='gray70') +
annotate('text',x=143,y=0.87,label='n>=116\nfreq<=3') +
annotate('text',x=215,y=0.87,label='n>=188\nfreq<=2') +
annotate('text',x=305,y=0.36,label='p = 0.384 = probability of survival') +
labs(x='Words order by frequency') +
labs(title='Each dot represents a Word in the dictionary.') +
labs(y='Probability Survival given Word',color='word\'s\nposition\nin name')

# log odds
logl <- function(p){
    e = 0.001
    q = 1 - p
    return(log(max(p,e))-log(max(q,e)))
}
# inverse logit
ilogit <- function(z){
    return (1/(1+exp(-z)))
}
# cross validation trials
trials = 100
total = 0
for (k in 1:trials){
if (k%%25==0) cat(sprintf("Begin trial %d\n completed",k))
s = sample(1:891,802)
s2 = (1:891)[-s]

# calculate the Tfreq and Survival within our training subset
dictionary$Survival <- NA
dictionary$Tfreq <- 0
for (i in 1:length(dictionary$Words)){
    x = intersect(which(data2[,dictionary$Words[i]]>0),s)
    dictionary$Survival[i] <- mean(data$Survive[x],na.rm=T)
    dictionary$Tfreq[i] <- length(x)
}
dictionary$Survival[is.na(dictionary$Survival)] <- NA
dictionary$Tfreq[is.na(dictionary$Tfreq)] <- NA
dictionary$Logl <- sapply(dictionary$Survival,FUN=logl)

# calculate bias term for Oscar's naive Bayes
ps = sum(data$Survived[s])/length(s)
bias = logl(1-ps)

# the following line mimics PCA dimension reduction
dictionary2 <- dictionary[dictionary$Tfreq>=4,]
p = rep(0,891-length(s))
for (j in 1:length(s2)){
    c = 0
    slogl = 0
    # perform Oscar's naive Bayes
    for (i in 1:nrow(dictionary2)){
        if (data2[s2[j],dictionary2$Words[i]]>0 & !is.na(dictionary2$Survival[i])){
            slogl = slogl + dictionary2$Logl[i]
            if (c>0) slogl = slogl + bias
            c = 1
        }
    }
    if (c!=0) p[j] = ilogit(slogl)
    else p[j] = ps
    if (k%%25==0 & j%%10==0) cat(sprintf(" j=%d ",j))
}
p <- ifelse(p>=0.5,1,0)
# calculate one minus misclassification rate
x = 1-sum(abs(data$Survived[s2]-p))/length(s2)
if (k%%25==0) cat(sprintf("\n Trial %d has CV accuracy %f\n",k,x))
total = total + x
}

cat(sprintf("Average CV accuracy of %d trials is %f\n",trials,total/trials))

