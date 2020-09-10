
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
Data1 <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

Data2 <- Data1[,-4]
#Take a look at the summary statistics of Data 

summary(Data2)

#Converting Gender variable to numeric
Gen <- as.numeric(Data2$Sex== "female")
Data2 <- cbind(Data2,Gen)
FinData <- Data2[, c(-1,-4,-8,-10)] 

#Now Data is prepared for finding patterns in data.
# We believe that upper class had previleges.
# upper deck were first one to get help & rescue, which would have been an  effect on the event survival. Let's see

passclass<- as.factor(Data2$Pclass)
summary(passclass[Data2$Survived==1])
summary(passclass)
persur <- summary(passclass[Data2$Survived==1])*100/summary(passclass)

barplot(persur, Main =" % of survivors Travel class-wise", las=1, ylab = "% of survivor", xlab = "Travel Class", legend.text=c("1st Class","2nd Class", "3rd Class"),
col = c(1,2,3))
box()
# It is clear that 1st class were given preferance over second class & third class
#traveller.
#Similarly person travelling with children must have been given preferance to
#light the sinking ship prior.

Parchild<- as.factor(Data2$Parch)
cbind(Parchild,Data2$Survived)
summary(Parchild[Data2$Survived==1])
perchi <- summary(Parchild[Data2$Survived==1])*100/summary(Parchild)
barplot(perchi, main = "Percent survival vs no.of co-travelers",xlab = "No. of Travelers",
        ylab = "% survival", las=1, density = 1000, col = c("Dark Blue","Blue", "Light Blue","Green","Red","Yellow"))
box()

temp<- table(Data2$Survived, Data2$Parch)

barplot(temp, beside = TRUE, legend.text = c("Traveller","Survivor"), col = c(3,5), las=1, ylab = "No. of Passenger", xlab = "No. of Co-Passenger")
box()

# We can easily deduce that people who were travelling with less nos. of family members
# had high chance of surviving
# further decoding the fact of co-travellers, Lets group travellers into 2 groups
# 1.Travelers travelling alone
# 2.Travelers with  1 or more copassengers check survival statistics

summary(Parchild)
summary(Parchild[Data2$Survived==1])
lonwolf <- (233)*100/(678)
fampac <- (65+40+3+0+1+0)*100/ (118+80+5+4+5+1)
sur<- cbind(lonwolf,fampac)
barplot(sur, width = 10, main = "Survivors stats travelling alone vs w/family",
        names.arg = c("Lone Traveller","Family Traveller"), space = 0.4, beside = FALSE,
         ylab = "% Survival", las=1, col = c("Blue", "Red"), axisnames = TRUE)
box()
# Oh, We can see people travelling with families were having high % on survivals.
