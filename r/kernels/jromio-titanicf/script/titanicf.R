library(ggplot2) # include ggplot library                     
library(scales)  # include scales library                     
# read files

trainf <- read.csv("../input/train.csv", header=TRUE, as.is=TRUE)  
testf  <- read.csv("../input/test.csv",  header=TRUE,  as.is=TRUE) 

#testf <- read.csv("C:/Users/Jessica/Desktop/Spring 16/Math 336/Proj 1/Final/test.csv")
#trainf <- read.csv("C:/Users/Jessica/Desktop/Spring 16/Math 336/Proj 1/Final/train.csv")

lname <- function( sort ) 
{
  sortName <- strsplit( sort, ',' ) # split elements into substring family
  sortName[[1]][1]
}
#select family names
trainf$Family <- sapply(trainf$Name, lname, USE.NAMES=FALSE) 
testf$Family  <- sapply(testf$Name,  lname, USE.NAMES=FALSE)  

traindata <- data.frame(survived = trainf$Survived, # survived
                        class = trainf$Pclass,     # class
                        gender = trainf$Sex,        # gender
                        Family = trainf$Family,     # family
                        test = FALSE )              #headers neglected

testdata  <- data.frame(survived = NA,                 # no
                        class = testf$Pclass,     # class
                        gender = testf$Sex,        # gender
                        Family = testf$Family,     # family
                        test = TRUE )              #headers included

allinfo <- rbind(traindata, testdata) #combine data into vector

familymembers <- table(factor(allinfo$Family )) #last names into same family

countf <- function(familyCount)  #count family members
{ 
  familymembers[[familyCount]] 
}

allinfo$family_count <- sapply(allinfo$Family, FUN = countf )

#if family > 1, then traveling  alone
allinfo$alone <- factor(ifelse(allinfo$family_count > 1, "With Family", "Alone")) 

tempvariable <- allinfo[ !allinfo$test, ] 

#create plots
plot1 <- ggplot( tempvariable, aes(x = alone, y = survived) )  + #creates plot
  
  stat_summary(fun.y = mean,  
               geom = "bar",  # bar graph
               size = 2,      # size
               fill=muted("red")) + # 
  xlab('Travel Type') +   # x-label
  ylab('Survival Rate') + # y-label
  ggtitle("Survival Rate: Alone or with Family") # chart title

plot2 <- ggplot(tempvariable, aes(x = alone, y = survived))  + # ggplot creates plot 
  
  stat_summary(fun.y = mean, 
               geom = "bar", # bar graph
               size = 2,     # size
               fill=muted("blue")) +  
  facet_grid(class~gender) +  
  xlab( 'Travel Type') +    # x-label
  ylab( 'Survival Rate' ) + # y-label
  ggtitle("Survival Rate: Class and Gender") 

#print plots
png("travelnsurvival.png", width=500, height=800)
print(plot1)
dev.off()

png("classngender.png", width=500, height=800)
print(plot2)
dev.off()