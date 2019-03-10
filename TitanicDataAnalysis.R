#load raw data
test <- read.csv("test.csv", header=TRUE)
train <- read.csv("train.csv", header=TRUE)

#add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

#swap the first two colummns in train dataframe to match test.survived dataframe
test.survived <- test.survived[c(2,1,3,4,5,6,7,8,9,10,11,12)]

#combined data sets
data.combined  <- rbind(train, test.survived)

#R data types (eg: factors,...)
str(data.combined)
#NA is null, <- is =
#machine learning algorithm doesn't like chr, string

data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

#take a look at distribution of survival rates, classes
table(data.combined$Survived)
table(data.combined$Pclass)

#load up ggplot2 package for use of visualization
library(ggplot2)

#hypothesis = rich folks survived at higher rate
train$Pclass <- as.factor(train$Pclass)

str(train)

#create plot with x and fill with survived (converted from int to factor)
ggplot(train, aes(x=Pclass, fill = factor(Survived))) +
  geom_bar(width=0.5) +
  xlab("Pclass")+
  ylab("Total count")+
  labs(fill="Survived")

#examine the first few names in the training data frame
head(as.character(train$Name))

#how many unique names are there across test and train data frame?
length(unique(as.character(data.combined$Name)))

#detect duplicate names
#first, get duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
#second, take a look at records in data.combined 
data.combined[which(data.combined$Name %in% dup.names),]

#what is up with the "Miss.", and "Mr." thing?
library(stringr)


#look up records with name "Mr. Jack"
data.combined[which(str_detect(data.combined$Name, "Mr.Jack")),]

#[row,column]; if all [,]
#any correlation between "Miss.", "Mr." and other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

#hypothesis = name titles correlate with ages
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]
misters <- data.combined[which(train$Sex == "male"),]
misters[1:5,]
 
rm(title)
rm(titles)

#expand the rlts between "Survived" and "Pclass" by adding the new "Title" variable to the data frame
#then explore a potentail 3-way rlts

#create a utility function to help with little extraction
extractTitle <- function(Name) {
  Name <- as.character(Name)
  if (length(grep("Miss.", Name)) >0) {return("Miss.")}
  else if (length(grep("Master.", Name)) >0) {return("Master.")}
  else if (length(grep("Mrs.", Name)) >0) {return("Mrs.")}
  else if (length(grep("Mr.", Name)) >0) {return("Mr.")}
  else {return("Other")}
}

titles <- NULL 
for(i in 1:nrow(data.combined)) {
  titles <-c(titles,extractTitle(data.combined[i,"Name"]))
}
data.combined$Title <- as.factor(titles)

#use of visualization for 3-way plot "Survived", "Pclass", and "Title"
#we need "Survived" in train set, only use the first 891 rows
ggplot(data.combined[1:891,], aes(x=Title, fill= Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total count")+
  labs(fill="Survived")

#binwidth is an argument of geom_histogram, which is used for continuous variable
#width is an argument of geom_bar, which is used for discrete variable

#what's the distribution of females to males across train & test
table(data.combined$Sex)

#visualize 3-way rlts of sex, pclass, and survived, compare to analysis
ggplot(data.combined[1:891,], aes(x=Sex, fill=Survived)) +
  geom_bar(binwidth = 10)+
  facet_wrap(~Pclass) +
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total count")+
  labs(fill="Survived")

#what's the distribution of age across train & test
summary(data.combined$Age)

#what's the distribution of age across train (set wiwth Survived data)
summary(data.combined[1:891,"Age"])

#visualize it!
ggplot(data.combined[1:891,], aes(x=Age, fill=Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth=10,center=5)+
  xlab("Age")+
  ylab("Total count")

#to deal with N/A or missing value: ignore, use mean/median value, proxy, or imputation
#proxy is find some value that mimics the missing values
#computation is creating a predictive model from the known data

#validate that "Master" is a good proxy for male children
boys <- data.combined[which(data.combined$Title=="Master."),]
summary(boys$Age)

men <- data.combined[which(data.combined$Title=="Mr."),]
summary(men$Age)

misses <- data.combined[which(data.combined$Title=="Miss."),]
summary(misses$Age)
#misses are mostly adult females, not children females

ggplot(misses[misses$Survived != "None",], aes(x=Age, fill=Survived))+
  facet_wrap(~Pclass)+
  geom_histogram(binwidth=5)
  ggtitle("Age for 'Miss.' by Pclass")+
  xlab("Age")+
  ylab("Total count")
  
#appears that female children have different survival rate, potential for feature engineering
#look at the group of misses traveling alone with the max age = max age of master
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch ==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <=14.5))

summary(data.combined$SibSp)

#number of unique data for SibSp variable
length(unique(data.combined$SibSp))

#treat variable as a factor
data.combined$SibSp <- as.factor(data.combined$SibSp)

#visualiza rlts between Survived, Sibsp, Pclass, and Title
ggplot(data.combined[1:891,], aes(x=SibSp, fill=Survived))+
  facet_wrap(~Pclass + Title)+
  geom_bar(width = 1)+
  ggtitle("Pclass, Title")+
  xlab("SibSp")+
  ylab("Total count")+
  ylim(0,300)+
  labs(fill="Survived")

data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x=Parch, fill=Survived))+
  facet_wrap(~Pclass + Title)+
  geom_bar(width = 1)+
  ggtitle("Pclass, Title")+
  xlab("Parch")+
  ylab("Total count")+
  ylim(0,300)+
  labs(fill="Survived")

#try out feature engineering by creating family size feature
famSS <-c(train$SibSp,test$SibSp)
famPC <-c(train$Parch,test$Parch)
data.combined$family.size <- as.factor(famSS + famPC +1)
ggplot(data.combined[1:891,], aes(x=family.size, fill=Survived))+
  facet_wrap(~Pclass + Title)+
  geom_bar(width = 1)+
  ggtitle("Pclass, Title")+
  xlab("Family size")+
  ylab("Total count")+
  ylim(0,300)+
  labs(fill="Survived")

#take a look at ticket variable
str(data.combined$Ticket)

#929 levels of Ticket data isn't likely a factor, so we convert
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

#seem like there's no apparent structure in the Ticket data
#let's find out (use substr to pull out 1st character)
Ticket.first.char <-ifelse(data.combined$Ticket==""," ",substr(data.combined$Ticket,1,1))
unique(Ticket.first.char)

#convert Ticket.first.char from character to factor
data.combined$Ticket.first.char <- as.factor(Ticket.first.char)

#visualize rlts between Ticket.first.char and Survived
ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill=Survived))+
  geom_bar()+
  ggtitle("Survivability by Ticket first character")+
  xlab("Ticket first character")+
  ylab("Total count")+
  ylim(0,350)+
  labs(fill="Survived")

#visualize 3-way rlts between Ticket.first.char, pclass and Survived
ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill=Survived))+
  geom_bar()+
  ggtitle("Pclass")+
  xlab("Ticket first character")+
  ylab("Total count")+
  ylim(0,150)+
  labs(fill="Survived")+
  facet_wrap(~Pclass)

#visualize 4-way rlts between Ticket.first.char, Title, pclass and Survived
ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill=Survived))+
  geom_bar()+
  ggtitle("Pclass, Title")+
  xlab("Ticket first character")+
  ylab("Total count")+
  ylim(0,200)+
  labs(fill="Survived")+
  facet_wrap(~Pclass+ Title)

#look at fare variable
summary(data.combined$Fare)
length(unique(data.combined$Fare))

rm(binwidth)

ggplot(data.combined, aes(x=Fare))+
  geom_bar(binwidth = 5)+
  ggtitle("Fare distribution")+
  xlab("Fare")+
  ylab("Total count")+
  ylim(0,200)

ggplot(data.combined[1:891,], aes(x=Fare, fill=Survived))+
  geom_histogram(binwidth = 5)+
  ggtitle("Pclass, Title")+
  xlab("Fare")+
  ylab("Total count")+
  ylim(0,50)+
  labs(fill="Survived")+
  facet_wrap(~Pclass+ Title)

#look at cabin variable
str(data.combined$Cabin)
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]
unique(data.combined$Cabin)

data.combined[which(data.combined$Cabin ==""), "Cabin"] <-"U"
data.combined$Cabin[1:100]

#look at first character of cabin
Cabin.first.char <- as.character(substr(data.combined$Cabin,1,1))
str(Cabin.first.char)
levels(Cabin.first.char)
data.combined$Cain.first.char <-Cabin.first.char

#look at rtls between cabin and survived
ggplot(data.combined[1:891,], aes(x=Cain.first.char, fill=Survived))+
  geom_bar()+
  ggtitle("Survivability by cabin first char")+
  xlab("Cabin first character")+
  ylab("Total count")+
  ylim(0,750)+
  labs(fill="Survived")
#rlts between cabin, pclass, survived
ggplot(data.combined[1:891,], aes(x=Cain.first.char, fill=Survived))+
  geom_bar()+
  ggtitle("Pclass")+
  xlab("Cabin first character")+
  ylab("Total count")+
  ylim(0,500)+
  labs(fill="Survived")+
facet_wrap(~Pclass)
#rlts between cabin, pclass, title, survived
ggplot(data.combined[1:891,], aes(x=Cain.first.char, fill=Survived))+
  geom_bar()+
  ggtitle("Pclass, Title")+
  xlab("Cabin first character")+
  ylab("Total count")+
  ylim(0,20)+
  labs(fill="Survived")+
  facet_wrap(~Pclass+Title)

#what about folks w multiple cabins?
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin," "),"y","n"))
ggplot(data.combined[1:891,], aes(x=cabin.multiple, fill=Survived))+
  geom_bar()+
  ggtitle("Pclass, Title")+
  xlab("Cabin multiple")+
  ylab("Total count")+
  ylim(0,350)+
  labs(fill="Survived")+
  facet_wrap(~Pclass+Title)

#does chance to survive depend on where you board on titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)
ggplot(data.combined[1:891,], aes(x=Embarked, fill=Survived))+
  geom_bar()+
  ggtitle("Pclass, Title")+
  xlab("Embart")+
  ylab("Total count")+
  ylim(0,350)+
  labs(fill="Survived")+
  facet_wrap(~Pclass+Title)

#connect to GitHub repository
#can I?
#let see if it works

#please work