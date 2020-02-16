#load raw data
train <- read.csv("train.csv", header= TRUE)
test <- read.csv("test.csv" , header=TRUE)

train <- data.frame(train[,2:11])
test <- data.frame(test[,2:10])

#Add a "survived" variable to the test set to allow for the combining data sets
test.survived <- data.frame(Survived=rep("none",nrow(test)),test[,])

#combine data sets
data.combined <- rbind(train, test.survived)

#About data type
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

str(data.combined)

#lets take alook at survival rate
table(data.combined$Survived)

#distribution across classes
table(data.combined$Pclass)

#load up ggplot2 package to use for visualization
library(ggplot2)

# hypothesis: rich survived at the higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train,aes(Pclass,fill=factor(Survived)))+
  geom_bar(width=0.5)+
  xlab("pclass")+
  ylab("total count")+
  labs(fill="survived")

#examine the first few names in the training data set
head(as.character(train$Name))

#how many unique names are they across both test and tarin
length(unique(as.character(data.combined$Name)))


#get duplicate names and store them as a vector
dup.names <- as.character(data.combined$Name)[which(duplicated((as.character(data.combined$Name))))]
#xu=as.character(data.combined$Name)[duplicated((as.character(data.combined$Name)))]

#take a look at the records in the combined data set
data.combined[(data.combined$Name %in% dup.names),]

#what is with "Miss." and "Mr." thing?
library(stringr)

#Any correlation with other variables (e.g. sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

#Any correlation with other variables (e.g. sibsp)?
mrses <- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,]

#Check out males to see if there is a pattern
males <- data.combined[which(data.combined$Sex=="male"),]
males[1:5,]

#Expand upon the relationship between "survived" and "Pclass" by addinf the new "title" 
#variable to the data set and explore 3-d relationship

# create a utility function to help with title extraction
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.",name))>0){
  return("Miss.")
  }else if (length(grep("Master.",name))>0){
    return("Master.")
  }else if (length(grep("Mrs.",name))>0){
    return("Mrs.")
  }else if (length(grep("Mr.",name))>0){
    return("Mr.")
  }else {
    return("none")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)){
  titles=c(titles,extractTitle(data.combined$Name[i]))
 }
data.combined$Title <- as.factor(titles)
  

# Since we only have survived labels for the train set, only use the first 891 rows
ggplot(data.combined[1:891,],aes(x=Title,fill=factor(Survived)))+
  geom_bar(stat="count")+
  facet_wrap(~Pclass)+
  ggtitle("pclass")+
  xlab("title")+
  ylab("total count")+
  labs(fill="survived")


#what is the distribution of female and male?
table(data.combined$Sex)

#visualize 3 ways relationship of sex, p-class, and survival
ggplot(data.combined[1:891,],aes(x=Sex,fill=factor(Survived)))+
  geom_bar(width=0.5)+
  facet_wrap(~Pclass)+
  ggtitle("pclass")+
  xlab("sex")+
  ylab("total count")+
  labs(fill="survived")

#let's take a look on age distrbution 
summary(data.combined$Age)
summary(data.combined$Age[1:891])


#Survival rate broken out by sex, pclass, age
ggplot(data.combined[1:891,],aes(x=Age,fill=factor(Survived)))+
  geom_histogram(binwidth=10)+
  facet_wrap(~Sex + Pclass)+
  ggtitle("pclass")+
  xlab("Age")+
  ylab("total count")+
  labs(fill="survived")

#validate master is a good proxy for children
boys <- data.combined$Age[data.combined$Title=="Master."] 
summary(boys)

#what about Miss
summary(misses$Age)

ggplot(misses[misses$Survived !="none",],aes(x=Age,fill=factor(Survived)))+
  geom_bar(width=10)+
  facet_wrap(~Pclass)+
  ggtitle("Age for Misses by P-class")+
  xlab("Age")+
  ylab("total count")+
  labs(fill="survived")


#Misses alone
misses.alone <- misses[which(misses$SibSp==0 & misses$Parch==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age<=14.5))

#Sibsp variable
summary(data.combined$SibSp)

length(unique(data.combined$SibSp))
data.combined$SibSp <- as.factor(data.combined$SibSp)


#Visualizae Sibsp, pclass and title
ggplot(data.combined[1:891,],aes(x=SibSp,fill=factor(Survived)))+
  geom_bar(width=1)+
  facet_wrap(~Pclass+Title)+
  ggtitle("pclass,title")+
  xlab("Sibsp")+
  ylab("total count")+
  labs(fill="survived")

#Parch variable
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,],aes(x=Parch,fill=factor(Survived)))+
  geom_bar(width=1)+
  facet_wrap(~Pclass+Title)+
  ggtitle("pclass,title")+
  xlab("Parch")+
  ylab("total count")+
  labs(fill="survived")


#creating family size variable
#data.combined$Family.size=(as.integer(data.combined$SibSp)+as.integer(data.combined$Parch)+1)
data.combined$Family.size=as.factor(as.integer(data.combined$SibSp)-1+as.integer(data.combined$Parch)-1+1)


ggplot(data.combined[1:891,],aes(x=Family.size,fill=factor(Survived)))+
  geom_bar(width=1)+
  facet_wrap(~Pclass+Title)+
  ggtitle("pclass,title")+
  xlab("Family size")+
  ylab("total count")+
  labs(fill="survived")


#take a look at ticket variable
str(data.combined$Ticket)

#based on huge number of levels is not a factor variable it is a string
#convert and display fist 20
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

#let's take alook at just the first char of each
Ticket.first.char <- ifelse(data.combined$Ticket=="", " ",substr(data.combined$Ticket,1,1))
unique(Ticket.first.char)

#make first char of ticket a factor
data.combined$Ticket.first.char <- as.factor(Ticket.first.char)


ggplot(data.combined[1:891,],aes(Ticket.first.char,fill=factor(Survived)))+
  geom_bar(width=0.5)+
  xlab("ticketfirstchar")+
  ylab("total count")+
  labs(fill="survived")

ggplot(data.combined[1:891,],aes(Ticket.first.char,fill=factor(Survived)))+
  geom_bar(width=1)+
  facet_wrap(~Pclass)+
  ggtitle("pclass")+
  xlab("ticketfirstchar")+
  ylab("total count")+
  labs(fill="survived")



ggplot(data.combined[1:891,],aes(Ticket.first.char,fill=factor(Survived)))+
  geom_bar(width=1)+
  facet_wrap(~Pclass+Title)+
  ggtitle("pclass,title")+
  xlab("ticketfirstchar")+
  ylab("total count")+
  labs(fill="survived")


#fare
summary(data.combined$Fare)
length(unique(data.combined$Fare))


ggplot(data.combined,aes(x=Fare))+
  geom_histogram(binwidth=5)+
  xlab("fare")+
  ylab("total count")+
  ggtitle("fare distribution")
    #ylim(0,200)

ggplot(data.combined[1:891,],aes(Fare,fill=factor(Survived)))+
  geom_histogram(binwidth=5)+
  facet_wrap(~Pclass+Title)+
  ggtitle("pclass,title")+
  xlab("fare")+
  ylab("total count")+
  labs(fill="survived")

#Cabin
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

#Replace the empty cabin with
data.combined$Cabin[which (data.combined$Cabin=="")] <- "U"
data.combined$Cabin[1:100]

#Take a look at just first cahracter
Cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
unique(Cabin.first.char)
levels(Cabin.first.char)

data.combined$Cabin.first.char <- Cabin.first.char

ggplot(data.combined[1:891,],aes(Cabin.first.char,fill=factor(Survived)))+
  geom_bar(width=0.5)+
  xlab("cabinfirstchar")+
  ylab("total count")+
  labs(fill="survived")

ggplot(data.combined[1:891,],aes(Cabin.first.char,fill=factor(Survived)))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("pclass")+
  xlab("cabinfirstchar")+
  ylab("total count")+
  labs(fill="survived")

#what about folks with multiple cabins
data.combined$multiple.cabin <- ifelse(str_detect(data.combined$Cabin," "), "Y" , "N")


ggplot(data.combined[1:891,],aes(multiple.cabin,fill=factor(Survived)))+
  geom_bar(width=1)+
  facet_wrap(~Pclass+Title)+
  ggtitle("pclass,title")+
  xlab("ticketfirstchar")+
  ylab("total count")+
  labs(fill="survived")


#Explantory model

library(randomForest)
library(doSNOW)

#Train a random forest with default parameters using pclass and title
set.seed(1234)
rf.train.1 <- data.combined[1:891,c("Pclass","Title")]
rf.label <- as.factor(train$Survived)

rf.1 <- randomForest(x=rf.train.1,y=rf.label,importance=TRUE, ntree=1000)
rf.1
varImpPlot(rf.1)


#Train a random forest with default parameters using pclass and title and sblsp
rf.train.2 <- data.combined[1:891,c("Pclass","Title","SibSp")]


rf.2 <- randomForest(x=rf.train.2,y=rf.label,importance=TRUE, ntree=1000)
rf.2
varImpPlot(rf.2)


#Train a random forest with default parameters using pclass and title and parch
rf.train.3 <- data.combined[1:891,c("Pclass","Title","Parch")]


rf.3 <- randomForest(x=rf.train.3,y=rf.label,importance=TRUE, ntree=1000)
rf.3
varImpPlot(rf.3)


#Train a random forest with default parameters using pclass and title and sblsp and Parch
rf.train.4 <- data.combined[1:891,c("Pclass","Title","SibSp","Parch")]


rf.4 <- randomForest(x=rf.train.4,y=rf.label,importance=TRUE, ntree=1000)
rf.4
varImpPlot(rf.4)


#Train a random forest with default parameters using pclass and title and familysize
set.seed(1234)
rf.train.5 <- data.combined[1:891,c("Pclass","Title","Family.size")]


rf.5 <- randomForest(x=rf.train.5,y=rf.label,importance=TRUE, ntree=1000)
rf.5
varImpPlot(rf.5)

#estimate error on test set (to see if our data are overfitted)
#subset our test records and features
test.submit.df <- data.combined[892:nrow(data.combined),c("Pclass","Title","Family.size")]

#make prediction
rf.5.predict <- predict(rf.5,test.submit.df )
str(rf.5.predict)
table(rf.5.predict)

#write out a csv file
submit.df <- data.frame(PassengerID=c(892:nrow(data.combined)),Survived=rf.5.predict)
write.csv(submit.df, file="asghar.csv", row.names=FALSE)

#our submission=0.794 besscore of training sets 0.81
# Lets use cross validation using the caret packageto get more accurate estimate


library(caret)
#we use 10 fold cross validation
#we need 100 total fold
set.seed(2348)
cv.10.fold <- createMultiFolds(rf.label,k=10,times=10)

#check startification
table(rf.label)
asghar <- length (rf.label[which(rf.label==1)])/length(rf.label[which(rf.label==0)])
asghar

table(rf.label[cv.10.fold[[33]]])
table(rf.label[cv.10.fold$Fold05.Rep03])

# set up caret's train control object per above
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = NULL, indexOut = cv.10.fold)
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = NULL, indexOut = cv.10.fold)
#Set up do anow package for multi core training
cl <- makeCluster(2,type="SOCK")
registerDoSNOW(cl)

#Set seed for reproducibility and train
#set.seed(34324)
#rf.5.cv.1 <- train(x=rf.train.5, y=rf.label, method="rf",tuneLength=2, ntree=1000, trControl=ctrl.1 )

#shut dowt cluster
stopCluster(cl)

#check out results
rf.5.cv.1

#Use single decision tree instead of random forestcart
library(rpart)
library(rpart.plot)


#create utility function 3-fold 10 times

rpart.cv <- function(seed,training,labels,ctrl){
  cl <- makeCluster(2,type="SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  rpart.cv <- train(x=training, y=labels, method="rpart",tuneLength=30, trControl=ctrl)
  stopCluster(cl)
  return(rpart.cv)
  
}

#grab feature
features<-c("Title","Pclass","Family.size")
rpart.train.1 <- data.combined[1:891,features]

#run Cv and check out results
rpart.1.cv.1 <- rpart.cv(94622,rpart.train.1,rf.label,ctrl.3)
rpart.1.cv.1
prp(rpart.1.cv.1$finalModel, type=0, extra=1, under=TRUE)

#parse out last name and title
name.splits <- str_split(data.combined$Name,",")
name.splits[[1]]
last.names<- sapply(name.splits,"[",1)
last.names[1:10]
data.combined$last.names <- last.names

#titles
name.splits.2 <- str_split(sapply(name.splits,"[",2)," ")
titles <- sapply(name.splits.2,"[",2)
unique(titles)

#re-map titles to be more excat
titles[which(titles %in% c("Dona.","the","Lady."))]<-"Mrs."
titles[which(titles %in% c("Ms.","Mlle."))]<-"Miss."
titles[which(titles=="Mme.")]<-"Mrs."
titles[which(titles %in% c("Don.","Jonkheer.","Rev.","Sir.","Dr."))]<-"Mr."
titles[which(titles %in% c("Capt.","Col.","Major."))]<-"Mr."
table(titles)

data.combined$new.titles <- as.factor(titles)

#grab feature
features<-c("new.titles","Pclass","Family.size")
rpart.train.2 <- data.combined[1:891,features]

#run Cv and check out results
rpart.2.cv.1 <- rpart.cv(94622,rpart.train.2,rf.label,ctrl.3)
rpart.2.cv.1
prp(rpart.2.cv.1$finalModel, type=0, extra=1, under=TRUE)

#Dive in on first class Mr.
first.mr <- which(data.combined$new.titles=="Mr." & data.combined$Pclass=="1")
first.mr.df <- data.combined[first.mr,]
summary(first.mr.df)

#One female
first.mr.df[first.mr.df$Sex=="female",]

#khatar<-rep("none",1309)
#khatar[which(data.combined$Sex=="female" | (data.combined$Sex=="male" & data.combined$new.titles=="Master."))]<-"zanobache"  
#khatar[which(data.combined$Sex=="male" & data.combined$new.titles=="Mr.")] <- "mard"
#abdol<-which(data.combined$Sex=="male" & data.combined$new.titles=="Mr.")
#data.combined$khatar<-as.factor(khatar)


#grab feature
#features<-c("khatar","Pclass","Family.size")
#rpart.train.3 <- data.combined[1:891,features]

#run Cv and check out results
#rpart.3.cv.1 <- rpart.cv(94622,rpart.train.3,rf.label,ctrl.3)
#rpart.3.cv.1
#prp(rpart.3.cv.1$finalModel, type=0, extra=1, under=TRUE)

data.combined$new.titles[which(data.combined$Sex=="female" & data.combined$new.titles=="Mr.")]<-"Mrs."  
first.mr <- which(data.combined$new.titles=="Mr." & data.combined$Pclass=="1")
first.mr.df <- data.combined[first.mr,]


#Let's take a look at 1st class Mr.
summary(first.mr.df[first.mr.df$Survived==1,])


#Visualize survival rates for first class Mr. by fare
ggplot(first.mr.df,aes(x=Fare,fill=Survived))+
         geom_density(alpha=0.5)+
         ggtitle("asghar")
         
#engineer feature about multiple passenger traveling with the same ticket
ticket.party.size <- rep(0,nrow(data.combined))
ave.fare <- rep(0.0,nrow(data.combined))

tickets <- data.combined$Ticket

#n=0
for (i in 1:nrow(data.combined)){
 fare.1 <- data.combined$Fare[which(data.combined$Ticket==tickets[i])]
 #print(fare.1)
 #n=n+1
 #print(n)
 ticket.party.size[i] <- length(fare.1)
 ave.fare[i] <- fare.1[1]/length(fare.1)
}
data.combined$ticket.family.size <- ticket.party.size
data.combined$ave.fare <- ave.fare

#Refresh 1st class Mr. dataframe

first.mr.df <- data.combined[first.mr,]
summary(first.mr.df)

#Hypothesis: ticket party size is highly correlated with av. fare
summary(data.combined$ave.fare)

#one missing
data.combined[is.na(data.combined$ave.fare),]

#Get records for similar passengers
similar.na.passanger <- data.combined[with(data.combined,which(Pclass=="1" & Title=="Mr." & Family.size=="1", Ticket!="3701")),]
summary(similar.na.passanger$ave.fare)

#Use median since its close to mean and a little higher than mean
data.combined$ave.fare[which(is.na(data.combined$ave.fare))]<-median(similar.na.passanger$ave.fare)
#median(similar.na.passanger$ave.fare)

#Leverage Caret's preprocess function to normalize data
preproc.data.combined <- data.combined[,c("ticket.family.size","ave.fare")]
preProc <- preProcess(preproc.data.combined , method = c("center", "scale"))

postpros.data.combined <- predict(preProc,preproc.data.combined)

#Hopothesis refused for all data
cor(postpros.data.combined$ticket.family.size,postpros.data.combined$ave.fare)

#How about just for 1st class
indexes <- which(data.combined$Pclass=="1")
cor(postpros.data.combined$ticket.family.size[indexes],postpros.data.combined$ave.fare[indexes])

#grab feature
features<-c("new.titles","Pclass","Family.size","ticket.family.size","ave.fare")
rpart.train.4 <- data.combined[1:891,features]

#run Cv and check out results
rpart.4.cv.1 <- rpart.cv(94622,rpart.train.4,rf.label,ctrl.3)
rpart.4.cv.1
prp(rpart.4.cv.1$finalModel, type=0, extra=1, under=TRUE)