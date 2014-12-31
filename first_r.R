#set workspace
setwd("~/R_workspace")

#read train and test file 
train <- read.csv("~/R_workspace/train.csv")
#View(train)
test <- read.csv("~/R_workspace/test.csv")
#View(test)

#install library
library(rpart)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#give test survived culome
test$Survived <- NA

#join train and test 
combi <- rbind(train, test)
#View(combi)

#convert to string
combi$Name <- as.character(combi$Name)

#split name and remove space
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

#combine small group
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# Convert to a factor
combi$Title <- factor(combi$Title)

#familysize
combi$FamilySize <- combi$SibSp + combi$Parch + 1

#family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

#delete wrong family
famIDs <- data.frame(table(combi$FamilyID))
#View(famIDs)
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

#convert to facter
combi$FamilyID <- factor(combi$FamilyID)

#assign new value
train <- combi[1:891,]
test <- combi[892:1309,]

#tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,data=train, method="class")
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

#write
write.csv(submit, file = "myseceonddtree.csv", row.names = FALSE)
