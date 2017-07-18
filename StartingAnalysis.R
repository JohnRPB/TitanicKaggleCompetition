#############################################################################################################################
######## ---------------------------------------------------------------------------------------------------------- #########
######## -------------------------------- Titanic Machine Learning Analysis Example ------------------------------- #########
######## ---------------------------------------------------------------------------------------------------------- #########
#############################################################################################################################

setwd("~/Documents/Summer_07/Kaggle/Titanic")

#' install.packages("caret")
library("caret")
library("magrittr")
library("ggplot2")

#' install.packages("randomForest")
library("randomForest")

trainSet <- read.table("train.csv", sep = ",", header = TRUE)
testSet <- read.table("test.csv", sep = ",", header = TRUE)

head(trainSet)
head(testSet)

####### -------------------- Categorical Variables ------------------------ #######
#' DATA MINING

trainSurvived <- trainSet[trainSet$Survived == 1,]

#' good candiate
ClassTable <- trainSet[,c("Survived", "Pclass")] %>% table
colFreqTable <- prop.table(ClassTable,2) %>% as.data.frame
colFreqTable <- colFreqTable[colFreqTable$Survived == 1,]
ggplot(aes(x = Pclass, y = Freq), data = colFreqTable) + geom_col()
prop.table(ClassTable,2)
#' Yes

SexTable <- trainSet[,c("Survived", "Sex")] %>% table
colFreqTable <- prop.table(SexTable,2) %>% as.data.frame
colFreqTable <- colFreqTable[colFreqTable$Survived == 1,]
ggplot(aes(x = Sex, y = Freq), data = colFreqTable) + geom_col()
prop.table(SexTable,2)
#' Yes

SibSpTable <- trainSet[,c("Survived", "SibSp")] %>% table
colFreqTable <- prop.table(SibSpTable,2) %>% as.data.frame
colFreqTable <- colFreqTable[colFreqTable$Survived == 1,]
ggplot(aes(x = SibSp, y = Freq), data = colFreqTable) + geom_col()
prop.table(SibSpTable,2)
#' good

ParchTable <- trainSet[,c("Survived", "Parch")] %>% table
colFreqTable <- prop.table(ParchTable,2) %>% as.data.frame
colFreqTable <- colFreqTable[colFreqTable$Survived == 1,]
ggplot(aes(x = Parch, y = Freq), data = colFreqTable) + geom_col()
prop.table(ParchTable,2)
#' Probably not

EmbarkedTable <- trainSet[,c("Survived", "Embarked")] %>% table
colFreqTable <- prop.table(EmbarkedTable,2) %>% as.data.frame
colFreqTable <- colFreqTable[colFreqTable$Survived == 1,]
ggplot(aes(x = Embarked, y = Freq), data = colFreqTable) + geom_col()
prop.table(EmbarkedTable,2)
#' Definitely include

####### ------------------- Quantitative Variables ------------------------ #######
#' DATA MINING

#' install.packages("fields")
library(fields)

trainSet$Survived <- trainSet$Survived %>% factor

#' Passenger Id (for fun)
p <- ggplot(data = trainSet, aes(x = Survived, y = PassengerId))
p + geom_boxplot()
#' Obviously no effect
#' No

#' Age
p <- ggplot(data = trainSet, aes(x = Survived, y = Age))
p + geom_boxplot()
#' Lots of missing values, plus not predictive
#' No

#' Fare
p <- ggplot(data = trainSet, aes(x = Survived, y = Fare))
p + geom_boxplot()
#' Somewhat
#' Yes

#' Cabin
trainSet$Cabin %>% levels
# ' Only kind of quantitative, but could be interesting to dissect later





