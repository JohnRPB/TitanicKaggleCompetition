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

# CLASS
ClassTable <- trainSet[,c("Survived", "Pclass")] %>% table
colFreqTable <- prop.table(ClassTable,2) %>% as.data.frame
colFreqTable <- colFreqTable[colFreqTable$Survived == 1,]
ggplot(aes(x = Pclass, y = Freq), data = colFreqTable) + geom_col()
prop.table(ClassTable,2)
#' Include: yes

# SEX
SexTable <- trainSet[,c("Survived", "Sex")] %>% table
colFreqTable <- prop.table(SexTable,2) %>% as.data.frame
colFreqTable <- colFreqTable[colFreqTable$Survived == 1,]
ggplot(aes(x = Sex, y = Freq), data = colFreqTable) + geom_col()
prop.table(SexTable,2)
#' Include: yes

# SIBSP
SibSpTable <- trainSet[,c("Survived", "SibSp")] %>% table
colFreqTable <- prop.table(SibSpTable,2) %>% as.data.frame
colFreqTable <- colFreqTable[colFreqTable$Survived == 1,]
ggplot(aes(x = SibSp, y = Freq), data = colFreqTable) + geom_col()
prop.table(SibSpTable,2)
#' Include: yes

# PARCH
ParchTable <- trainSet[,c("Survived", "Parch")] %>% table
colFreqTable <- prop.table(ParchTable,2) %>% as.data.frame
colFreqTable <- colFreqTable[colFreqTable$Survived == 1,]
ggplot(aes(x = Parch, y = Freq), data = colFreqTable) + geom_col()
prop.table(ParchTable,2)
#' Inculde: probably not

# EMBARKED
EmbarkedTable <- trainSet[,c("Survived", "Embarked")] %>% table
colFreqTable <- prop.table(EmbarkedTable,2) %>% as.data.frame
colFreqTable <- colFreqTable[colFreqTable$Survived == 1,]
ggplot(aes(x = Embarked, y = Freq), data = colFreqTable) + geom_col()
prop.table(EmbarkedTable,2)
#' Include: yes

####### ------------------- Quantitative Variables ------------------------ #######
#' DATA MINING

#' install.packages("fields")
library(fields)

trainSet$Survived <- trainSet$Survived %>% factor

# ID (for fun)
p <- ggplot(data = trainSet, aes(x = Survived, y = PassengerId))
p + geom_boxplot()
#' Obviously no effect
#' Include: no

#' AGE
p <- ggplot(data = trainSet, aes(x = Survived, y = Age))
p + geom_boxplot()
#' Lots of missing values, plus not predictive
#' Include: no

#' FARE
p <- ggplot(data = trainSet, aes(x = Survived, y = Fare))
p + geom_boxplot()
#' Somewhat predictive
#' Include: no

#' CABIN
trainSet$Cabin %>% levels
#' Only kind of quantitative, but could be interesting to dissect later
#' Include: not yet

####### ---------------------- Building Model ------------------------ #######

set.seed(42)

model <- train(Survived ~ Pclass + Sex + SibSp + Embarked + Parch + Fare, 
               data = trainSet, 
               method = "rf", 
               trControl = trainControl(method = "cv", number = 5))
model


