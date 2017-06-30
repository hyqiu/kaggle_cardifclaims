###### Reprise de code pour Big Data

library(VIM)
library(mice)
library(rattle)
library(ggplot2)
library(rpart)
library(ROCR)
library (hmeasure)
library(splines)
library(lattice)
library(DMwR)
library(car)
library(ada)
library(rpart.plot)
library(FastImputation)
library(Amelia)
library(glmnet)
library(caret)

setwd("/Users/HenriYQ/Documents/Cours ENSAE 3A/Big Data pour l'Assurance/")
getwd()

train <- read.csv("Données/train.csv", na.strings = c("", " "))
test <- read.csv("Données/test.csv", na.strings = c("", " "))
#train.original <- train
#test.original <- test

v.names <- names(train)[3:ncol(dtrain)] #### les noms de variables

#### Recensement des types de variables

discrets <- c("v3", "v22", "v24", "v30", "v31", "v47", "v52", "v56","v66", "v71", "v74", "v75", "v79", "v91", "v107", 
              "v110","v112", "v113", "v125")

continus <- v.names[!v.names %in% c("ID", "target",discrets)]

### Now we'll check all the levels from train that have categories of less than 1 percent. And we'll set those to NA

### We know the "freaks" are v22, v56 and v125. We'll just drop v22 (too many levels !). For the other, we'll drop those who are 
### too scarce and impute them. 

p <- 0.01

lf.125 <- names(which(prop.table(table(train$v125)) < p))
length(which(prop.table(table(train$v125)) < p))
length(which(prop.table(table(train$v125)) >= p))

lf.56 <- names(which(prop.table(table(train$v56)) < p))
length(which(prop.table(table(train$v56)) < p))
length(which(prop.table(table(train$v56)) >= p))

levels(train$v125)[levels(train$v125) %in% lf.125] <- NA
levels(train$v56)[levels(train$v56) %in% lf.56] <- NA

### We'll impute the continuous variables with FastImputation (EM algorithm)

library(FastImputation)

continus.plus <- train[,!colnames(train) %in% discrets]
patterns <- TrainFastImputation(continus.plus)
imp.continus <- FastImputation(x = continus.plus, patterns = patterns, verbose = TRUE)
train.2 <- continus.plus

train.0 <- cbind(train.2[,c("ID", "target",continus)], train[, discrets])
train.0$v22 <- NULL

### Check which are the variables that have no missing value

table.complete <- sapply(train, function(x) sum(is.na(x)))

complete <- which(table.complete == 0)
complete <- complete[!names(complete) %in% c("ID")]
names.complete <- names(complete)

### Impute by random forest all the categorical variables : use all the variables that weren't imputed before

library(missForest)

ToUse <- unique(c(names.complete,discrets))
ToUse <- ToUse[-which(ToUse == "v22")]

imp.train <- missForest(train.0[,ToUse],maxiter = 5, verbose = TRUE)$ximp

################################################ Et on a notre base imputée ########################################################