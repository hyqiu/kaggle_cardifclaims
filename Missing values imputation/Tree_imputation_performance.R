setwd("C:/Users/CHARLES/Dropbox/Big Data & Assurance/Bases de données")
setwd("~/Documents/ENSAE/3A/S2/Big data et assurance/Projet")

train = read.csv("train.csv", na.strings=c("","NA"))
test = read.csv("test.csv", na.strings=c("","NA"))


str(train[,1:60])
str(train[,61:132])

library(plyr)
library(rpart)
library(rpart.plot)


# On récupère le vecteur des noms des facteurs
Facteurs = c(3,24,30,31,47,52,56,66, 71, 74,75,79,91,107,110,112,113,125)
Facteurs_names = colnames(train[,(Facteurs+2)])
Facteurs_names

# On retraite v56 et v125 pour réduire leurs modalités
p= 0.01
lf <- names(which(prop.table(table(train$v56)) < p))
train$v56[which(train$v56 %in% lf)] = NA
train$v56 = factor(train$v56)

lf <- names(which(prop.table(table(train$v125)) < p))
train$v125[which(train$v125 %in% lf)] = NA
train$v125 = factor(train$v125)

# On supprime v22
train$v22 = NULL


# On crée un vecteur donnant le nb de NA par variable
sumna = function(x){
  y = sum(is.na(x))
  return(y)
}
NAparvar = apply(train[,3:ncol(train)], 2, sumna)
Vcomp = names(NAparvar[which(NAparvar==0)])
Vcomp
Vincomp = names(NAparvar[which(NAparvar!=0)])
Vincomp

Performances = data.frame(matrix(ncol=5, nrow=length(Vincomp)))
colnames(Performances) = c("variable", "Type", "Performance(cp=10-3)","Performance(cp=10-4)","Performance(cp=10-5)")
Performances$Type = "numeric"


for (i in 1:length(Vincomp)){
  Performances[i,1]=Vincomp[i]
  Full = train[!is.na(train[,Vincomp[i]]),c(Vcomp,Vincomp[i])]
  colnames(Full)[13]="v"
  # On crée la base Full_test, avec 25% des lignes, tirées aléatoirement
  # et la base Full_train comportant les autres lignes
  Echantillon = sample(nrow(Full), nrow(Full)/4)
  Full_test = Full[Echantillon,]
  Full_train = Full[!((1:nrow(Full)) %in% Echantillon),]
  if (Vincomp[i] %in% Facteurs_names){
     tree<-rpart( v ~ ., data=Full_train, control=rpart.control(cp=10^-3))
     prediction = predict(tree,Full_test, type="class")
     #prp(tree, type=2, extra=1)
     Performances[i,2]="factor"
     Performances[i,3] = sum(prediction==Full_test$v)/nrow(Full_test)
     tree<-rpart( v ~ ., data=Full_train, control=rpart.control(cp=10^-4))
     prediction = predict(tree,Full_test, type="class")
     Performances[i,4] = sum(prediction==Full_test$v)/nrow(Full_test)
     tree<-rpart( v ~ ., data=Full_train, control=rpart.control(cp=10^-5))
     prediction = predict(tree,Full_test, type="class")
     Performances[i,5] = sum(prediction==Full_test$v)/nrow(Full_test)
   } else {
     tree<-rpart( v ~ ., data=Full_train, control=rpart.control(cp=10^-3), method="anova")
     prediction = predict(tree,Full_test)
     reg = lm(prediction ~ Full_test$v)
     Performances[i,3] = summary(reg)$r.squared
     tree<-rpart( v ~ ., data=Full_train, control=rpart.control(cp=10^-4), method="anova")
     prediction = predict(tree,Full_test)
     reg = lm(prediction ~ Full_test$v)
     Performances[i,4] = summary(reg)$r.squared
     tree<-rpart( v ~ ., data=Full_train, control=rpart.control(cp=10^-5), method="anova")
     prediction = predict(tree,Full_test)
     reg = lm(prediction ~ Full_test$v)
     Performances[i,5] = summary(reg)$r.squared
   }
}

Performances
Performances["max"]= NA
Performances$max = apply(Performances[,3:5],1,max)

 write.table(Performances, "Performances-imp-modèles.txt", sep="\t") 

Performances = read.table("Performances-imp-modèles.txt", sep="\t")
Performances


