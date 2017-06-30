setwd("C:/Users/CHARLES/Dropbox/Big Data & Assurance/Bases de données")

train = read.csv("train.csv", na.strings=c("","NA"))
test = read.csv("test.csv", na.strings=c("","NA"))


Facteurs = c(3,24,30,31,47,52,56,66, 71, 74,75,79,91,107,110,112,113,125)
Facteurs_names = colnames(train[,(Facteurs+2)])
Facteurs_names

Echantillon = sample(nrow(train), nrow(train)/100)
train_sample = train[Echantillon, 3:ncol(train)]

train_sample$v22=NULL

############### Un essai avec Amelia
library(Amelia)
a.out = amelia(train_sample, m=2, noms=Facteurs_names[1:(length(Facteurs)-1)], amcheck=F)
# https://lists.gking.harvard.edu/pipermail/amelia/2015-January/001128.html


############## Imputation des variables continues avec FastImputation
library(FastImputation)
continus <- train_sample[,!colnames(train_sample) %in% Facteurs_names]
patterns <- TrainFastImputation(continus)

imp.continus <- FastImputation(x = continus, patterns = patterns, verbose = TRUE)
imp.continus



# Regarder rfImpute
# https://cran.r-project.org/web/packages/randomForest/randomForest.pdf


############ Imputation avec missForest
######### le papier correspondant: http://arxiv.org/abs/1105.0828
library(missForest)
missForest(train_sample)


train_facteurs = train[,colnames(train) %in% Facteurs_names]
str(train_facteurs)
# Donc à part v22, deux variables ont plus de 40 modalités: v56 et v125
# Que fait-on?

############ On remplace les modalités représentant moins de 1% des individus par des NA
# pour v56
table(train$v56, exclude=NULL)
p= 0.01
lf <- names(which(prop.table(table(train$v56)) < p))
length(which(prop.table(table(train$v56)) < p))
length(which(prop.table(table(train$v56)) >= p))
train$v56[which(train$v56 %in% lf)] = NA

# On reconstitue le facteur (efface les levels devenus vides)
train$v56 = factor(train$v56)
table(train$v56, exclude=NULL)

# pour v125
table(train$v125, exclude=NULL)
p= 0.01
lf <- names(which(prop.table(table(train$v125)) < p))
length(which(prop.table(table(train$v125)) < p))
length(which(prop.table(table(train$v125)) >= p))
train$v125[which(train$v125 %in% lf)] = NA

# On reconstitue le facteur (efface les levels devenus vides)
train$v125 = factor(train$v125)
table(train$v125, exclude=NULL)

#### Essai sur v22, mais trop de NA, il faut la virer
# sum(is.na(train$v22))
# p= 0.001
# lf <- names(which(prop.table(table(train$v22)) < p))
# length(which(prop.table(table(train$v22)) < p))
# length(which(prop.table(table(train$v22)) >= p))
# train$v22[which(train$v22 %in% lf)] = NA
# 
# # On reconstitue le facteur (efface les levels devenus vides)
# train$v22 = factor(train$v22)
# table(train$v22, exclude=NULL)



########## Bilan: prétraitement de la base
train = read.csv("train.csv", na.strings=c("","NA"))

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

library(FastImputation)

# On crée un vecteur donnant le nb de NA par variable
sumna = function(x){
  y = sum(is.na(x))
  return(y)
}
NAparvar = apply(train[,3:ncol(train)], 2, sumna)
Vcomp = names(NAparvar[which(NAparvar==0)])
Vcomp


# On récupère un échantillon de 20% de la base
Echantillon = sample(nrow(train), nrow(train)/5)
train_sample = train[Echantillon, 3:ncol(train)]


continus <- train_sample[,!colnames(train_sample) %in% Facteurs_names]
continus = continus[,!colnames(continus) %in% Vcomp]
Continus_names = colnames(continus)

# continus
# str(continus)

# On crée un échantillon dans train_sample, de 33%, qui est celui pour lequel la variable étudiée va être considérée
# comme non-observée
Echantillon2 = sample(nrow(continus), nrow(continus)/3)
Echantillon2

Perf_imp = data.frame(matrix(ncol=3, nrow=ncol(continus)))
colnames(Perf_imp) = c("variable", "Nombre_NA","Performance")
Perf_imp$variable = colnames(continus)
Perf_imp$Nombre_NA = NAparvar[which(!(names(NAparvar) %in% c(Vcomp, Facteurs_names)))]
Perf_imp

for (i in 1:ncol(continus)){
  # on stocke dans un vecteur les valeurs qu'on va transformer en NA
  Rows_changed = which(!is.na(continus[Echantillon2,i]))
  # continus_changed est le df pour lequel tous les ind de Echantillon2 sont considérés comme non-
  # observés pour la variable i
  continus_changed = continus
  continus_changed[Rows_changed,i] = NA
  # On impute la base
  patterns <- TrainFastImputation(continus_changed)
  imp.continus <- FastImputation(x = continus_changed, patterns = patterns, verbose = TRUE)
  # On récupère le vecteur des valeurs que l'on a changées en NA
  # celles prédites par l'imputation
  predites = imp.continus[Rows_changed,i]
  # et les vraies valeurs
  reelles = continus[Rows_changed,i]
  # On les régresse et on récupère le coeff de détermination
  reg = lm(predites ~ reelles)
  Perf_imp[i,3] = summary(reg)$r.squared
#   str(imp.continus)
#   continus[1:10,1:5]
#   imp.continus[1:10,1:5] 
}

Perf_imp

write.table(Perf_imp, "Performances-imputation-gaussienne.txt", sep="\t")

Perf_arbresreg = read.table("Performances-imp-modèles.txt", sep="\t")
Perf_impgauss = read.table("Performances-imputation-gaussienne.txt", sep="\t")

Perf_arbresreg = Perf_arbresreg[,c(1,2,6)]
Perf_impgauss

Comparaison_perf = merge(Perf_arbresreg, Perf_impgauss, all=T)
# On réordonne les colonnes
Comparaison_perf = Comparaison_perf[c(1,2,4,3,5)]
colnames(Comparaison_perf) = c("variable", "Type", "Nombre_NA", "Imputation_arbres", "Imputation_gaussienne")
Comparaison_perf$Rapport = Comparaison_perf$Imputation_gaussienne/Comparaison_perf$Imputation_arbres
# On ordonne le data frame par Rapport croissant
Comparaison_perf = Comparaison_perf[order(Comparaison_perf[,6],decreasing=T),]
Comparaison_perf
write.table(Comparaison_perf, "Comparaison arbres vs gaussien.txt", sep="\t")

Comparaison_perf[Comparaison_perf$Rapport>1,6]
