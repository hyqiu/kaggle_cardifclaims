#install.packages("xgboost")

library(xgboost)
## Loading required package: xgboost
library(methods)
## Loading required package: methods
library(data.table)
## Loading required package: data.table
library(magrittr)
## Loading required package: magrittr
library(Ckmeans.1d.dp)


### Extrait du tutoriel
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
head(agaricus.train)
head(agaricus.test)

#Random ForestT - 1000 trees
bst <- xgboost(data = agaricus.train$data, label = agaricus.train$label, max.depth = 4, num_parallel_tree = 1000, subsample = 0.5, colsample_bytree =0.5, nround = 1, objective = "binary:logistic")

pred <- predict(bst, agaricus.test$data)

# train$data@Dimnames[[2]] represents the column names of the sparse matrix.
importance_matrix=xgb.importance(agaricus.train$data@Dimnames[[2]], model = bst)
xgb.plot.importance(importance_matrix)

xgb.plot.tree(agaricus.train$data@Dimnames[[2]], model = bst) # Ne marche pas


########## Tentative sur la base train ##########################

class(train)

cv.nround <- 5 # Le nb maxi d'itérations (de quoi ?)
cv.nfold <- 3  # Nb de groupes pour la validation croisée

# XGB avec validation croisée

bst.cv = xgb.cv( data = as.matrix(train[1:1000, 3:4])
                 ,label = train[1:1000, 2] # label correspond à la variable target, apparemment
                 ,nfold = cv.nfold         # Nb de groupes pour la validation croisée
                 ,nrounds = cv.nround      # Le nb maxi d'itérations (de quoi ?)
                 ,max.depth = 4            # La profondeur maxi des arbres
                 ,objective = "binary:logistic" # Pour dire que la variable target est binaire
                 ,missing=NA               # Pour dire que les NA sont des valeurs manquantes
                 ,prediction=TRUE          # Pour que l'output contiennent les probas prédites (que target=1 ?)
                 )
      # TESTER AVEC D'AUTRES VALEURS DE PARAMETRES

# Comparaison entre les valeurs de target et les probas prédites par XGBoost
compare_result=cbind(train[1:1000, 2],bst.cv$pred) 

# XGB dans validation croisée
bst = xgboost( data = as.matrix(train[1:1000, 3:4])
                 ,label = train[1:1000, 2] # label correspond à la variable target, apparemment
                 ,nrounds = cv.nround      # Le nb maxi d'itérations (de quoi ?)
                 ,max.depth = 4            # La profondeur maxi des arbres
                 ,objective = "binary:logistic" # Pour dire que la variable target est binaire
                 ,missing=NA               # Pour dire que les NA sont des valeurs manquantes
                 ,prediction=TRUE         # Pour que l'output contiennent les probas prédites (que target=1 ?)
               )

# Plot de l'"importance des variables" : voir ce que cela veut dire 
Names_variables=colnames(train[1:1000, 3:4])
importance_matrix=xgb.importance(Names_variables, model = bst)
xgb.plot.importance(importance_matrix)

xgb.plot.tree(Names_variables, model = bst) # Ne marche pas


############################ Tentative avec plus de variables : tjrs les 1000 premières valeurs uniquement ###############

train_var=train[, -c(1,2)] # Retrait de id et target

Class_type<-NULL
for (i in 1:ncol(train_var)){
  Class_type[i]=class(train_var[,i])
}
Class_type == "numeric"| Class_type =="integer" # Condition pour n'avoir que des variables quanti


bst.cv = xgb.cv( data = as.matrix(train_var[1:1000, Class_type == "numeric"| Class_type =="integer"])
                 ,label = train[1:1000, 2] # label correspond à la variable target, apparemment
                 ,nfold = cv.nfold         # Nb de groupes pour la validation croisée
                 ,nrounds = cv.nround      # Le nb maxi d'itérations (de quoi ?)
                 ,max.depth = 4            # La profondeur maxi des arbres
                 ,objective = "binary:logistic" # Pour dire que la variable target est binaire
                 ,missing=NA               # Pour dire que les NA sont des valeurs manquantes
                 ,prediction=TRUE          # Pour que l'output contiennent les probas prédites (que target=1 ?)
)
          # TESTER AVEC D'AUTRES VALEURS DE PARAMETRES

# Comparaison entre les valeurs de target et les probas prédites par XGBoost
compare_result=cbind(train[1:1000, 2],bst.cv$pred)

# Courbe ROC
Y= train[1:1000, 2]
S= bst.cv$pred
library(ROCR)
pred = prediction (S,Y)
plot ( performance (pred ,"tpr","fpr")) # Tracé de la courbe ROC
abline(b=1, a=0, col="red")

# Obtention de l'AUC et d'autres indicateurs pour comparer les modèles 

library(hmeasure)
HMeasure(Y,S)$metrics[,1:5] # AUC = 0.65




############################ Tentative avec toutes les valeurs ###############

train_var=train[, -c(1,2)] # Retrait de id et target

Class_type<-NULL
for (i in 1:ncol(train_var)){
  Class_type[i]=class(train_var[,i])
}
Class_type == "numeric"| Class_type =="integer" # Condition pour n'avoir que des variables quanti

#### Cross-validated xgb

bst.cv = xgb.cv( data = as.matrix(train_var[, Class_type == "numeric"| Class_type =="integer"])
                 ,label = train[, 2] # label correspond à la variable target, apparemment
                 ,nfold = cv.nfold         # Nb de groupes pour la validation croisée
                 ,nrounds = cv.nround      # Le nb maxi d'itérations (de quoi ?)
                 ,max.depth = 4            # La profondeur maxi des arbres
                 ,objective = "binary:logistic" # Pour dire que la variable target est binaire
                 ,missing=NA               # Pour dire que les NA sont des valeurs manquantes
                 ,prediction=TRUE          # Pour que l'output contiennent les probas prédites (que target=1 ?)
)
        # TESTER AVEC D'AUTRES VALEURS DE PARAMETRES

# Comparaison entre les valeurs de target et les probas prédites par XGBoost
compare_result=cbind(train[, 2],bst.cv$pred)

# Courbe ROC
Y= train[, 2]
S= bst.cv$pred
library(ROCR)
pred = prediction (S,Y)
plot ( performance (pred ,"tpr","fpr")) # Tracé de la courbe ROC
abline(b=1, a=0, col="red")

# Obtention de l'AUC et d'autres indicateurs pour comparer les modèles 

library(hmeasure)
HMeasure(Y,S)$metrics[,1:5] # AUC = 0.709


#### xgb

bst = xgboost( data = as.matrix(train_var[, Class_type == "numeric"| Class_type =="integer"])
               ,label = train[, 2] # label correspond à la variable target, apparemment
               ,nrounds = cv.nround      # Le nb maxi d'itérations (de quoi ?)
               ,max.depth = 4            # La profondeur maxi des arbres
               ,objective = "binary:logistic" # Pour dire que la variable target est binaire
               ,missing=NA               # Pour dire que les NA sont des valeurs manquantes
               ,prediction=TRUE          # Pour que l'output contiennent les probas prédites (que target=1 ?)
)

# Plot de l'"importance des variables" : voir ce que cela veut dire 
Names_variables=colnames(train_var[, Class_type == "numeric"| Class_type =="integer"])
importance_matrix=xgb.importance(Names_variables, model = bst)
xgb.plot.importance(importance_matrix)


#### Avec presque toutes les variables

train_var_quali=train_var[which((Class_type == "numeric"| Class_type =="integer") ==FALSE) ]
summary(train_var_quali)

Class_type<-NULL
for (i in 1:ncol(train_var_quali)){
  Class_type[i]=class(train_var_quali[,i])
  length_levels[i]=length(levels(train_var_quali[,i]))
}
colnames(train_var_quali)


indic <- model.matrix( ~ v3
                         # + v22
                         + v24
                         + v30
                         + v31 
                         + v47
                         + v52
                         + v56
                         + v66
                         + v71
                         + v74
                         + v75
                         + v79 
                         + v91
                         -1, model.frame( ~ v3
                                         # + v22
                                         + v24
                                         + v30
                                         + v31 
                                         + v47
                                         + v52
                                         + v56
                                         + v66
                                         + v71
                                         + v74
                                         + v75
                                         + v79 
                                         + v91
                                         -1,train_var_quali,
                                                na.action=function(x)x))
nrow(indic)
nrow(train_var)

train_var_quanti=train_var[, Class_type == "numeric"| Class_type =="integer"]
nrow(train_var_quanti)
train_var_quanti_indic=data.frame(train_var_quanti,indic)

#### Cross-validated xgb


bst.cv = xgb.cv( data = as.matrix(train_var_quanti_indic)
                 ,label = train[, 2] # label correspond à la variable target, apparemment
                 ,nfold = 5                # Nb de groupes pour la validation croisée
                 ,nrounds = 10             # Le nb maxi d'itérations (le nombre d'arbres construits)
                 ,max.depth = 4            # La profondeur maxi des arbres
                 ,objective = "binary:logistic" # Pour dire que la variable target est binaire
                 ,missing=NA               # Pour dire que les NA sont des valeurs manquantes
                 ,prediction=TRUE          # Pour que l'output contiennent les probas prédites (que target=1 ?)
)
# TESTER AVEC D'AUTRES VALEURS DE PARAMETRES

# Comparaison entre les valeurs de target et les probas prédites par XGBoost
compare_result=cbind(train[, 2],bst.cv$pred)

# Courbe ROC
Y= train[, 2]
S= bst.cv$pred
library(ROCR)
pred = prediction (S,Y)
plot ( performance (pred ,"tpr","fpr")) # Tracé de la courbe ROC
abline(b=1, a=0, col="red")

# Obtention de l'AUC et d'autres indicateurs pour comparer les modèles 

library(hmeasure)
HMeasure(Y,S)$metrics[,1:5] # AUC = 0.700

### xgb

bst = xgboost( data = as.matrix(train_var_quanti_indic)
               ,label = train[, 2] # label correspond à la variable target, apparemment
               ,nrounds = 5      # Le nb maxi d'itérations (de quoi ?)
               ,max.depth = 4            # La profondeur maxi des arbres
               ,objective = "binary:logistic" # Pour dire que la variable target est binaire
               ,missing=NA               # Pour dire que les NA sont des valeurs manquantes
               ,prediction=TRUE          # Pour que l'output contiennent les probas prédites (que target=1 ?)
)

# Plot de l'"importance des variables" : voir ce que cela veut dire 
Names_variables=colnames(train_var_quanti_indic)
importance_matrix=xgb.importance(Names_variables, model = bst)
xgb.plot.importance(importance_matrix)
