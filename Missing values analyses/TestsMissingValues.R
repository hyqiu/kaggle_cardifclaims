

############################## Analyse des variables ##############
## Liste des variables complètes : v24  v38  v47  v62  v66  v71  v72  v74  v75  v79 v110 v129

train_var=train[, -c(1,2)] # Retrait de id et target

Class_type<-NULL
for (i in 1:ncol(train_var)){
  Class_type[i]=class(train_var[,i])
}
colnames((train_var[, Class_type == "numeric"| Class_type =="integer"])) # Liste des variables quanti
colnames((train_var[,!(Class_type == "numeric"| Class_type =="integer")])) # Liste des variables quali


########################### Tentatives de méthodes ###########

##### Pattern

# Cf. plot Henri

##### Test de Little

library(BaylorEdPsych)
library(mvnmle)

data(EndersTable1_1)
head(EndersTable1_1)
LittleMCAR(EndersTable1_1)


LMCAR=LittleMCAR(train[sample(1:nrow(train),10000),c("v1","v38","v62","v72","v129")])
LMCAR$p.value  # P-valeur très faible -> rejet de l'hyp MCAR
               # Apparemment, ne marche pas en multivarié
LMCAR$chi.square
LMCAR$amount.missing


## Corrélation 
cor(train$v1[!is.na(train$v1)],train$target[!is.na(train$v1)])
cor(train$v2[!is.na(train$v2)],train$target[!is.na(train$v2)])

##### Régression logistique

# Cf. code CheckMAR


###### Utilisation du package MissMech
library(MissMech)
out <- TestMCARNormality(data=train1[sample(1:nrow(train1),1000), c("v2","v62","v38")])
print(out)
    # Il faut supprimer le nombre d'observations
out <- TestMCARNormality(data=train1[sample(1:nrow(train1),1000), c("v1","v38","v62","v72","v129")])
print(out)
out <- TestMCARNormality(data=train1[, c("v14","v62","v38")])
print(out)
 
