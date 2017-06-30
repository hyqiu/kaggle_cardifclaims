#setwd("C:/Users/CHARLES/Dropbox/Big Data & Assurance/Bases de donn?es")
setwd("~/Documents/ENSAE/3A/S2/Big data et assurance/Projet")

train = read.csv("train.csv", na.strings=c("","NA"))
test = read.csv("test.csv", na.strings=c("","NA"))
str(train)

t(head(train))
dim(train) # 114321    133


table(train$target)
# 1 : on n'a pas besoin de poser de questions supplémentaires

sum(is.na(train))/(ncol(train)*nrow(train))
# 32.7% de NA dans la matrice

# On calcule le nombre de NA pour chaque variable
count_NA = rep(0,131)
for (i in 3:133) {
  count_NA[i-2] = sum(is.na(train[,i]))
}
count_NA = as.data.frame(t(count_NA))
colnames(count_NA) = colnames(train[3:133])
count_NA
# Combien de variables ne pr?sentent aucune valeur manquante?
ncol(count_NA[,which(count_NA[1,]==0)])
# 12

# Lesquelles?
count_NA[,which(count_NA[1,]==0)]
# v24 v38 v47 v62 v66 v71 v72 v74 v75 v79 v110 v129


#######################################################""
# On compte le nombre de NA par individu
count_NA_obs = apply(train, 1, FUN = function(x) sum(is.na(x)) )
summary(count_NA_obs)

count_NA_obs = as.data.frame(count_NA_obs)
str(count_NA_obs)
library(ggplot2)
previous_them <- theme_set(theme_bw()) # pour avoir des graphiques ? fond blanc
ggplot(count_NA_obs, aes(x=count_NA_obs)) + geom_histogram(binwidth=1)
# en zoomant progressivement sur les faibles effectifs
ggplot(count_NA_obs, aes(x=count_NA_obs)) + geom_histogram(binwidth=1) + coord_cartesian(ylim = c(0, 5000))
ggplot(count_NA_obs, aes(x=count_NA_obs)) + geom_histogram(binwidth=1) + coord_cartesian(ylim = c(0, 1000))
ggplot(count_NA_obs, aes(x=count_NA_obs)) + geom_histogram(binwidth=1) + coord_cartesian(ylim = c(0, 200))
ggplot(count_NA_obs, aes(x=count_NA_obs)) + geom_histogram(binwidth=1) + coord_cartesian(ylim = c(0, 50))

# On cr?e un tableau qui donne combien d'individus n'ont pas r?pondu ? x questions
nb_ind = data.frame(matrix(NA, nrow = 1, ncol = 132))
colnames(nb_ind) = 0:131
for (i in 1:132){
  nb_ind[1,i] = sum(count_NA_obs==i-1)
}
nb_ind



unique(is.na(train[count_NA_obs==100,]))
# On obtient une seule ligne, ce qui signifie que lorsqu'un individu ne répond pas à 100 questions, 
# ce sont toujours les mêmes 100 questions.
unique(is.na(train[count_NA_obs==1,]))
# On obtient 8 lignes
unique(is.na(train[count_NA_obs==2,]))
nrow(unique(is.na(train[count_NA_obs==2,])))
# On obtient 17 lignes

#unique(is.na(train[(count_NA_obs==1 |count_NA_obs==2 ),]))
#nrow(unique(is.na(train[(count_NA_obs==1 |count_NA_obs==2 ),])))

# on regarde pour chaque variable combien de profils "deux donn?es manquantes"
# ont un NA pour cette variable
nb_profils2na_par_var = apply(unique(is.na(train[count_NA_obs==2,])),2, FUN=function(x) sum(x))

# on cible celles qui ont au moins un profil
nb_profils2na_par_var_sans0 = nb_profils2na_par_var[nb_profils2na_par_var!=0]
nb_profils2na_par_var_sans0
# v3  v21  v22  v30  v31  v56  v87  v98 v102 v105 v112 v113 
length(nb_profils2na_par_var_sans0)
# 12 variables

# on regarde quelles sont les variables touch?es par
# les profils qui ont une seule donn?e manquante 
nb_profils1na_par_var = apply(unique(is.na(train[count_NA_obs==1,])),2, FUN=function(x) sum(x))
nb_profils1na_par_var_sans0 = nb_profils1na_par_var[nb_profils1na_par_var!=0]
nb_profils1na_par_var_sans0
# v21  v22  v30  v56  v98 v102 v112 v113 

# On observe que les variables obtenues dans nb_profils1na_par_var_sans0
# sont incluses dans nb_profils2na_par_var_sans0

# Par cons?quent, en retirant de la premi?re ?tape
# (calibration des mod?les sur la population na=0,1,2 pour les variables n'ayant
# pas de donn?es manquantes sur cette population)



train[count_NA_obs!=0 & count_NA_obs != 1 & count_NA_obs != 25 & count_NA_obs != 81 & count_NA_obs != 100 & count_NA_obs != 101 & count_NA_obs != 106, "target" ]
# On compte le nombre de personnes qui ne rentrent pas dans les catégories ci-dessus.
# Il y en a 195

unique_isna_train=unique(is.na(train))
unique_isna_train_nbNA = apply(unique_isna_train,1, function(x) sum(x))
nrow(unique_isna_train)
hist(unique_isna_train_nbNA)


# on r?cup?re les indices des variables remplies par les individus qui ont 100 r?ponses manquantes
# (on a vu qu'il s'agissait des mm variables)
temp = train[count_NA_obs==100,]
common_var = which(!is.na(temp[1,]))
common_var
base_0NAind = train[count_NA_obs==0,common_var]
base_100NAind = train[count_NA_obs==100,common_var]

i=5
var_trigger = data.frame(matrix(0, nrow = 1, ncol = length(common_var)))
colnames(var_trigger) = colnames(train)[common_var]

for (i in 3:length(common_var)){
  i
  if (is.element(class(base_0NAind[,i])[1], c("numeric", "integer"))){
    if ( max(base_0NAind[,i]) < min(base_100NAind[,i]|min(base_0NAind[,i]) > max(base_100NAind[,i]) ){
      var_trigger[1,i]=1
    }
  }
  if (class(base_0NAind[,i])[1] =="factor"){
    levels1 = unique(base_0NAind[,i])
    levels2 = unique(base_0NAind[,i])
    if (!identical(setdiff(levels1, levels2), character(0))) {
      var_trigger[1, i]=1
    }
  }
}
var_trigger

##############################################################################
# Etude des modalités en trop dans la base test par rapport à la base train ##
# Sur les 12 variables en commun à tous les répondants #######################
##############################################################################

# v24 v38 v47 v62 v66 v71 v72 v74 v75 v79 v110 v129
### Comparaison des modalités de la variable v24 des bases test et train.
levels_v24_train = levels(train$v24) #
levels_v24_test = levels(test$v24) #
length(setdiff(levels_v24_test,levels_v24_train)) # 0 modalités apparaissent dans test mais pas dans train
length(setdiff(levels_v24_train,levels_v24_test)) # 0 modalités apparaissent dans train mais pas dans test

### Comparaison des modalités de la variable v38 des bases test et train.
table(test$v38)
table(train$v38)

### Comparaison des modalités de la variable v47 des bases test et train.
levels_v47_train = levels(train$v47) #10
levels_v47_test = levels(test$v47) #9
length(setdiff(levels_v47_test,levels_v47_train)) # 0 modalités apparaissent dans test mais pas dans train
length(setdiff(levels_v47_train,levels_v47_test)) # 1 modalité apparaissent dans train mais pas dans test

### Comparaison des modalités de la variable v62 des bases test et train.
table(test$v62)
table(train$v62)

### Comparaison des modalités de la variable v66 des bases test et train.
table(train$v66)
table(test$v66)

### Comparaison des modalités de la variable v71 des bases test et train.
levels_v71_train = levels(train$v71) #9
levels_v71_test = levels(test$v71) #9
length(setdiff(levels_v71_test,levels_v71_train)) # 3 modalités apparaissent dans test mais pas dans train
length(setdiff(levels_v71_train,levels_v71_test)) # 3 modalités apparaissent dans train mais pas dans test
# Note : à chaque fois, une seule personne a la modalité qui manque
# Est-ce qu'on lui donne la valeur la plus fréquente, à savoir "F" ? OUIIII

### Comparaison des modalités de la variable v72 des bases test et train.
table(train$v72)
table(test$v72)

### Comparaison des modalités de la variable v74 des bases test et train.
levels_v74_train = levels(train$v74) #
levels_v74_test = levels(test$v74) #
length(setdiff(levels_v74_test,levels_v74_train)) # 0 modalités apparaissent dans test mais pas dans train
length(setdiff(levels_v74_train,levels_v74_test)) # 0 modalités apparaissent dans train mais pas dans test

### Comparaison des modalités de la variable v75 des bases test et train.
levels_v75_train = levels(train$v75) #
levels_v75_test = levels(test$v75) #
length(setdiff(levels_v75_test,levels_v75_train)) # 0 modalités apparaissent dans test mais pas dans train
length(setdiff(levels_v75_train,levels_v75_test)) # 0 modalités apparaissent dans train mais pas dans test

### Comparaison des modalités de la variable v79 des bases test et train.
levels_v79_train = levels(train$v79) #18
levels_v79_test = levels(test$v79) #17
length(setdiff(levels_v79_test,levels_v79_train)) # 0 modalités apparaissent dans test mais pas dans train
length(setdiff(levels_v79_train,levels_v79_test)) # 1 modalités apparaissent dans train mais pas dans test

### Comparaison des modalités de la variable v110 des bases test et train.
levels_v110_train = levels(train$v110) #
levels_v110_test = levels(test$v110) #
length(setdiff(levels_v110_test,levels_v110_train)) # 0 modalités apparaissent dans test mais pas dans train
length(setdiff(levels_v110_train,levels_v110_test)) # 0 modalités apparaissent dans train mais pas dans test

### Comparaison des modalités de la variable v129 des bases test et train.
table(train$v129)
table(test$v129)

##############################################################################
# Etude des modalités en trop pour toutes les variables de la base train #####
# Sur les 12 variables en commun à tous les répondants #######################
##############################################################################

n = 133
exceding_modes = matrix(0, nrow = n, ncol = 12)
var_communes = c(26, 40, 49, 64, 68, 73, 74, 76, 77, 81, 112, 131)
for (i in c(3:133)[!(c(3:133) %in% var_communes)]) {
  train_v = is.na(train[,i])
  for (j in 1:12) {
    exceding_modes[i,j] = length(
                            setdiff(
                              levels( train[ train_v,  var_communes[j] ]  ),
                              levels( train[ !train_v, var_communes[j] ] )
                            )
                          )
  }
}
summary(exceding_modes) # OK

