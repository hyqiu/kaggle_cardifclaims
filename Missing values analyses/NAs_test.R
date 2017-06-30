
setwd("C:/Users/CHARLES/Dropbox/Big Data & Assurance/Bases de données")

test = read.csv("test.csv", na.strings=c("","NA"))

dim(test) # 114321    133
t(head(test))

str(test)
test$v75

table(test$target)
# 1 : on n'a pas besoin de poser de questions supplÃ©mentaires

sum(is.na(test))/(ncol(test)*nrow(test))
# 32.9% de NA dans la matrice (vs 32.7 pour train)

# On calcule le nombre de NA pour chaque variable
count_NA = rep(0,131)
for (i in 2:132) {
  count_NA[i-1] = sum(is.na(test[,i]))
}
count_NA = as.data.frame(t(count_NA))
colnames(count_NA) = colnames(test[2:132])
count_NA
# Combien de variables ne présentent aucune valeur manquante?
ncol(count_NA[,which(count_NA[1,]==0)])
# 12

# Lesquelles?
count_NA[,which(count_NA[1,]==0)]
# v24 v38 v47 v62 v66 v71 v72 v74 v75 v79 v110 v129


# On compte le nombre de NA par individu
count_NA_obs = apply(test, 1, FUN = function(x) sum(is.na(x)) )
summary(count_NA_obs)

count_NA_obs = as.data.frame(count_NA_obs)
library(ggplot2)
previous_them <- theme_set(theme_bw()) # pour avoir des graphiques à fond blanc
ggplot(count_NA_obs, aes(x=count_NA_obs)) + geom_histogram(binwidth=1)
# en zoomant progressivement sur les faibles effectifs
ggplot(count_NA_obs, aes(x=count_NA_obs)) + geom_histogram(binwidth=1) + coord_cartesian(ylim = c(0, 5000))
ggplot(count_NA_obs, aes(x=count_NA_obs)) + geom_histogram(binwidth=1) + coord_cartesian(ylim = c(0, 1000))
ggplot(count_NA_obs, aes(x=count_NA_obs)) + geom_histogram(binwidth=1) + coord_cartesian(ylim = c(0, 200))
ggplot(count_NA_obs, aes(x=count_NA_obs)) + geom_histogram(binwidth=1) + coord_cartesian(ylim = c(0, 50))

# On crée un tableau qui donne combien d'individus n'ont pas répondu à x questions
nb_ind = data.frame(matrix(NA, nrow = 1, ncol = 132))
colnames(nb_ind) = 0:131
for (i in 1:132){
  nb_ind[1,i] = sum(count_NA_obs==i-1)
}
nb_ind

# Dans l'ensemble, les gens ne rÃ©pondent pas Ã 
# - 0 question (62561)
# - 1 question (934)
# - 25 questions (873)
# - 81 questions (2045)
# - 100 questions (47438)
# - 101 questions (210)
# - 106 questions (65)
# Tous les autres sont infÃ©rieurs Ã  50.

unique(is.na(test[count_NA_obs==100,]))
# On obtient une seule ligne, ce qui signifie que lorsqu'un individu ne rÃ©pond pas Ã  100 questions, 
# ce sont toujours les mÃªmes 100 questions.
unique(is.na(test[count_NA_obs==1,]))
# On obtient 3 lignes
unique(is.na(test[count_NA_obs==25,]))
# On obtient 1 ligne
unique(is.na(test[count_NA_obs==81,]))
# 1 ligne
unique(is.na(test[count_NA_obs==101,]))
# 1 ligne
unique(is.na(test[count_NA_obs==106,]))
# 1 ligne

test[count_NA_obs!=0 & count_NA_obs != 1 & count_NA_obs != 25 & count_NA_obs != 81 & count_NA_obs != 100 & count_NA_obs != 101 & count_NA_obs != 106, "target" ]
# On compte le nombre de personnes qui ne rentrent pas dans les catÃ©gories ci-dessus.
# Il y en a 195

# Cette population de 195 personnes a un taux de "target=1" très proche de celui de
# la population globale (76%)
mean(test[count_NA_obs!=0 & count_NA_obs != 1 & count_NA_obs != 25 & count_NA_obs != 81 & count_NA_obs != 100 & count_NA_obs != 101 & count_NA_obs != 106, "target" ])
# 0.764
mean(test$target)
#  0.761
