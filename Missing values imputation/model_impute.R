setwd("C:/Users/CHARLES/Dropbox/Big Data & Assurance/Bases de données")
setwd("~/Documents/ENSAE/3A/S2/Big data et assurance/Projet")

train = read.csv("train.csv", na.strings=c("","NA"))
test = read.csv("test.csv", na.strings=c("","NA"))
str(train[,1:60])
str(train[,61:133])
# Quelles sont les variables cat?gorielles ? plus de 10 modalit?s?
# v22 >18000
# v56  122
# v79 18     # mais cette variable fait partie des 12 variables communes; on va la laisser intacte
# v112 22
# v113 36
# v125 90

count_NA_obs = apply(train, 1, FUN = function(x) sum(is.na(x)) )
summary(count_NA_obs)

count_NA_obs = as.data.frame(count_NA_obs)

# On va travailler sur les 17000 ind sans donn?es manquantes dans un premier tps
train_ind_0na = train[count_NA_obs==0,]
train_essai = train_ind_0na[1:500,]


#train_grp
library(rpart)
library(rpart.plot)

treev22<-rpart(v22 ~ v24 + v38 + v47 +v62 +v66+ v71+ v72+ v74+ v75+ v79+ v110 +v129, data=train_essai, control=rpart.control(cp=10^-3))
prp(treegp1, type=2, extra=1)
printcp(treegp1)



base$Group1<-relevel(base$Group1, "20")

# nb: pour se renseigner sur le where:
#http://stackoverflow.com/questions/29215499/combining-df-and-rpartwhere

treev112<-rpart(v112 ~ v24 + v38 + v47 +v62 +v66+ v71+ v72+ v74+ v75+ v79+ v110 +v129, data=train, control=rpart.control(cp=10^-4))
prp(treev112, type=2, extra=2)
printcp(treev112)

##########################################################
# Mise Ã  jour de la base de donnÃ©es aprÃ¨s analyse des NA #
##########################################################

# Conclusion de l'analyse des NA sur les 12 variables communes Ã  tous les rÃ©pondants
levels_v71_train = setdiff(levels(train$v71),levels(test$v71)) #  "A" "D" "K"
levels_v71_test = setdiff(levels(test$v71) ,levels(train$v71)) # "E" "H" "J"
levels(test$v71) = c("B", "C", "F", "F", "G", "F", "I", "F", "L")
table(test$v71)

#treev112<-rpart(v112 ~ v24 + v38 + v47 +v62 +v66+ v71+ v72+ v74+ v75+ v79+ v110 +v129, data=train, control=rpart.control(cp=10^-5))
#prp(treev112, type=2, extra=2)
#printcp(treev112)
#str(train$v112)

####################################################################
###### Exemple : Etude de la variable v112 ###### 
# 382 NA dans v112, 113 939 non NA
real_v112 = train[!is.na(train$v112),114]

# Arbre simple
treev112<-rpart(v112 ~ v24 + v38 + v47 +v62 +v66+ v71+ v72+ v74+ v75+ v79+ v110 +v129, data=train, control=rpart.control(cp=10^-4))
pred_tree_v112 = predict(
  object = treev112, 
  data=train[!is.na(train$v112),],
  type="class")
table(pred_tree_v112,real_v112)
perf_tree_v112 = sum((pred_tree_v112==real_v112))/length(real_v112)
1-perf_tree_v112
# On trouve un error rate de 80.39%


# Tentative Charles, à partir de la page:
# http://stackoverflow.com/questions/9666212/how-to-compute-error-rate-from-a-decision-tree
# démarche identique, mais pour verif
real_v112 = train[!is.na(train$v112),114]
length(real_v112)
treev112<-rpart(v112 ~ v24 + v38 + v47 +v62 +v66+ v71+ v72+ v74+ v75+ v79+ v110 +v129, data=train, control=rpart.control(cp=10^-4))
printcp(treev112)

class.pred <- table(predict(treev112, type="class"), real_v112)
1-sum(diag(class.pred))/sum(class.pred)
# On retrouve un error rate de 80.39%

# Avec une RF
library("randomForest")
RFv112 = randomForest(v112 ~ v24 + v38 + v47 +v62 +v66+ v71+ v72+ v74+ v75+ v79+ v110 +v129, 
             data=train[!is.na(train$v112),],
             mtry = 7,
             ntree=50)
pred_RF_v112 = predict(RFv112, data=train[!is.na(train$v112),],type="response")
table(pred_RF_v112,real_v112)
perf_RF_v112 = sum((pred_RF_v112==real_v112))/length(real_v112)


###### Exemple : Etude de la variable v1 ###### 
# 49 832 NA dans v112, 64 489 non NA
real_v1 = train[!is.na(train$v1),3]
length(train$v1)-length(real_v1)

# GLM
GLMv1 = glm(v1 ~ v24 + v38 + v47 +v62 +v66+ v71+ v72+ v74+ v75+ v79+ v110 +v129, 
              data = train[!is.na(train$v1),]
)
summary(GLMv1)

GLMv1 = glm(v1 ~ ., 
          data = train[!is.na(train$v1),3:133]
)
summary(GLMv1)


# Etude de la variable v3
real_v3 = train[!is.na(train$v2),4]
treev3<-rpart(v3 ~ v24 + v38 + v47 +v62 +v66+ v71+ v72+ v74+ v75+ v79+ v110 +v129, data=train, control=rpart.control(cp=10^-4), method="anova")
printcp(treev3)

class.pred <- table(predict(treev3, type="vector"), real_v3)
1-sum(diag(class.pred))/sum(class.pred)

# Un error rate de 0.2%

