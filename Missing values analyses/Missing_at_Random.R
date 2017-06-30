library(mice)
library(VIM)

head(train)
head(test)

# Summary pour voir s'il n'y a pas de valeurs aberrantes à première vue
summary(train)

plot(train$v1)
hist(train$v1)

# Construction de la matrice des variables explicatives
train_var=train[,3:ncol(train)]

# Proportion d'observations manquantes par variable
prop_missing_val_by_var=colSums(is.na(train_var)/nrow(train_var))
prop_missing_val_by_var[prop_missing_val_by_var==0]

length(prop_missing_val_by_var[0.4<prop_missing_val_by_var & prop_missing_val_by_var<0.45])

sort(prop_missing_val_by_var)

  # Même étude sur la table test
prop_missing_val_by_var_test=colSums(is.na(test)/nrow(test))
sort(prop_missing_val_by_var_test)


# Proportion de variables manquantes par individu
rowSums(is.na(train_var)/ncol(train_var))

# Patron
md.pattern(train_var)

aggr_train_var=aggr(train_var)
head(aaggr_train_var)

# Missingness pattern can also be visualised in VIM package by
train_var_aggr = aggr(train_var, col=mdc(1:2), numbers=TRUE
                      , sortVars=TRUE, labels=names(train_var), cex.axis=.7
                      , gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

complete_cases=complete.cases(train[,3:ncol(train)])

train_complete_cases=train[complete_cases,]
head(train_complete_cases)
nrow(train_complete_cases)

#train_complete_cases_unless_v1=train[complete.cases(train[,4:ncol(train)]),]
#head(train_complete_cases)
#nrow(train_complete_cases)

train1=train

## Etude de la "missingness" des variables ayant entre 42 et 45% de valeurs manquantes

# v30

  # Regardons si v30 est MCAR en utilisant seulement les variables complètes
train1$miss_v30 <- ifelse(is.na(train$v30), 1, 0)                  
summary(train1$miss_v30)

reg_log=glm(miss_v30 ~ v24 + v38  +v47  +v62  +v66  +v71  +v72  +v74  +v75  +v79+ v110+ v129, data=train1, family=binomial)
summary(reg_log) # Les NA sont dus à la colinéarité parfaite entre les variables explicatives
reg_log_step=step(reg_log,direction = "forward") 
summary(reg_log_step) # Certains coeffs sont significativement non nuls -> v30 MAR au minimum

  # Comparaison avec le résultat qu'on obtiendrait avec une variable complétement aléatoire à la place de missing_v30
reg_log_bernoulli=glm(rbinom(nrow(train1),size = 1,prob = 0.5)
                      ~ v24 + v38  +v47  +v62  +v66  +v71  +v72  +v74  +v75  +v79+ v110+ v129, data=train1, family=binomial)
summary(reg_log_bernoulli)


## Etude de la "missingness" des variables ayant entre 3 et 6% de valeurs manquantes

# v31

# Regardons si v31 est MCAR en utilisant seulement les variables complètes
train1$miss_v31 <- ifelse(is.na(train$v30), 1, 0)                  
summary(train1$miss_v31)

reg_log=glm(miss_v31 ~ v24 + v38  +v47  +v62  +v66  +v71  +v72  +v74  +v75  +v79+ v110+ v129, data=train1, family=binomial)
summary(reg_log) # Les NA sont dus à la colinéarité parfaite entre les variables explicatives
reg_log_step=step(reg_log,direction = "forward") 
summary(reg_log_step) # Certains coeffs sont significativement non nuls -> v31 MAR au minimum



## Etude de la "missingness" des variables ayant entre 0 et 1% de valeurs manquantes

# v52

# Regardons si v52 est MCAR en utilisant seulement les variables complètes
train1$miss_v52 <- ifelse(is.na(train$v52), 1, 0)                  
summary(train1$miss_v52)

reg_log=glm(miss_v52 ~ v24 + v38  +v47  +v62  +v66  +v71  +v72  +v74  +v75  +v79+ v110+ v129, data=train1, family=binomial)
summary(reg_log) # Les NA sont dus à la colinéarité parfaite entre les variables explicatives
reg_log_step=step(reg_log,direction = "forward") 
summary(reg_log_step) # Aucun coeff n'est significativement non nul -> v52 MCAR (en même temps, il n'y a que 3 valeurs manquantes pour cette variable) 


# v21

# Regardons si v21 est MCAR en utilisant seulement les variables complètes
train1$miss_v21 <- ifelse(is.na(train$v21), 1, 0)                  
summary(train1$miss_v21)

reg_log=glm(miss_v21 ~ v24 + v38  +v47  +v62  +v66  +v71  +v72  +v74  +v75  +v79+ v110+ v129, data=train1, family=binomial)
summary(reg_log) # Les NA sont dus à la colinéarité parfaite entre les variables explicatives
reg_log_step=step(reg_log,direction = "forward") 
summary(reg_log_step) # Certains coeffs sont significativement non nuls -> v21 MAR au minimum

sum(train1$miss_v52)





sum()# Test de Wald (nullité de l'ensemble des coeffs)
library(aod)
wald.test(b = coef(reg_log), Sigma = vcov(reg_log), Terms = 2:length(reg_log$coefficients))


head(train1[,c("v30","v129","miss_v30")],n=50)

summary(glm(miss_v30 ~ v129 + v79, data=train1, family=binomial))
