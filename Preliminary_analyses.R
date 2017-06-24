library(rattle)
library(ggplot2)

setwd(".../...")
getwd()

train <- read.csv("Données/train.csv", na.strings = c("", " "))
test <- read.csv("Données/test.csv", na.strings = c("", " "))

summary(train)

###### Counting the number of NAs

count_NA = rep(0,131)
for (i in 3:133) {
  count_NA[i-2] = sum(is.na(train[,i]))
}
count_NA = as.data.frame(t(count_NA))
colnames(count_NA) = colnames(train[3:133])

##### Look at people's distributions regarding their respective answers
count_NA_obs = apply(train, 1, FUN = function(x) sum(is.na(x)) )
summary(count_NA_obs)
hist(count_NA_obs)
count_NA_obs = as.data.frame(count_NA_obs)

count_NA_obs.t = apply(test, 1, FUN = function(x) sum(is.na(x)) )
summary(count_NA_obs.t)
hist(count_NA_obs.t)
count_NA_obs.t = as.data.frame(count_NA_obs.t)

ggplot(count_NA_obs, aes(x=count_NA_obs)) + geom_histogram(binwidth=1) + coord_cartesian(ylim = c(0, 50))

table(count_NA_obs)
# People don't answer to :
# - 0 question (17756)
# - 1 question (28488)
# - 2 questions (14179)
# - 3 questions (1690)
# - 4 questions (1377)
# - 25 questions (570)
# - 26 questions (254)
# - 81 questions (457)
# - 82 questions (1049)
# - 83 questions (458)
# - 100 questions (4417)
# - 101 questions (26967)
# - 102 questions (15262)
# - 103 questions (708)
# - 104 questions (216)
# - 105 questions (83)

##### Create variable for each observation with number of non-answers
train$NO_RESPONSE <- as.numeric(as.character(count_NA_obs$count_NA_obs))
test$NO_RESPONSE <- as.numeric(as.character(count_NA_obs.t$count_NA_obs.t))

boxplot(NO_RESPONSE~target, data = train)
### Boxplot doesn't tell much : the number of non-responses doesn't explain target well

#### Continuous and discrete variables
# - Discrete : v3, v22, v24, v30, v31, v47, v52, v66, v71, v74, v75, v79, v91, v107, v110, v112, v113, v125
# - Continuous : others

names <- colnames(train)[2:(ncol(train)-1)]
features <- train[,colnames(train) %in% names]
discrets <- c("target", "v3", "v22", "v24", "v30", "v31", "v47", "v52", "v66", "v71", "v74", "v75", "v79", "v91", "v107", 
              "v110","v112", "v113", "v125")
train.discrete <- train[,colnames(train)%in%discrets]

prop.table(table(train.discrete$target, train.discrete[,"v3"]),2)
#prop.table(table(train.discrete$target, train.discrete[,"v22"]),2) --> beaucoup de modalités !
prop.table(table(train.discrete$target, train.discrete[,"v24"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v30"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v31"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v47"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v52"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v66"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v71"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v74"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v75"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v79"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v91"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v107"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v112"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v113"]),2)
prop.table(table(train.discrete$target, train.discrete[,"v125"]),2)

#### Rank variables by number of NAs

res <- colSums(is.na(train)==TRUE)
dres <- t(data.frame(res))
dres <- dres[,colnames(dres) %in% names]
sdres <- sort(dres)
res <- rev(sdres)
rr <- cbind(names(res),res)
rownames(rr) <- NULL
colnames(rr) <- c("Variable","NAs")
rr <- data.frame(rr)
rr$Variable <- NULL

##### NA analysis

unique(is.na(train[count_NA_obs==100,]))
# When an individual doesn't answer 100 questions, it's always the same 100 questions

unique(is.na(train[count_NA_obs==1,]))
# 3 rows
unique(is.na(train[count_NA_obs==25,]))
# 1 row
unique(is.na(train[count_NA_obs==81,]))
# 1 row
unique(is.na(train[count_NA_obs==101,]))
# 1 row

train[count_NA_obs!=0 & count_NA_obs != 1 & count_NA_obs != 25 & count_NA_obs != 81 & count_NA_obs != 100 & count_NA_obs != 101 & count_NA_obs != 106, "target" ]
# How many don't fit into one category ?
# 195, amongst whom 76% have target variable that is == 1

mean(train[count_NA_obs!=0 & count_NA_obs != 1 & count_NA_obs != 25 & count_NA_obs != 81 & count_NA_obs != 100 & count_NA_obs != 101 & count_NA_obs != 106, "target" ])
mean(train$target)
# The target averages are the same on the outliers or on all the database

# Select individuals who answered 31 questions exactly
train_100 = train[count_NA_obs == 100,!(unique(is.na(train[count_NA_obs==100,])))]
dim(train_100)
summary(train_100)

# column ids of the 31 questions responded by the 47 000 individuals
which(!(unique(is.na(train[count_NA_obs==100,]))))

