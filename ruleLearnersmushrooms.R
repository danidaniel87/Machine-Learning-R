# Example – identifying poisonous
# mushrooms with rule learners

####Step 1 – collecting data####
mushrooms <- read.csv("C:/Users/Solutio/Desktop/Machine Learning with R/mushroom.csv", stringsAsFactors = TRUE)
str(mushrooms)
####Step 2 – exploring and preparing the data####
str(mushrooms$veil_type) #es raro tener solo 1 nivel, nos la cargamos porque esta mal clasificado 

mushrooms$veil_type <- NULL

table(mushrooms$type)

####Step 3 – training a model on the data####
library("RWeka")
library("rJava")

mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R

####Step 4 – evaluating model performance####
summary(mushroom_1R)

####Step 5 – improving model performance####
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip
