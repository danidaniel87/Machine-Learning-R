####Example – identifying risky bank loans using C5.0 decision trees####

####Step 1 – collecting data####
credit <- read.csv("C:/Users/Solutio/Desktop/credit.csv", stringsAsFactors=T)

####Step 2 – exploring and preparing the data####
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)

summary(credit$months_loan_duration)
summary(credit$amount)

table(credit$default)

####Data preparation – creating random training and test datasets####
set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample,]
credit_test<-credit[-train_sample,]

#tenemos que tener al menos 30% de defaults
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

library("C50")
?C5.0

##necesitamos quitar la columna "default" para entrenar el modelo
credit_model <- C5.0(credit_train[-17],credit_train$default)
credit_model
summary(credit_model)


####Step 4 – evaluating model performance####
credit_pred<- predict(credit_model, credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

###salen 59 bien predecidos que no se les da prestamo y 14 que si, mientras que hay 19 que se predice que no y 
###sería que si y 8 que se predice que si y sería que no. 

####Boosting the accuracy of decision trees####
##se añade la opcion trials en la funcion c5.0 para limitar el número de iteraciones 
##se aconseja no hacer menos de 10

credit_boost10<- C5.0(credit_train[-17], credit_train$default, trials = 10)

credit_boost10

summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)

CrossTable(credit_test$default, credit_boost_pred10, prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE 
           ,dnn = c('actual default', 'predicted default'))

####Making mistakes more costlier than others####
##generamos una matriz de costes de asignacion##
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
###
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,
                     dimnames = matrix_dimensions)
####en el error cost, un falso positivo cuesta 4 veces mas que el negativo
####se le añade al calulo la matriz de costes####
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                    costs = error_cost)