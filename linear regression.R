launch <- read.csv("C:/Users/Solutio/Desktop/Machine Learning with R/launch.csv.txt", stringsAsFactors=FALSE)

b <- cov(launch$temperature, launch$distress_ct)/var(launch$temperature)

a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a

r <- cov(launch$temperature, launch$distress_ct) /
  (sd(launch$temperature) * sd(launch$distress_ct))
r

reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}

str(launch)

reg(y = launch$distress_ct, x = launch[2])
reg(y = launch$distress_ct, x = launch[2:4])


#####Example – predicting medical expenses using linear regression#####
####Step 1 – collecting data####
insurance <- read.csv("C:/Users/Solutio/Desktop/Machine Learning with R/insurance.csv")

##expenses is our dependent variable, we see the distribution of the variable
summary(insurance$expenses)
hist(insurance$expenses)

####Exploring relationships among features – the correlation matrix####
cor(insurance[c("age", "bmi", "children", "expenses")])

####Visualizing relationships among features – the scatterplot matrix####
pairs(insurance[c("age", "bmi", "children", "expenses")])
library("psych")
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

####Step 3 – training a model on the data####

ins_model <- lm(expenses ~ age + children + bmi + sex +
                  smoker + region, data = insurance)
ins_model <- lm(expenses ~ ., data = insurance)

ins_model

####Step 4 – evaluating model performance####
summary(ins_model)

#1 The residuals
#2 the p-value
#3 The multiple R-squared value

####Step 5 – improving model performance As mentioned####
##Model specification – adding non-linear relationships
#add a non-linear age variable
insurance$age2 <- insurance$age^2

####Transformation – converting a numeric variable to a binary indicator
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

####Model specification – adding interaction effects
####Putting it all together – an improved regression model
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)

summary(ins_model2)

####Example – estimating the quality of wines with regression trees and model trees####
####Step 1 – collecting data
redwines <- read.csv("C:/Users/Solutio/Desktop/Machine Learning with R/redwines.csv")
whitewines <- read.csv("C:/Users/Solutio/Desktop/Machine Learning with R/whitewines.csv")
wine <-whitewines
####Step 2 – exploring and preparing the data
str(wine)

hist(wine$quality)
summary(wine)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

####Step 3 – training a model on the data
library(rpart)
library(rpart.plot)
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart
summary(m.rpart)

##Visualizing decision trees
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)

####Step 4 – evaluating model performance####
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)###vemos si se van mucho los resultados (en este caso se van el min y max)

cor(p.rpart, wine_test$quality)##vemos la correlacion

##Measuring performance with the mean absolute error


