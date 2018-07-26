########K-NN neighbours ##############
exams <- read.csv("C:/Users/Solutio/Desktop/exams.csv")
wbcd<-exams
#str(wbcd)
wbcd[,"id"]<-NULL
class(wbcd$diagnosis)
levels(wbcd$diagnosis)
factor(wbcd$diagnosis, levels = c("B", "M"), labels=c("Benign", "Malignant"))
wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),
                        labels = c("Benign", "Malignant"))
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
####hay que normalizar ya que hay mucha diferencia entre los valores#####

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
#######creating training and test datasets#####
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
#####guardamos las etiquetas de los diagnosticos en train y test tambien#####
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#####Step 3: training a model on the data####
library("class")
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

#######Step 4 - evaluating model performance#####
library("gmodels", lib.loc="~/R/win-library/3.4")
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

#######Step 5 - improving model performance#####
#####Transformation - z-score standardization###
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
#The mean of a z-score standardized variable should always be zero, and the range should be fairly compact.

####Divide the data into training and test sets####
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
             prop.chisq = FALSE)
