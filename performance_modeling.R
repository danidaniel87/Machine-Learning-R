##### Chapter 10: Evaluating Model Performance -------------------

## Create the predicted probabilities from the SMS classifier built in Chapter 4.
## NOTE: THIS SECTION WILL NOT RUN WITHOUT RUNNING THE CHAPTER 4 CODE TO CREATE THE SMS CLASSIFIER!

####Step 1 â collecting data####
SMSSpamCollection <- read.delim("C:/Users/Solutio/Desktop/smsspam/SMSSpamCollection.txt", header=FALSE, stringsAsFactors=FALSE)

####Data preparation â cleaning and standardizing text data#####
sms_raw<-SMSSpamCollection
names(sms_raw)<-c("type", "text")

sms_raw$type<-as.factor(sms_raw$type)

####Data preparation â cleaning and standardizing text data####
library("tm")
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

###para inspeccionar smsÂ´s###
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:5], as.character)

###transformamos todo a minÃºsculas####
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
as.character(sms_corpus_clean[[2]])

####elimino los numeros####
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

####eliminamos palabras que no sirven####
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords, stopwords())
stopwords(kind="sp") ###estas son las stopwords en español

####eliminamos puntuaciones####
replacePunctuation <- function(x) {
  gsub("[[:punct:]]+", " ", x)}

#replacePunctuation(as.character(sms_corpus_clean))
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
####pasamos las palabras a sus raices####
library("SnowballC")
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

#####Eliminamos espacios en blanco####
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

####vemos como quedan los 3 primeros sms antes y despues del tratamiento####
as.character(sms_corpus[[10]])
as.character(sms_corpus_clean[[10]])

####Data preparation - splitting text documents into words####
#####Creating a DTM sparse matrix####
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

####usamos esta funcion con el dataframe en bruto####
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))
#####To force the two prior document term matrices to be identical#####
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = function(x) { removeWords(x, stopwords()) },
  removePunctuation = TRUE,
  stemming = TRUE
))

#####Data preparation - creating training and test datasets####
sms_dtm_train <- sms_dtm[1:(length(sms_corpus_clean)*0.75), ]

sms_dtm_test <- sms_dtm[(length(sms_corpus_clean)*0.75+1):length(sms_corpus_clean), ]

sms_train_labels<-sms_raw[1:(length(sms_corpus_clean)*0.75), ]$type
sms_test_labels <- sms_raw[(length(sms_corpus_clean)*0.75+1):length(sms_corpus_clean), ]$type

###comprobamos que mantienen la proporción ambos splits####
prop.table(table(sms_test_labels))
prop.table(table(sms_train_labels))


####Visualizing text data - word clouds####
library("wordcloud")
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE,colors = c("red", "blue"))

####we'll create a subset where the message type is spam and other with ham####
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")          

####hacemos nubes de palabras para ham y spam####
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

####Data preparation - creating indicator features for frequent words####
sms_freq_words<-findFreqTerms(sms_dtm_train, 5)

####we want all the rows, but only the columns representing the words in the sms_freq_words vector####
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

####The commands to convert the training and test matrices are as follows:####
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

##############Step 3 - training a model on the data#############
library("e1071")

####To build our model on the sms_train matrix, we'll use the following command:####
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

#####Step 4 - evaluating model performance####
sms_test_pred <- predict(sms_classifier, sms_test)

####comparamos los valores que nos da con los reales####
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, 
           prop.t = FALSE,
           dnn = c('predicted', 'actual'))

####Step 5 - improving model performance#####
####We'll build a Naive Bayes model as done earlier, but this time set laplace = 1####
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,laplace = 1)

sms_test_pred2 <- predict(sms_classifier2, sms_test)

CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

# uncomment this line to output the sms_results to CSV
# write.csv(sms_results, "sms_results.csv", row.names = FALSE)

## Confusion matrixes in R ----
sms_results <- read.csv("sms_results.csv")

# the first several test cases
head(sms_results)

# test cases where the model is less confident
head(subset(sms_results, prob_spam > 0.40 & prob_spam < 0.60))

# test cases where the model was wrong
head(subset(sms_results, actual_type != predict_type))

# specifying vectors
table(sms_results$actual_type, sms_results$predict_type)

# alternative solution using the formula interface (not shown in book)
xtabs(~ actual_type + predict_type, sms_results)

# using the CrossTable function
library(gmodels)
crossTable<-CrossTable(sms_results$actual_type, sms_results$predict_type)

# accuracy and error rate calculation --
# accuracy
(152 + 1203) / (152 + 1203 + 4 + 31)
# error rate
(4 + 31) / (152 + 1203 + 4 + 31)
# error rate = 1 - accuracy
1 - 0.9748201

## Beyond accuracy: other performance measures ----
library(caret)
confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# Kappa statistic
# example using SMS classifier
pr_a <- 0.865 + 0.109
pr_a

pr_e <- 0.868 * 0.888 + 0.132 * 0.112
pr_e

k <- (pr_a - pr_e) / (1 - pr_e)
k

# calculate kappa via the vcd package
library(vcd)
Kappa(table(sms_results$actual_type, sms_results$predict_type))

# calculate kappa via the irr package
library(irr)
kappa2(sms_results[1:2])

# Sensitivity and specificity
# example using SMS classifier
sens <- 152 / (152 + 31)
sens

spec <- 1203 / (1203 + 4)
spec

# example using the caret package
library(caret)
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type, negative = "ham")

# Precision and recall
prec <- 152 / (152 + 4)
prec

rec <- 152 / (152 + 31)
rec

# example using the caret package
library(caret)
posPredValue(sms_results$predict_type, sms_results$actual_type, positive = "spam")
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# F-measure
f <- (2 * prec * rec) / (prec + rec)
f

f <- (2 * 152) / (2 * 152 + 4 + 31)
f

## Visualizing Performance Tradeoffs ----
library(ROCR)
pred <- prediction(predictions = sms_results$prob_spam,
                   labels = sms_results$actual_type)

# ROC curves
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve for SMS spam filter", col = "blue", lwd = 2)

# add a reference line to the graph
abline(a = 0, b = 1, lwd = 2, lty = 2)

# calculate AUC
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)

## Estimating Future Performance ----

# partitioning data
library(caret)
credit <- read.csv("credit.csv")

# Holdout method
# using random IDs
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500],]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

# using caret function
in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
credit_train <- credit[in_train, ]
credit_test <- credit[-in_train, ]

# 10-fold CV
folds <- createFolds(credit$default, k = 10)
str(folds)
credit01_test <- credit[folds$Fold01, ]
credit01_train <- credit[-folds$Fold01, ]

## Automating 10-fold CV for a C5.0 Decision Tree using lapply() ----
library(caret)
library(C50)
library(irr)

credit <- read.csv("credit.csv")

set.seed(123)
folds <- createFolds(credit$default, k = 10)

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[-x, ]
  credit_test <- credit[x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)
mean(unlist(cv_results))

