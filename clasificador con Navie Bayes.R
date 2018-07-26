####Probabilistic Learning – Classification Using Naive Bayes####
####Step 1 – collecting data####
SMSSpamCollection <- read.delim("C:/Users/Solutio/Desktop/smsspam/SMSSpamCollection.txt", header=FALSE, stringsAsFactors=FALSE)

####Data preparation – cleaning and standardizing text data#####
sms_raw<-SMSSpamCollection
names(sms_raw)<-c("type", "text")

sms_raw$type<-as.factor(sms_raw$type)

####Data preparation – cleaning and standardizing text data####
library("tm")
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

###para inspeccionar sms´s###
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:5], as.character)

###transformamos todo a minúsculas####
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
as.character(sms_corpus_clean[[2]])

####elimino los numeros####
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

####eliminamos palabras que no sirven####
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords, stopwords())
stopwords(kind="sp") ###estas son las stopwords en espa�ol

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

###comprobamos que mantienen la proporci�n ambos splits####
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
