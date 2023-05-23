library(tm)
library(e1071)
library(caret)
library(dplyr)

# Load the data
patentdata2 <- read.csv("patentdata.csv", stringsAsFactors = FALSE)

patentdata2$industryCategory <- na_if(patentdata2$industryCategory, "")

# Split the data into training and testing sets
# Split the data into a training set and a test set
train_data <- subset(patentdata2, !is.na(industryCategory))
test_data <- subset(patentdata2, is.na(industryCategory))


# Create a corpus of the patent titles
corpus <- Corpus(VectorSource(train_data$title))

# Clean and preprocess the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Train the Naive Bayes model on the training data
nb_model <- naiveBayes(x = as.matrix(dtm), y = train_data$industryCategory)

# Evaluate the performance of the classifier on the test data
corpus_test <- Corpus(VectorSource(test_data$title))
corpus_test <- tm_map(corpus_test, content_transformer(tolower))
corpus_test <- tm_map(corpus_test, removeNumbers)
corpus_test <- tm_map(corpus_test, removeWords, stopwords("english"))
corpus_test <- tm_map(corpus_test, stripWhitespace)
dtm_test <- DocumentTermMatrix(corpus_test, control = list(dictionary = Terms(dtm)))
test_pred <- predict(nb_model, as.matrix(dtm_test))

# Add the predicted industry categories to the test data
test_data$industryCategory <- test_pred

# View the predicted industry categories
test_data$industryCategory
