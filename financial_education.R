# Loading Necessary Libraries
library(utils)
library(tm)

# Loading datasets
fin_edu <- read.csv("financial-education-general.csv", na.strings = c("/", ""))

# Metadata
non_na_proportion <- colMeans(!is.na(fin_edu))
avg_confidence <- round(mean(fin_edu$Confidence.Level, na.rm = TRUE), 3)
median_confidence <- median(fin_edu$Confidence.Level, na.rm = TRUE)

calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate the mode of Confidence.Level column
mode_confidence <- calculate_mode(fin_edu$Confidence.Level[!is.na(fin_edu$Confidence.Level)])

#### #### Word Frequency Analysis

# Convert comments to lowercase for consistency
fin_edu$Comments <- tolower(fin_edu$Comments)

# Create a Corpus
corpus <- Corpus(VectorSource(fin_edu$Comments))

# Preprocess text: remove punctuation, numbers, and common English stop words
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Convert to a term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Convert term-document matrix to a matrix of word frequencies
word_freq <- as.matrix(tdm)

# Calculate total frequency of each word
total_freq <- rowSums(word_freq)

# Get top N words (excluding common stop words)
top_n_words <- head(sort(total_freq, decreasing = TRUE), 10)
print(top_n_words)


#### test code - extract key word

# Define the sentence
sentence <- "It showed me that my peers also feel similarly to me about college finances"

# Create a corpus from the sentence
corpus <- Corpus(VectorSource(sentence))

# Preprocessing: Convert to lowercase, remove punctuation, and remove stopwords
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Convert to a document term matrix (dtm)
dtm <- DocumentTermMatrix(corpus)

# Convert dtm to a matrix
term_matrix <- as.matrix(dtm)

# Calculate word frequencies
word_freq <- colSums(term_matrix)

# Sort words by frequency in descending order
sorted_words <- sort(word_freq, decreasing = TRUE)

# Extract top keywords (e.g., top 5)
top_keywords <- names(sorted_words)[1:5]

# Print the top keywords
print(top_keywords)

# Install and load required packages
install.packages("quanteda")
library(quanteda)

# Define the sentence
sentence <- "It showed me that my peers also feel similarly to me about college finances"

# Tokenize the sentence into words
tokens <- tokens(sentence, remove_punct = TRUE, remove_numbers = TRUE, 
                 remove_symbols = TRUE, remove_twitter = TRUE, 
                 remove_hyphens = TRUE, remove_url = TRUE)

# Convert tokens to a document-feature matrix
dfm <- dfm(tokens)

# Remove stopwords
dfm <- dfm_remove(dfm, stopwords("en"))

# Calculate tf-idf scores
dfm_tfidf <- dfm_tfidf(dfm)

# Extract keywords based on tf-idf scores
keywords <- topfeatures(dfm_tfidf, n = 5)

# Print the top keywords
print(keywords)

