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
