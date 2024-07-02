# Loading Necessary Libraries
library(utils)
library(tm)
library(dplyr)
library(stringr)
library(stopwords)
library(graphics)
library(RColorBrewer)
library(ggplot2)

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

# Confidence level visualization
ggplot(fin_edu, aes(x = Confidence.Level)) +
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
  labs(x = "Confidence Level", y = "Frequency", 
       title = "Histogram of Confidence Levels")

# Comparative Analysis
# Create a new column indicating presence of comments
fin_edu$Comments_Present <- ifelse(!is.na(fin_edu$Comments), "Yes", "No")

# Compare confidence levels based on presence of comments
ggplot(fin_edu, aes(x = Comments_Present, y = Confidence.Level, fill = Comments_Present)) +
  geom_boxplot() +
  labs(x = "Comments Present", y = "Confidence Level", 
       title = "Comparison of Confidence Levels based on Comments Presence")


# Perform two-sample t-test
t_test <- t.test(Confidence.Level ~ Comments_Present, data = fin_edu)

# Print the results
print(t_test)

# Perform Wilcoxon rank-sum test (Mann-Whitney U test)
wilcox_test <- wilcox.test(Confidence.Level ~ Comments_Present, data = fin_edu)

# Print the results
print(wilcox_test)

# Create a new column indicating presence of questions
fin_edu$Question_Present <- ifelse(!is.na(fin_edu$Questions.Preventing.Action), "Yes", "No")

# Compare confidence levels based on presence of questions
ggplot(fin_edu, aes(x = Question_Present, y = Confidence.Level, fill = Question_Present)) +
  geom_boxplot() +
  labs(x = "Question Present", y = "Confidence Level", 
       title = "Comparison of Confidence Levels based on Question Presence")

# Create a binary column indicating overcome obstacles
fin_edu$OO_Present <- ifelse(!is.na(fin_edu$Overcome.Obstacles), "Yes", "No")

# Compare confidence levels based on presence of overcome obstacles
ggplot(fin_edu, aes(x = OO_Present, y = Confidence.Level, fill = OO_Present)) +
  geom_boxplot() +
  labs(x = "Question Present", y = "Confidence Level", 
       title = "Comparison of Confidence Levels based on Overcome Obstacles")


#### Comment Analysis

# Define the categories to count
categories <- c("Model", "Content", "Extension", "Staff", "Instrument")

# Initialize a named vector to store counts
category_counts <- setNames(rep(0, length(categories)), categories)


# Count occurrences of each category
for (cat in fin_edu$Category) {
# Split by delimiter and convert to lower case
  cat_split <- str_split(cat, "/")[[1]]
# Increment counts for each category found
  for (sub_cat in cat_split) {
    if (sub_cat %in% categories) {
      category_counts[sub_cat] <- category_counts[sub_cat] + 1
    }
  }
}

# Convert the named vector to a data frame for better readability
category_counts_df <- as.data.frame(as.table(category_counts))
colnames(category_counts_df) <- c("Category", "Count")

# Print the category counts
print(category_counts_df)

#### #### Pie Chart

# Define colors to use from a palette
colors <- brewer.pal(nrow(category_counts_df), "Set3")

# Plotting a pie chart with specified colors
pie(category_counts_df$Count, labels = category_counts_df$Category,
    col = colors,
    main = "Distribution of Comment Categories")

# Adding a legend with specified colors
legend("topright", legend = category_counts_df$Category,
       fill = colors,
       title = "Categories",
       cex = 0.8)



# Combine all sentences into a single string
cb_compliment <- paste(key_compliment, collapse = " ")

# Convert text to lower case
cb_compliment <- tolower(cb_compliment)

# Remove punctuation
cb_compliment <- str_replace_all(cb_compliment, "[[:punct:]]", "")

# Split the text into individual words
words <- str_split(cb_compliment, "\\s+")[[1]]

# Remove common stop words
stop_words <- stopwords("en")
words <- words[!words %in% stop_words]

# Create a table of word frequencies
word_freq <- table(words)

# Convert to data frame and sort by frequency
word_freq_df <- as.data.frame(word_freq, stringsAsFactors = FALSE)
word_freq_df <- word_freq_df %>% arrange(desc(Freq))

# Print the most common words
print(word_freq_df)

# good example *3
# compliments on the presenter


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

