# Loading Necessary Libraries
library(utils)
library(tm)
library(dplyr)
library(stringr)
library(stopwords)

# Loading datasets
joy_cha <- read.csv("joy-challenge.csv", na.strings = c("/", ""))

# Initialize a list to store metadata
joy_meta <- list()

# Function to generate metadata for each column
generate_metadata <- function(column) {
  if (is.numeric(column)) {
    return(list(
      type = "numeric",
      count = length(column),
      avg = mean(column, na.rm = TRUE),
      median = median(column, na.rm = TRUE)
    ))
  } else {
    return(list(
      type = "categorical",
      count = length(column),
      unique_values = length(unique(column))
    ))
  }
}

# Apply the function to each column and store the results in metadata list
joy_meta <- lapply(joy_cha, generate_metadata)
