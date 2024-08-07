# Loading Necessary Libraries
library(utils)
library(tm)
library(dplyr)
library(stringr)
library(stopwords)
library(ggplot2)

# Loading datasets
joy_cha <- read.csv("joy-challenge.csv", na.strings = c("/", ""))

# Rename columns
colnames(joy_cha) <- c(
  "decision_of_participation",
  "sufficient_communication",
  "understood_evaluation",
  "seed_funds_helpful",
  "where_for_help",
  "avg_proj_time_perweek",
  "showcase_2.5hr",
  "talk_4min",
  "event_1hr",
  "what_worked_well",
  "want_difference",
  "improve_experience",
  "invite_outside_smith"
)

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

#### Visualization

# Function to create pie chart for categorical variables
create_pie_chart <- function(data, column_name) {
  # Create a data frame with the counts of each unique value
  df <- data %>%
    group_by(!!sym(column_name)) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    arrange(desc(count))
  
  # Create the pie chart
  pie_chart <- ggplot(df, aes(x = "", y = percentage, fill = !!sym(column_name))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    ggtitle(paste("Distribution of", column_name)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(fill = column_name)
  
  # Print the pie chart
  print(pie_chart)
}

# Identify categorical variables, excluding columns with long answers
categorical_vars <- setdiff(
  names(Filter(is.character, joy_cha)),
  c("what_worked_well", "want_difference", "improve_experience")
)

# Create pie charts for each categorical variable
for (var in categorical_vars) {
  create_pie_chart(joy_cha, var)
}

