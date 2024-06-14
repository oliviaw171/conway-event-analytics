# Loading Necessary Libraries
library(utils)

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
