# Loading Necessary Libraries
library(utils)
library(tm)
library(dplyr)
library(stringr)
library(stopwords)

# Loading datasets
joy_cha <- read.csv("joy-challenge.csv", na.strings = c("/", ""))