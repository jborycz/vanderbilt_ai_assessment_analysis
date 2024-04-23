### Libraries
pacman::p_load(tidyverse,
               tidytext,
               tm,
               textdata,
               lubridate,
               dplyr,
               tidyr,
               magrittr,
               data.table,
               stringr,
               readr,
               ggrepel,
               patchwork,
               SentimentAnalysis)

### Load data
# Likert data
ai_data <- read_csv("data/20240124_AI_Assessment_Data.csv")
ai_data$answer_number <- as.numeric(ai_data$answer_number )
# Open response data and categories
ai_open_responses <- read_csv("data/20240404_AI_Assessment_Open_Responses.csv")
ai_open_responses$before_answer_number <- as.numeric(ai_open_responses$before_answer_number)
ai_open_responses$after_answer_number <- as.numeric(ai_open_responses$after_answer_number)

### Scripts
# This scripts computes the mean values of the likert responses from both survey questions
# ANOVA and TukeyHSD of these values are computed
# A plot comparing the results is then created
source("scripts/survey_plots.R")
# This script is for the sentiment analysis on the open response questions
# Plots are created for the limitations and uses open response questions
# Plots are created comparing the proportion of responses by category
source("scripts/open_response_analysis.R")


