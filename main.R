# Libraries
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

# Load data
ai_data <- read_csv("data/20240124_AI_Assessment_Data.csv")
ai_data$answer_number <- as.numeric(ai_data$answer_number )
ai_open_responses <- read_csv("data/20240404_AI_Assessment_Open_Responses.csv")
ai_open_responses$before_answer_number <- as.numeric(ai_open_responses$before_answer_number)
ai_open_responses$after_answer_number <- as.numeric(ai_open_responses$after_answer_number)

# Scripts
source("scripts/survey_plots.R")
source("scripts/open_response_analysis.R")


