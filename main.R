# Libraries
pacman::p_load(tidyverse,
               tidytext,
               lubridate,
               dplyr,
               tidyr,
               magrittr,
               data.table,
               stringr,
               readr,
               patchwork)

# Load data
ai_data <- read_csv("data/20240124_AI_Assessment_Data.csv")
ai_data$answer_number <- as.numeric(ai_data$answer_number )

# Combine dataframes
source("scripts/survey_plots.R")


