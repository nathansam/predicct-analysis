library(tidyverse)
library(magrittr)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/HADS/")

source("data_cleaning.R")

custom_theme = theme_minimal() + 
  theme(
    title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "top"
  )

# How many have HADS at each time point
# Anxiety
data_anxiety_soft_long %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(
    n = sum(!is.na(anxiety_hads))
  ) %>%
  ggplot(aes(x = month, y = n)) +
  geom_col(fill = 'blue', colour = 'blue', alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, 1800, 200)) +
  xlab("Month") +
  ylab("Number of patients") +
  ggtitle("Number of responders to the HADS anxiety questionnaire") +
  custom_theme

# Depression is the same

