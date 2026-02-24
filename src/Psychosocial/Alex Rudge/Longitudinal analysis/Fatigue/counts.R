library(tidyverse)
library(magrittr)
library(survival)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/Fatigue/")

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

# How many responded each month?
data_soft_long %>%
  dplyr::count(month) %>%
  dplyr::mutate(month = forcats::as_factor(month)) %>%
  ggplot(aes(x = month, y = n)) +
  geom_col(colour = 'blue',, fill = 'blue', alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, 2000, 250)) +
  xlab("Month") +
  ylab("Number of participant responses") +
  ggtitle("Number of questionnaires completed per month") +
  custom_theme

# Percentage reporting fatigue per month
data_soft_long %>%
  dplyr::count(month, OftenLackEnergy) %>%
  dplyr::mutate(month = forcats::as_factor(month)) %>%
  ggplot(aes(x = month, y = n, fill = OftenLackEnergy)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, 2000, 250)) +
  xlab("Month") +
  ylab("Number of participant responses") +
  ggtitle("Number of questionnaires completed per month") +
  custom_theme


# How many people responded to n questionnaires
data_soft_long %>%
  dplyr::group_by(ParticipantNo) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::group_by(n) %>%
  count(n) %>%
  dplyr::mutate(n = forcats::as_factor(n)) %>%
  ggplot(aes(x = n, y = nn)) +
  geom_col(colour = 'blue',, fill = 'blue', alpha = 0.5) +
  xlab("Number of monthly questionnaires completed") +
  ylab("Number of participants") +
  ggtitle("Number of questionnaires completed per person") +
  custom_theme


