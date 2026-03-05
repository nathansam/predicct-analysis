library(tidyverse)
library(magrittr)
library(survival)
library(gtsummary)
library(patchwork)


setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/")

filepath <- '/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Longitudinal analysis/'

# Load follow up data
data <- readr::read_rds(glue::glue("{filepath}followup.rds"))

palette <- c("#FFA500", "#0072B2", "#009E73")
# Repeat palette with better quotations


# Counts for yearly questionnaires

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


# Anxiety
data_counts_anxiety <- data %>%
  dplyr::filter(!is.na(anxiety_group)) %>%
  dplyr::count(month, anxiety_group) %>%
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n),
    # Mid point for a stacked bar
    mid = rev(cumsum(rev(n))) - n/2) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    month = forcats::as_factor(month),
    percent = glue::glue("{round(p*100)}%")
  )

plot_anxiety <- data_counts_anxiety %>%
  ggplot(aes(x = month, y = n, fill = anxiety_group)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 2000, 500),
    limits = c(0, 2000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Anxiety HADS") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5
    ) +
  custom_theme

plot_anxiety

# Depression
data_counts_depression <- data %>%
  dplyr::filter(!is.na(depression_group)) %>%
  dplyr::count(month, depression_group) %>%
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n),
    # Mid point for a stacked bar
    mid = rev(cumsum(rev(n))) - n/2) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    month = forcats::as_factor(month),
    percent = glue::glue("{round(p*100)}%")
  )

plot_depression <-data_counts_depression %>%
  ggplot(aes(x = month, y = n, fill = depression_group)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 2000, 500),
    limits = c(0, 2000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Depression HADS") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5) +
  custom_theme

plot_depression

# Somatisation
data_counts_somatisation <- data %>%
  dplyr::mutate(somatisation = forcats::fct_recode(somatisation, 'Moderate/Severe' = 'ModSev')) %>%
  dplyr::filter(!is.na(somatisation)) %>%
  dplyr::count(month, somatisation) %>%
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n),
    # Mid point for a stacked bar
    mid = rev(cumsum(rev(n))) - n/2
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    month = forcats::as_factor(month),
    percent = glue::glue("{round(p*100)}%")
  )

plot_somatisation <- data_counts_somatisation %>%
  ggplot(aes(x = month, y = n, fill = somatisation)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 2000, 500),
    limits = c(0, 2000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Somatisation") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5) +
  custom_theme

plot_somatisation

# Sleep Disturbance
data_counts_sleep <- data %>%
  dplyr::filter(!is.na(SleepDisturbance)) %>%
  dplyr::count(month, SleepDisturbance) %>%
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n),
    # Mid point for a stacked bar
    mid = rev(cumsum(rev(n))) - n/2
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    month = forcats::as_factor(month),
    percent = glue::glue("{round(p*100)}%")
  )

plot_sleep <- data_counts_sleep %>%
  ggplot(aes(x = month, y = n, fill = SleepDisturbance)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 2000, 500),
    limits = c(0, 2000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Sleep Disturbance") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5) +
  custom_theme

plot_sleep

# Exercise
data_counts_exercise <- data %>%
  dplyr::filter(!is.na(MinimumExercise)) %>%
  dplyr::count(month, MinimumExercise) %>%
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n),
    # Mid point for a stacked bar
    mid = rev(cumsum(rev(n))) - n/2
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    month = forcats::as_factor(month),
    percent = glue::glue("{round(p*100)}%")
  )

plot_exercise <- data_counts_exercise %>%
  ggplot(aes(x = month, y = n, fill = MinimumExercise)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 2000, 500),
    limits = c(0, 2000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Meets recommended exercise") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5) +
  custom_theme

plot_exercise



# Combine
plot_anxiety + plot_depression + plot_somatisation + plot_sleep + plot_exercise +
  patchwork::plot_layout(
    ncol = 2) &
  patchwork::plot_annotation(
    title = 'Number of respondents to yearly questionnaires'
  ) &
  theme(plot.title = element_text(size = 14, hjust = 0.5))


# Save pdf 8 x 10 portrait