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
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "top",
    plot.tag.position = c(0.95, 0.80),
    plot.tag = element_text(size = 12, hjust = 1, vjust = 1)
    
  )


# Anxiety
data_counts_anxiety <- data %>%
  dplyr::filter(!is.na(anxiety_group)) %>%
  dplyr::count(month, diagnosis2, anxiety_group) %>%
  dplyr::group_by(month, diagnosis2) %>%
  dplyr::mutate(
    p = n/sum(n),
    # Mid point for a stacked bar
    mid = rev(cumsum(rev(n))) - n/2) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    month = forcats::as_factor(month),
    percent = glue::glue("{round(p*100)}%")
  )

plot_anxiety_cd <- data_counts_anxiety %>%
  dplyr::filter(diagnosis2 == 'CD') %>%
  ggplot(aes(x = month, y = n, fill = anxiety_group)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 1000, 250),
    limits = c(0, 1000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(
    fill = "Anxiety HADS",
    tag = "Crohn's disease"
    ) +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5
    ) +
  custom_theme

plot_anxiety_cd

plot_anxiety_uc <- data_counts_anxiety %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU') %>%
  ggplot(aes(x = month, y = n, fill = anxiety_group)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 1000, 250),
    limits = c(0, 1000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Anxiety HADS",
       tag = "Ulcerative Colitis/IBDU") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5
  ) +
  custom_theme

plot_anxiety_uc


# Depression
data_counts_depression <- data %>%
  dplyr::filter(!is.na(depression_group)) %>%
  dplyr::count(month, diagnosis2, depression_group) %>%
  dplyr::group_by(month, diagnosis2) %>%
  dplyr::mutate(
    p = n/sum(n),
    # Mid point for a stacked bar
    mid = rev(cumsum(rev(n))) - n/2) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    month = forcats::as_factor(month),
    percent = glue::glue("{round(p*100)}%")
  )

plot_depression_cd <- data_counts_depression %>%
  dplyr::filter(diagnosis2 == 'CD') %>%
  ggplot(aes(x = month, y = n, fill = depression_group)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 1000, 250),
    limits = c(0, 1000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Depression HADS",
       tag = "Crohn's disease") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5) +
  custom_theme

plot_depression_cd

plot_depression_uc <- data_counts_depression %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU') %>%
  ggplot(aes(x = month, y = n, fill = depression_group)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 1000, 250),
    limits = c(0, 1000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Depression HADS",
       tag = "Ulcerative Colitis/IBDU") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5) +
  custom_theme

plot_depression_uc

# Somatisation
data_counts_somatisation <- data %>%
  dplyr::mutate(somatisation = forcats::fct_recode(somatisation, 'Moderate/Severe' = 'ModSev')) %>%
  dplyr::filter(!is.na(somatisation)) %>%
  dplyr::count(month, diagnosis2, somatisation) %>%
  dplyr::group_by(month, diagnosis2) %>%
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

plot_somatisation_cd <- data_counts_somatisation %>%
  dplyr::filter(diagnosis2 == 'CD') %>%
  ggplot(aes(x = month, y = n, fill = somatisation)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 1000, 250),
    limits = c(0, 1000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Somatisation",
       tag = "Crohn's disease") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5) +
  custom_theme

plot_somatisation_cd

plot_somatisation_uc <- data_counts_somatisation %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU') %>%
  ggplot(aes(x = month, y = n, fill = somatisation)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 1000, 250),
    limits = c(0, 1000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Somatisation",
       tag = "Ulcerative Colitis/IBDU") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5) +
  custom_theme

plot_somatisation_uc

# Sleep Disturbance
data_counts_sleep <- data %>%
  dplyr::filter(!is.na(SleepDisturbance)) %>%
  dplyr::count(month, diagnosis2 ,SleepDisturbance) %>%
  dplyr::group_by(month, diagnosis2) %>%
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

plot_sleep_cd <- data_counts_sleep %>%
  dplyr::filter(diagnosis2 == 'CD') %>%
  ggplot(aes(x = month, y = n, fill = SleepDisturbance)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 1000, 250),
    limits = c(0, 1000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Sleep Disturbance",
       tag = "Crohn's disease") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5) +
  custom_theme

plot_sleep_cd


plot_sleep_uc <- data_counts_sleep %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU') %>%
  ggplot(aes(x = month, y = n, fill = SleepDisturbance)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 1000, 250),
    limits = c(0, 1000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Sleep Disturbance",
       tag = "Ulcerative Colitis/IBDU") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5) +
  custom_theme

plot_sleep_uc


# Exercise
data_counts_exercise <- data %>%
  dplyr::filter(!is.na(MinimumExercise)) %>%
  dplyr::count(month, diagnosis2 ,MinimumExercise) %>%
  dplyr::group_by(month, diagnosis2) %>%
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

plot_exercise_cd <- data_counts_exercise %>%
  dplyr::filter(diagnosis2 == 'CD') %>%
  ggplot(aes(x = month, y = n, fill = MinimumExercise)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 1000, 250),
    limits = c(0, 1000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Meets recommended exercise",
       tag = "Crohn's disease") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5) +
  custom_theme

plot_exercise_cd


plot_exercise_uc <- data_counts_exercise %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU') %>%
  ggplot(aes(x = month, y = n, fill = MinimumExercise)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 1000, 250),
    limits = c(0, 1000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Meets recommended exercise",
       tag = "Ulcerative Colitis/IBDU") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3.5) +
  custom_theme

plot_exercise_uc

# Combine
plot_anxiety_cd + plot_anxiety_uc +
  plot_depression_cd + plot_depression_uc +
  plot_somatisation_cd + plot_somatisation_uc + 
  plot_sleep_cd + plot_sleep_uc +
  plot_exercise_cd + plot_exercise_uc +
  patchwork::plot_layout(
    ncol = 2) &
  patchwork::plot_annotation(
    title = 'Number of respondents to yearly questionnaires'
  ) &
  theme(plot.title = element_text(size = 14, hjust = 0.5))


# Save pdf 13 x 10 portrait