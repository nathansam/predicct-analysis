library(tidyverse)
library(magrittr)
library(survival)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/")

source("Fatigue/data_cleaning.R")
source("Life Events/data_cleaning.R")

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

palette <- c("#FFA500", "#0072B2", "#009E73")

# Combine fatigue and life events

fatigue %<>%
  dplyr::select(ParticipantNo, month, OftenLackEnergy)

lifeevents %<>%
  dplyr::select(ParticipantNo, month, AnyLifeEvents)

# Combine

data <- fatigue %>%
  dplyr::full_join(lifeevents, by = c('ParticipantNo', 'month'))


# Total number of responses per month
data %>%
  dplyr::count(month) %>%
  dplyr::mutate(month = forcats::as_factor(month)) %>%
  ggplot(aes(x = month, y = n)) +
  geom_col(colour = 'blue', fill = 'blue', alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, 2000, 250)) +
  xlab("Month") +
  ylab("Number of participant responses") +
  ggtitle("Number of questionnaires completed per month") +
  custom_theme


# Fatigued per month
# With percentages as labels?

data_counts_fatigue <- data %>%
  dplyr::filter(!is.na(OftenLackEnergy)) %>%
  dplyr::count(month, OftenLackEnergy) %>%
  # Calculate %
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


plot_fatigue <- data_counts_fatigue %>%
  ggplot(aes(x = month, y = n, fill = OftenLackEnergy)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 2000, 500),
    limits = c(0, 2000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Often feels fatigued in last month") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3
    ) +
  custom_theme

plot_fatigue

# Same for Life Events
data_counts_life <- data %>%
  dplyr::filter(!is.na(AnyLifeEvents)) %>%
  dplyr::count(month, AnyLifeEvents) %>%
  # Calculate %
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

plot_life <- data_counts_life %>%
  ggplot(aes(x = month, y = n, fill = AnyLifeEvents)) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 2000, 500),
    limits = c(0, 2000)
  ) +
  scale_fill_manual(values = palette) +
  xlab("Month") +
  ylab("Count") +
  labs(fill = "Any significant life events in last month") +
  #ggtitle("Number of questionnaires completed per month") +
  # Labels
  geom_text(
    aes(x = month, y = mid, label = percent), 
    size = 3
    ) +
  custom_theme

plot_life


# Combine together
plot_fatigue + plot_life +
  patchwork::plot_layout(
    ncol = 1,
    axes = 'collect') &
  patchwork::plot_annotation(
      title = 'Number of respondents to monthly questionnaires'
    ) &
  theme(plot.title = element_text(size = 14, hjust = 0.5))

# save pdf 8*10