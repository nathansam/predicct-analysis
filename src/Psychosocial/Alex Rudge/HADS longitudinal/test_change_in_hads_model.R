library(tidyverse)
library(magrittr)
library(survminer)
library(patchwork)
library(lmerTest)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/HADS longitudinal/")

source("data_cleaning.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Theme
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


# Change in HADS score between 0 and 12 months.

data_anxiety_soft_raw <- data_anxiety_soft
data_anxiety_soft_long_raw <- data_anxiety_soft_long

# All patients with scores at 0 and 12 months and who were not censored before 12m
id_anxiety_soft <- data_anxiety_soft_raw %>%
  dplyr::filter(
    !is.na(anxiety_hads),
    !is.na(anxiety_hads_12),
    !((DiseaseFlareYN == 0) & (time <= 365))
  ) %>%
  dplyr::pull(ParticipantNo) %>%
  unique()

data_anxiety_soft_long_lme <- data_anxiety_soft_long_raw %>%
  # Flare between 0 and 12
  dplyr::mutate(
    flare0to12 = dplyr::case_when(
      (DiseaseFlareYN == 1) & time <= 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  # Only patients with data at 12m
  dplyr::filter(ParticipantNo %in% id_anxiety_soft) %>%
  # Do not need 24m
  dplyr::filter(month != 24) %>% 
  dplyr::select(ParticipantNo, month, anxiety_hads, flare0to12) %>%
  dplyr::mutate(month = as.numeric(month))


# LME
model <- lmer(anxiety_hads ~ month*flare0to12 + (1|ParticipantNo), data = data_anxiety_soft_long_lme)

summary(model)
  
anova(model)


ggplot(data_anxiety_soft_long_lme, aes(x = month, y = anxiety_hads, color = flare0to12, group = flare0to12)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  theme_minimal() +
  labs(title = "Change in Value Over Time by Group")
