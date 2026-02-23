library(tidyverse)
library(magrittr)
library(survival)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal data analysis/HADS/")

source("data_cleaning.R")

# Anxiety ####
## Soft ####
data_anxiety_soft %<>%
  # Remove patients with an event before 12 months
  dplyr::filter(
    time > 365
  ) %>%
  # 12 months is now baseline
  dplyr::mutate(
    time = time - 365
  )

# LOCF
data_anxiety_soft %<>%
  dplyr::mutate(
    anxiety_hads_locf = dplyr::coalesce(anxiety_hads_12, anxiety_hads)
  )

# Score group
data_anxiety_soft %<>%
  dplyr::mutate(
    score_group_locf = cut(
      anxiety_hads_locf,
      breaks = c(0, 7, 21),
      labels = c("0-7", "8-21"),
      include.lowest = TRUE
    )
  )

## Hard ####
data_anxiety_hard %<>%
  # Remove patients with an event before 12 months
  dplyr::filter(
    time > 365
  ) %>%
  # 12 months is now baseline
  dplyr::mutate(
    time = time - 365
  )

# LOCF
data_anxiety_hard %<>%
  dplyr::mutate(
    anxiety_hads_locf = dplyr::coalesce(anxiety_hads_12, anxiety_hads)
  )

# Score group
data_anxiety_hard %<>%
  dplyr::mutate(
    score_group_locf = cut(
      anxiety_hads_locf,
      breaks = c(0, 7, 21),
      labels = c("0-7", "8-21"),
      include.lowest = TRUE
    )
  )


# Depression ####
## Soft ####
data_depression_soft %<>%
  # Remove patients with an event before 12 months
  dplyr::filter(
    time > 365
  ) %>%
  # 12 months is now baseline
  dplyr::mutate(
    time = time - 365
  )

# LOCF
data_depression_soft %<>%
  dplyr::mutate(
    depression_hads_locf = dplyr::coalesce(depression_hads_12, depression_hads)
  )

# Score group
data_depression_soft %<>%
  dplyr::mutate(
    score_group_locf = cut(
      depression_hads_locf,
      breaks = c(0, 7, 21),
      labels = c("0-7", "8-21"),
      include.lowest = TRUE
    )
  )

## Hard ####
data_depression_hard %<>%
  # Remove patients with an event before 12 months
  dplyr::filter(
    time > 365
  ) %>%
  # 12 months is now baseline
  dplyr::mutate(
    time = time - 365
  )

# LOCF
data_depression_hard %<>%
  dplyr::mutate(
    depression_hads_locf = dplyr::coalesce(depression_hads_12, depression_hads)
  )

# Score group
data_depression_hard %<>%
  dplyr::mutate(
    score_group_locf = cut(
      depression_hads_locf,
      breaks = c(0, 7, 21),
      labels = c("0-7", "8-21"),
      include.lowest = TRUE
    )
  )

