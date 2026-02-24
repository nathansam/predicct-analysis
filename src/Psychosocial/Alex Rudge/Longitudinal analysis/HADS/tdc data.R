library(tidyverse)
library(magrittr)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/HADS/")

source("data_cleaning.R")

# Anxiety ####
## Soft ####

data_anxiety_soft_static <- data_anxiety_soft %>%
  dplyr::select(
    ParticipantNo, SiteNo, diagnosis2, Sex, age_decade, FC, Smoke, IMD, time, DiseaseFlareYN
  ) %>%
  # Remove people with time 0
  dplyr::filter(
    time > 0
  )

data_anxiety_soft_td <- data_anxiety_soft_long %>%
  dplyr::select(
    ParticipantNo, month, anxiety_hads
  ) %>%
  # Last observation carried forward
  dplyr::arrange(ParticipantNo, month) %>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(anxiety_hads, .direction = "down") %>%
  # Convert month to days to match time
  dplyr::mutate(
    month = as.numeric(month),
    day = 365*month/12) %>%
  # Anxiety as categorical
  dplyr::mutate(
    score_group = cut(
      anxiety_hads,
      breaks = c(0, 7, 21),
      labels = c("0-7", "8-21"),
      include.lowest = TRUE
    )
  )

# Start stop
data_anxiety_soft_merged <- tmerge(
  data_anxiety_soft_static, 
  data_anxiety_soft_static, 
  id = ParticipantNo, 
  endpoint = event(time, DiseaseFlareYN)) %>%
  tmerge(
    data2 = data_anxiety_soft_td,
    id = ParticipantNo,
    score_group = tdc(day, score_group)
  )




## Hard ####
data_anxiety_hard_static <- data_anxiety_hard %>%
  dplyr::select(
    ParticipantNo, SiteNo, diagnosis2, Sex, age_decade, FC, Smoke, IMD, time, DiseaseFlareYN
  ) %>%
  # Remove people with time 0
  dplyr::filter(
    time > 0
  )

data_anxiety_hard_td <- data_anxiety_hard_long %>%
  dplyr::select(
    ParticipantNo, month, anxiety_hads
  ) %>%
  # Last observation carried forward
  dplyr::arrange(ParticipantNo, month) %>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(anxiety_hads, .direction = "down") %>%
  # Convert month to days to match time
  dplyr::mutate(
    month = as.numeric(month),
    day = 365*month/12) %>%
  # Anxiety as categorical
  dplyr::mutate(
    score_group = cut(
      anxiety_hads,
      breaks = c(0, 7, 21),
      labels = c("0-7", "8-21"),
      include.lowest = TRUE
    )
  )

# Start stop
data_anxiety_hard_merged <- tmerge(
  data_anxiety_hard_static, 
  data_anxiety_hard_static, 
  id = ParticipantNo, 
  endpoint = event(time, DiseaseFlareYN)) %>%
  tmerge(
    data2 = data_anxiety_hard_td,
    id = ParticipantNo,
    score_group = tdc(day, score_group)
  )



# Depression ####
## Soft ####

data_depression_soft_static <- data_depression_soft %>%
  dplyr::select(
    ParticipantNo, SiteNo, diagnosis2, Sex, age_decade, FC, Smoke, IMD, time, DiseaseFlareYN
  ) %>%
  # Remove people with time 0
  dplyr::filter(
    time > 0
  )

data_depression_soft_td <- data_depression_soft_long %>%
  dplyr::select(
    ParticipantNo, month, depression_hads
  ) %>%
  # Last observation carried forward
  dplyr::arrange(ParticipantNo, month) %>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(depression_hads, .direction = "down") %>%
  # Convert month to days to match time
  dplyr::mutate(
    month = as.numeric(month),
    day = 365*month/12) %>%
  # depression as categorical
  dplyr::mutate(
    score_group = cut(
      depression_hads,
      breaks = c(0, 7, 21),
      labels = c("0-7", "8-21"),
      include.lowest = TRUE
    )
  )

# Start stop
data_depression_soft_merged <- tmerge(
  data_depression_soft_static, 
  data_depression_soft_static, 
  id = ParticipantNo, 
  endpoint = event(time, DiseaseFlareYN)) %>%
  tmerge(
    data2 = data_depression_soft_td,
    id = ParticipantNo,
    score_group = tdc(day, score_group)
  )



## Hard ####
data_depression_hard_static <- data_depression_hard %>%
  dplyr::select(
    ParticipantNo, SiteNo, diagnosis2, Sex, age_decade, FC, Smoke, IMD, time, DiseaseFlareYN
  ) %>%
  # Remove people with time 0
  dplyr::filter(
    time > 0
  )

data_depression_hard_td <- data_depression_hard_long %>%
  dplyr::select(
    ParticipantNo, month, depression_hads
  ) %>%
  # Last observation carried forward
  dplyr::arrange(ParticipantNo, month) %>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(depression_hads, .direction = "down") %>%
  # Convert month to days to match time
  dplyr::mutate(
    month = as.numeric(month),
    day = 365*month/12) %>%
  # depression as categorical
  dplyr::mutate(
    score_group = cut(
      depression_hads,
      breaks = c(0, 7, 21),
      labels = c("0-7", "8-21"),
      include.lowest = TRUE
    )
  )

# Start stop
data_depression_hard_merged <- tmerge(
  data_depression_hard_static, 
  data_depression_hard_static, 
  id = ParticipantNo, 
  endpoint = event(time, DiseaseFlareYN)) %>%
  tmerge(
    data2 = data_depression_hard_td,
    id = ParticipantNo,
    score_group = tdc(day, score_group)
  )
