library(tidyverse)
library(magrittr)


# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/HADS longitudinal/")

source("data_cleaning.R")


# How many patients have hads scores?
data_survival_anxiety_soft_long %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(
    n = sum(!is.na(anxiety_hads))
  )

data_survival_depression_soft_long %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(
    n = sum(!is.na(depression_hads))
  )


# Change in hads score over time

# Soft
# 0 to 12 months
# Anxiety
data_survival_anxiety_soft %>%
  dplyr::mutate(
    anxiety_change_0_12 = anxiety_hads_12 - anxiety_hads,
  ) %>%
  dplyr::mutate(
    flare_0_12 = dplyr::case_when(
      DiseaseFlareYN == 1 & time <= 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  ggplot(aes(x = anxiety_change_0_12, colour = flare_0_12)) +
  geom_density()

# Depression
data_survival_depression_soft %>%
  dplyr::mutate(
    depression_change_0_12 = depression_hads_12 - depression_hads,
  ) %>%
  dplyr::mutate(
    flare_0_12 = dplyr::case_when(
      DiseaseFlareYN == 1 & time <= 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  ggplot(aes(x = depression_change_0_12, colour = flare_0_12)) +
  geom_density()

# 12 to 24
# Anxiety
data_survival_anxiety_soft %>%
  dplyr::mutate(
    anxiety_change_12_24 = anxiety_hads_24 - anxiety_hads_12,
  ) %>%
  dplyr::mutate(
    flare_12_24 = dplyr::case_when(
      DiseaseFlareYN == 1 & time > 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  ggplot(aes(x = anxiety_change_12_24, colour = flare_12_24)) +
  geom_density()

# Depression
data_survival_depression_soft %>%
  dplyr::mutate(
    depression_change_12_24 = depression_hads_24 - depression_hads_12,
  ) %>%
  dplyr::mutate(
    flare_12_24 = dplyr::case_when(
      DiseaseFlareYN == 1 & time > 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  ggplot(aes(x = depression_change_12_24, colour = flare_12_24)) +
  geom_density()


# Hard
# 0 to 12 months
# Anxiety
data_survival_anxiety_hard %>%
  dplyr::mutate(
    anxiety_change_0_12 = anxiety_hads_12 - anxiety_hads,
  ) %>%
  dplyr::mutate(
    flare_0_12 = dplyr::case_when(
      DiseaseFlareYN == 1 & time <= 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  ggplot(aes(x = anxiety_change_0_12, colour = flare_0_12)) +
  geom_density()

# Depression
data_survival_depression_hard %>%
  dplyr::mutate(
    depression_change_0_12 = depression_hads_12 - depression_hads,
  ) %>%
  dplyr::mutate(
    flare_0_12 = dplyr::case_when(
      DiseaseFlareYN == 1 & time <= 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  ggplot(aes(x = depression_change_0_12, colour = flare_0_12)) +
  geom_density()

# 12 to 24
# Anxiety
data_survival_anxiety_hard %>%
  dplyr::mutate(
    anxiety_change_12_24 = anxiety_hads_24 - anxiety_hads_12,
  ) %>%
  dplyr::mutate(
    flare_12_24 = dplyr::case_when(
      DiseaseFlareYN == 1 & time > 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  ggplot(aes(x = anxiety_change_12_24, colour = flare_12_24)) +
  geom_density()

# Depression
data_survival_depression_hard %>%
  dplyr::mutate(
    depression_change_12_24 = depression_hads_24 - depression_hads_12,
  ) %>%
  dplyr::mutate(
    flare_12_24 = dplyr::case_when(
      DiseaseFlareYN == 1 & time > 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  ggplot(aes(x = depression_change_12_24, colour = flare_12_24)) +
  geom_density()




