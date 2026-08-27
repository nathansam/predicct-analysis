library(tidyverse)
library(magrittr)


# Load in all the Cox results

filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Primary analysis/"

# Suffix - cc (complete case) or mice

suffix <- "_cc.rds"

for (suffix in c("_cc.rds", "_mice.rds")){

# HADS
cox_results_hads_anxiety <- readr::read_rds(paste0(filepath, "cox_results_hads_anxiety_cts", suffix))

cox_results_hads_depression <- readr::read_rds(paste0(filepath, "cox_results_hads_depression_cts", suffix))

# Exercise
cox_results_exercise <- readr::read_rds(paste0(filepath, "cox_results_exercise", suffix))

# Life Events
cox_results_lifeevents <- readr::read_rds(paste0(filepath, "cox_results_lifeevents", suffix))

# PHQ
cox_results_phq <- readr::read_rds(paste0(filepath, "cox_results_phq_cts", suffix))

# PSQI
cox_results_psqi <- readr::read_rds(paste0(filepath, "cox_results_psqi", suffix))

# Fatigue
cox_results_fatigue <- readr::read_rds(paste0(filepath, "cox_results_fatigue", suffix))


# Differentiate between anxiety and depression
cox_results_hads_anxiety %<>%
  dplyr::mutate(variable = "hads_score_anxiety")

cox_results_hads_depression %<>%
  dplyr::mutate(variable = "hads_score_depression")


# Combine
cox_results <- cox_results_hads_anxiety %>%
  dplyr::bind_rows(cox_results_hads_depression) %>%
  dplyr::bind_rows(cox_results_phq) %>%
  dplyr::bind_rows(cox_results_fatigue) %>%
  dplyr::bind_rows(cox_results_psqi) %>%
  dplyr::bind_rows(cox_results_exercise) %>%
  dplyr::bind_rows(cox_results_lifeevents)


# Ordering for plotting?
# Need to do manually
cox_results %<>%
  dplyr::mutate(
    ordering = dplyr::case_when(
      term == 'hads_score' ~ 0,
      term == 'TotalPHQ' ~ 0,
      term == 'MinimumExerciseYes' ~ 0,
      term == 'MinimumExerciseNo' ~ 1,
      term == 'AnyLifeEventsNo' ~ 0,
      term == 'AnyLifeEventsYes' ~ 1,
      term == 'SleepDisturbanceNo' ~ 0,
      term == 'SleepDisturbanceYes' ~ 1,
      term == 'OftenLackEnergyNo' ~ 0,
      term == 'OftenLackEnergyYes' ~ 1
    )
  )

# Tidy up the term
cox_results %<>%
  dplyr::mutate(
    term_tidy = dplyr::case_when(
      term == 'hads_score' & variable == 'hads_score_anxiety' ~ 'HADS anxiety unit increase',
      term == 'hads_score' & variable == 'hads_score_depression' ~ 'HADS depression unit increase',
      term == 'TotalPHQ' ~ 'PHQ-15 unit increase',
      term == 'MinimumExerciseYes' ~ 'Exercise target: met',
      term == 'MinimumExerciseNo' ~ 'Exercise target: not met',
      term == 'AnyLifeEventsNo' ~ 'Recent life events: none',
      term == 'AnyLifeEventsYes' ~ 'Recent life events: 1+',
      term == 'SleepDisturbanceNo' ~ 'Sleep disturbance: no',
      term == 'SleepDisturbanceYes' ~ 'Sleep disturbance: yes',
      term == 'OftenLackEnergyNo' ~ 'Often fatigued: no',
      term == 'OftenLackEnergyYes' ~ 'Often fatigued: yes'
    )
  )

# Confidence intervals
cox_results %<>%
  dplyr::mutate(
    conf.interval.tidy = dplyr::case_when(
      (is.na(conf.low) & is.na(conf.high)) ~ "-",
      TRUE ~ paste0(sprintf("%#.3g", estimate), " (", sprintf("%#.3g", conf.low), "–", sprintf("%#.3g", conf.high), ")")
    )
  )

# Tidy p values
# Following BMJ guidance
cox_results %<>%
  dplyr::mutate(
    p.value.tidy = dplyr::case_when(
      is.na(p.value) ~ "-",
      p.value > 0.01 ~ sprintf("%#.2f", round(p.value, 2)),
      p.value >= 0.001 ~ sprintf("%#.3f", round(p.value, 3)),
      p.value < 0.001 ~ '<0.001'
    )
  )


# Significance
cox_results %<>%
  dplyr::mutate(
    significance = dplyr::case_when(
      is.na(p.value) ~ "Reference level",
      p.value <= 0.05 ~ "Significant",
      p.value > 0.05 ~ "Not Significant"
    )
  )


# Multiple testing
# Adjust p values using Benjamini-Hochberg, separately for UC/IBDU and CD
cox_results %<>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::mutate(
    q.value = stats::p.adjust(p.value, method = "BH")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::relocate(q.value, .after = p.value)

cox_results %<>%
  dplyr::mutate(
    q.value.tidy = dplyr::case_when(
      is.na(q.value) ~ "-",
      q.value > 0.01 ~ sprintf("%#.2f", round(q.value, 2)),
      q.value >= 0.001 ~ sprintf("%#.3f", round(q.value, 3)),
      q.value < 0.001 ~ '<0.001'
    )
  )

# Save
readr::write_rds(
  x = cox_results,
  file = paste0(filepath, "cox_results_all_variables_cts", suffix)
)

}
