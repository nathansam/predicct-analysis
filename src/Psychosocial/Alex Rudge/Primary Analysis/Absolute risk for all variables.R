library(tidyverse)
library(magrittr)


# Load in all the Cox results

filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Partially adjusted/"

# HADS
absolute_risk_anxiety <- readr::read_rds(paste0(filepath, "absolute_risk_anxiety.rds"))

absolute_risk_depression <- readr::read_rds(paste0(filepath, "absolute_risk_depression.rds"))

# Exercise
absolute_risk_exercise <- readr::read_rds(paste0(filepath, "absolute_risk_exercise.rds"))

# Life Events
absolute_risk_lifeevents <- readr::read_rds(paste0(filepath, "absolute_risk_lifeevents.rds"))

# PHQ
absolute_risk_phq <- readr::read_rds(paste0(filepath, "absolute_risk_phq.rds"))

# PSQI
absolute_risk_psqi <- readr::read_rds(paste0(filepath, "absolute_risk_psqi.rds"))


# Combine
data_absolute_risk <- absolute_risk_anxiety %>%
  dplyr::bind_rows(absolute_risk_depression) %>%
  dplyr::bind_rows(absolute_risk_exercise) %>%
  dplyr::bind_rows(absolute_risk_lifeevents) %>%
  dplyr::bind_rows(absolute_risk_phq) %>%
  dplyr::bind_rows(absolute_risk_psqi)

# Term
data_absolute_risk %<>%
  dplyr::mutate(
    term = paste0(variable, level)
  )

# Ordering
data_absolute_risk %<>%
  dplyr::mutate(
    ordering = dplyr::case_when(
      term == 'anxiety0-7' ~ 0,
      term == 'anxiety8-21' ~ 1,
      term == 'depression0-7' ~ 0,
      term == 'depression8-21' ~ 1,
      term == 'MinimumExerciseYes' ~ 0,
      term == 'MinimumExerciseNo' ~ 1,
      term == 'AnyLifeEventsNo' ~ 0,
      term == 'AnyLifeEventsYes' ~ 1,
      term == 'somatisationNone' ~ 0,
      term == 'somatisationMild' ~ 1,
      term == 'somatisationModSev' ~ 2,
      term == 'SleepDisturbanceNo' ~ 0,
      term == 'SleepDisturbanceYes' ~ 1,
    )
  )


# Tidy up the term
data_absolute_risk %<>%
  dplyr::mutate(
    term_tidy = dplyr::case_when(
      term == 'anxiety0-7' ~ 'HADS Anxiety Score 0-7',
      term == 'anxiety8-21' ~ 'HADS Anxiety Score 8-21',
      term == 'depression0-7' ~ 'HADS Depression Score 0-7',
      term == 'depression8-21' ~ 'HADS Depression Score 8-21',
      term == 'MinimumExerciseYes' ~ 'Meets recommended exercise',
      term == 'MinimumExerciseNo' ~ 'Does not meet recommended exercise',
      term == 'AnyLifeEventsNo' ~ 'No life events in past month',
      term == 'AnyLifeEventsYes' ~ 'At least 1 life event in past month',
      term == 'somatisationNone' ~ 'Somatisation 0-4 (none)',
      term == 'somatisationMild' ~ 'Somatisation 5-9 (mild)',
      term == 'somatisationModSev' ~ 'Somatisation 10-30 (moderate/severe)',
      term == 'SleepDisturbanceNo' ~ 'No sleep disturbance (PSQI<=5)',
      term == 'SleepDisturbanceYes' ~ 'Sleep disturbance (PSQI>5)',
    )
  )

# Confidence intervals
# As percentages as that is how they will be reported
data_absolute_risk %<>%
  dplyr::mutate(conf.interval.tidy = paste0(
    sprintf("%#.3g", estimate*100), 
    "%",
    " (",
    sprintf("%#.3g", conf.low*100),
    "%",
    " to ",
    sprintf("%#.3g", conf.high*100),
    "%",
    ")"
  )
)
