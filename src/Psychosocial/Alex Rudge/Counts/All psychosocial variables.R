library(tidyverse)
library(magrittr)
library(glue)


# Create a file containing all psychosocial variables (at baseline)

# Load all psychosocial

filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

hads <- readr::read_rds(
  glue::glue("{filepath}hads.rds")
)

phq <- readr::read_rds(
  glue::glue("{filepath}PHQ.rds")
)

fatigue <- readr::read_rds(
  glue::glue("{filepath}Fatigue.rds")
)

exercise <- readr::read_rds(
  glue::glue("{filepath}Exercise.rds")
)

lifeevents <- readr::read_rds(
  glue::glue("{filepath}Life events.rds")
)

psqi <- readr::read_rds(
  glue::glue("{filepath}PSQI.rds")
)

# All participants completing at least one psychosocial questionnaire
participants <- dplyr::bind_rows(
  hads %>% dplyr::select(diagnosis2, ParticipantNo),
  phq %>% dplyr::select(diagnosis2, ParticipantNo),
  exercise %>% dplyr::select(diagnosis2, ParticipantNo),
  lifeevents %>% dplyr::select(diagnosis2, ParticipantNo),
  psqi %>% dplyr::select(diagnosis2, ParticipantNo),
) %>%
  dplyr::distinct(diagnosis2, ParticipantNo)


# Other common variables
common_variables <- readr::read_rds("/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/common_variables.rds")

# Only select participantno and psychosocial variable for each file

hads_anxiety <- hads %>%
  dplyr::filter(hads_type == "anxiety_hads") %>%
  dplyr::rename(anxiety_hads = hads_score) %>%
  dplyr::select(ParticipantNo, anxiety_hads)

hads_depression <- hads %>%
  dplyr::filter(hads_type == "depression_hads") %>%
  dplyr::rename(depression_hads = hads_score) %>%
  dplyr::select(ParticipantNo, depression_hads)

phq %<>%
  dplyr::select(ParticipantNo, TotalPHQ) %>%
  dplyr::rename(somatisation_score = TotalPHQ)

fatigue %<>%
  dplyr::select(ParticipantNo, OftenLackEnergy)

exercise %<>%
  dplyr::select(ParticipantNo, MinimumExercise)

lifeevents %<>%
  dplyr::select(ParticipantNo, AnyLifeEvents)

psqi %<>%
  dplyr::select(ParticipantNo, SleepDisturbance)

# Join to participants

data <- participants %<>%
  dplyr::select(ParticipantNo) %>%
  dplyr::left_join(hads_anxiety) %>%
  dplyr::left_join(hads_depression) %>%
  dplyr::left_join(phq) %>%
  dplyr::left_join(fatigue) %>%
  dplyr::left_join(exercise) %>%
  dplyr::left_join(lifeevents) %>%
  dplyr::left_join(psqi)

# Add common participant-level variables
data %<>%
  dplyr::left_join(common_variables, by = 'ParticipantNo')


# Save
readr::write_rds(
  x = data,
  file = glue::glue("{filepath}all_psychosocial_variables.rds")
)
