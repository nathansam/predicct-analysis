library(tidyverse)
library(magrittr)


# Manuscript counts

# Study population size
# Hard to give a single figure because we sometimes exclude depending on the
# exposure we are using. Unless I run all psychosocial variables and find the
# number of unique participant numbers included in the study.

# Run all data cleaning

participants <- hads %>%
  dplyr::bind_rows(exercise) %>%
  dplyr::bind_rows(lifeevents) %>%
  dplyr::bind_rows(phq) %>%
  dplyr::bind_rows(psqi) %>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::distinct(ParticipantNo) %>%
  dplyr::ungroup()


participants %>%
  dplyr::group_by(diagnosis2) %>%
  summarise(n = n())

# 1855 participants sufficiently answered at least one psychosocial variable.  
# CD 935, UC/IBDU 920


# Save participants
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

readr::write_rds(
  x = participants,
  file = paste0(filepath, "participants.rds")
)
