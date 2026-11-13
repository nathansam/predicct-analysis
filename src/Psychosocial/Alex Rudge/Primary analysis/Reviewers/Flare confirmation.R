library(tidyverse)
library(magrittr)



# Objective flares confirmed with biochem and therapy change.

flare_confirmation <- readr::read_rds("/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Review/flare-confirmation.rds")

flares_hard <- readr::read_rds("/Volumes/igmm/cvallejo-predicct/people/chiara/flares_hard.RDS")


# Only select relevant columns
flare_confirmation %<>%
  dplyr::select(ParticipantNo, ParticipantId, hardflare, hardflare_time, objectConfirmed)

# How many were confirmed
flare_confirmation %>%
  dplyr::filter(hardflare == 1) %>%
  dplyr::count(objectConfirmed)
# This is all flares, even if they occurred after 2 years


# Looking at only those within 2 years

flares_hard %<>%
  dplyr::left_join(
    flare_confirmation %>%
      dplyr::select(ParticipantNo, objectConfirmed)
    ) %>%
  # If no flare make NA
  dplyr::mutate(
    objectConfirmed = dplyr::if_else(hardflare == 1, objectConfirmed, NA)
  ) 

flares_hard %>%
  dplyr::count(objectConfirmed)


# Replace hardflare only if confirmed with biomarkers and therapy change
flares_hard %<>%
  dplyr::mutate(
    hardflare = dplyr::if_else(objectConfirmed == "Both", 1, 0),
    hardflare = tidyr::replace_na(hardflare, 0)
  )

flares_hard %>%
  dplyr::count(hardflare)

