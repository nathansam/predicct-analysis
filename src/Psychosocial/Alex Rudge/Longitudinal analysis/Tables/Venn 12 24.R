library(tidyverse)
library(magrittr)
library(ggvenn)

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/")

# Demographics for patients who answered baseline, 12 and 24 month questionnaires

filepath <- '/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Longitudinal analysis/'

# Load follow up data
data <- readr::read_rds(glue::glue("{filepath}followup.rds"))

# Venn diagram of who responded at 12 and 24 months

# Who had responses
data %<>%
  dplyr::filter(
    dplyr::if_any(
      .cols = c(anxiety, depression, somatisation, MinimumExercise, SleepDisturbance),
      .fns = function(x) !is.na(x)
      )
    )

# Only interested in 12 and 24 months

purrr::map(
  .x = c(12, 24),
  .f = function(x){
    data %>%
      dplyr::filter(month == x) %>%
      dplyr::pull(ParticipantNo)
  }
) %>%
  purrr::set_names(stringr::str_c("Month ", c(12, 24), " \n responders")) %>%
  ggvenn(fill_color = c('blue', 'orange'))
