library(tidyverse)
library(magrittr)
library(survival)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal data analysis/HADS/")

#source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")
source("landmarking data.R")

# Counts

# Number of patients included
data_anxiety_soft %>% 
  nrow()

# By IBD type
data_anxiety_soft %>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::count()

# Number of events
# Soft
data_anxiety_soft %>%
  dplyr::count(DiseaseFlareYN)

# Hard
data_anxiety_hard %>%
  dplyr::count(DiseaseFlareYN)

# Quite a small number of events. 
# Compared with 469 soft flares in first 12 months, and 138 hard flares. 
# Does make sense that it decreases as the amount in the risk set decreases.

