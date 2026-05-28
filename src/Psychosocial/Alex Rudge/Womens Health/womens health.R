library(tidyverse)
library(magrittr)
library(glue)
library(survival)
library(broom)

# Functions
setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Womens Health")

source("womens health functions.R")

# Load in data
filepath_data <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct4/cox-objects"


# Tidy cox models into results dataframes

# Meno
results_meno <- read_and_tidy_cox(
  subdir = "01 cox-menopause",
  file_prefix = "cox-meno",
  variable = 'meno2'
)

results_aub <- read_and_tidy_cox(
  subdir = "02 cox-aub",
  file_prefix = "cox-aub",
  variable = 'GlobalAUB'
)

# Dysmenorrhoea
results_dysmenorrhoea <- read_and_tidy_cox(
  subdir = "03 cox-dysmenorrhoea",
  file_prefix = "cox-dysm",
  variable = 'PeriodTypePain'
)

# PMS
results_pms <- read_and_tidy_cox(
  subdir = "04 cox-pms",
  file_prefix = "cox-pms",
  variable = 'SufferPMS'
)

# IBD pain
results_ibd_global <- read_and_tidy_cox(
  subdir = "05 cox-ibd-global",
  file_prefix = "cox-ibd-global",
  variable = 'IBDsymptoms_menses'
)


# Contraception
results_contraception <- read_and_tidy_cox(
  subdir = "06 cox-contraception",
  file_prefix = "cox-contra",
  variable = 'hormonal2'
)


# Combine into a single df
results <- dplyr::bind_rows(
  results_meno,
  results_aub,
  results_dysmenorrhoea,
  results_pms,
  results_ibd_global,
  results_contraception
)

# Ordering for plots - they are actually already in the correct order

results %<>%
  dplyr::group_by(variable, ibd_type, flare_type) %>%
  dplyr::mutate(
    ordering = (dplyr::row_number() - 1)
  ) %>%
  dplyr::ungroup()


# Tidy up the terms
results %<>%
  dplyr::mutate(
    term.tidy = level
)

# Confidence intervals
results %<>%
  dplyr::mutate(
    conf.interval.tidy = dplyr::case_when(
      (is.na(conf.low) & is.na(conf.high)) ~ "-",
      TRUE ~ paste0(sprintf("%#.3g", estimate), " (", sprintf("%#.3g", conf.low), "–", sprintf("%#.3g", conf.high), ")")
    )
  )

# P-values
results %<>%
  dplyr::mutate(
    p.value.tidy = dplyr::case_when(
      is.na(p.value) ~ "-",
      p.value > 0.01 ~ sprintf("%#.2f", round(p.value, 2)),
      p.value >= 0.001 ~ sprintf("%#.3f", round(p.value, 3)),
      p.value < 0.001 ~ '<0.001'
    )
  )

# Significance
results %<>%
  dplyr::mutate(
    significance = dplyr::case_when(
      is.na(p.value) ~ "Reference level",
      p.value <= 0.05 ~ "Significant",
      p.value > 0.05 ~ "Not Significant"
    )
  )

# Save

readr::write_rds(
  x = results,
  file = glue("{filepath_data}/cox_all_results.rds")
)


