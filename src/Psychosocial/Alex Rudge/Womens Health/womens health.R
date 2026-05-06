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

# Frequency
results_freq <- read_and_tidy_cox(
  subdir = "02 cox-cycle-frequency",
  file_prefix = "cox-freq",
  variable = 'Frequency'
)

# Prolonged
results_prolonged <- read_and_tidy_cox(
  subdir = "03 cox-prolonged-periods",
  file_prefix = "cox-long",
  variable = 'PeriodLong'
)

# Irregular
results_irregular <- read_and_tidy_cox(
  subdir = "04 cox-irregular-cycles",
  file_prefix = "cox-regul",
  variable = 'PeriodIrregular'
)

# Heavy
results_heavy <- read_and_tidy_cox(
  subdir = "05 cox-heavy-periods",
  file_prefix = "cox-vol",
  variable = 'DescribeBloodLoss_heavy'
)

# Dysmenorrhoea
results_dysmenorrhoea <- read_and_tidy_cox(
  subdir = "06 cox-dysmenorrhoea",
  file_prefix = "cox-dysm",
  variable = 'PeriodTypePain'
)

# PMS
results_pms <- read_and_tidy_cox(
  subdir = "07 cox-pms",
  file_prefix = "cox-pms",
  variable = 'SufferPMS'
)

# IBD pain
results_ibd_pain <- read_and_tidy_cox(
  subdir = "08 cox-ibd-pain",
  file_prefix = "cox-ibd-pain",
  variable = 'AbdominalPain_binary'
)

# Bowel movements
results_bowel_movements <- read_and_tidy_cox(
  subdir = "09 cox-more-bowel-movements",
  file_prefix = "cox-ibd-freq",
  variable = 'MoreBowelMovements_binary'
)

# Looser bowel
results_looser_bowel <- read_and_tidy_cox(
  subdir = "10 cox-looser-bowel-movements",
  file_prefix = "cox-ibd-loose",
  variable = 'LooserBowelMovements_binary'
)

# PR Bleeding
results_pr_bleeding <- read_and_tidy_cox(
  subdir = "11 cox-pr-bleeding",
  file_prefix = "cox-prb",
  variable = 'PassMoreBlood_binary'
)

# Nausea
results_nausea <- read_and_tidy_cox(
  subdir = "12 cox-nausea",
  file_prefix = "cox-naus",
  variable = 'ProblemsNausea_binary'
)

# Contraception
results_contraception <- read_and_tidy_cox(
  subdir = "13 cox-contraception",
  file_prefix = "cox-contra",
  variable = 'hormonal2'
)


# Combine into a single df
results <- dplyr::bind_rows(
  results_meno,
  results_freq,
  results_prolonged,
  results_irregular,
  results_heavy,
  results_dysmenorrhoea,
  results_pms,
  results_ibd_pain,
  results_bowel_movements,
  results_looser_bowel,
  results_pr_bleeding,
  results_nausea,
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
    term.tidy = dplyr::case_match(
      term,
      'meno2Pre-menopausal' ~ 'Pre-menopausal',
      'meno2Post-menopausal' ~ 'Post-menopausal',
      'FrequencyInfrequent' ~ 'Cycles infrequent',
      'FrequencyNormal' ~ 'Cycles regular',
      'FrequencyFrequent' ~ 'Cycles frequent',
      'PeriodLongNormal' ~ 'Period duration normal',
      'PeriodLongLong' ~ 'Period duration long',
      'PeriodIrregularNo' ~ 'Periods regular',
      'PeriodIrregularYes' ~ 'Periods irregular',
      'DescribeBloodLoss_heavyLight' ~ 'Periods light',
      'DescribeBloodLoss_heavyModerate' ~ 'Periods moderate',
      'DescribeBloodLoss_heavyHeavy' ~ 'Periods heavy',
      'PeriodTypePainNone' ~ 'Period pain, none',
      'PeriodTypePainMild to moderate pain' ~ 'Period pain, mild/moderate',
      'PeriodTypePainSevere pain' ~ 'Period pain, severe',
      'SufferPMSNo' ~ 'No PMS',
      'SufferPMSYes' ~ 'PMS',
      'AbdominalPain_binaryNot agree' ~ 'No increased IBD abdominal pain during periods',
      'AbdominalPain_binaryAgree' ~ 'Increased IBD abdominal pain during periods',
      'AbdominalPain_binaryUndifferentiated' ~ 'IBD and period abdominal pain indistinguishable',
      'MoreBowelMovements_binaryNot agree' ~ 'Not more bowel movements during period',
      'MoreBowelMovements_binaryAgree' ~ 'More bowel movements during period',
      'LooserBowelMovements_binaryNot agree' ~ 'Not looser bowel movements during period',
      'LooserBowelMovements_binaryAgree' ~ 'Looser bowel movements during period',
      'PassMoreBlood_binaryNot agree' ~ 'Not more stool blood during period',
      'PassMoreBlood_binaryAgree' ~ 'More stool blood during period',
      'ProblemsNausea_binaryAgree' ~ 'Problems with nausea and vomiting ',
      'ProblemsNausea_binaryNot agree' ~ 'No problems with nausea and vomiting',
      'hormonal2No' ~ 'Not using hormonal contraception',
      'hormonal2Yes' ~ 'Using hormonal contraception'
    )
  )

# Confidence intervals
results %<>%
  dplyr::mutate(
    conf.interval.tidy = dplyr::case_when(
      (is.na(conf.low) & is.na(conf.high)) ~ "-",
      TRUE ~ paste0(sprintf("%#.3g", estimate), " (", sprintf("%#.3g", conf.low), " to ", sprintf("%#.3g", conf.high), ")")
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


