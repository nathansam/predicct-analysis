library(tidyverse)
library(magrittr)
library(survival)
library(gtsummary)


setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/HADS/")

source("data_cleaning.R")


# Demographics for patients who answered baseline, 12 and 24 month questionnaires

# Calculate score group
hads_long <- hads %>%
  dplyr::rename(
    anxiety_0 = anxiety_hads,
    anxiety_12 = anxiety_hads_12,
    anxiety_24 = anxiety_hads_24,
    depression_0 = depression_hads,
    depression_12 = depression_hads_12,
    depression_24 = depression_hads_24,
    ) %>%
  tidyr::pivot_longer(
    cols = c(anxiety_0, anxiety_12, anxiety_24, depression_0, depression_12, depression_24),
    names_to = c(".value", "month"),
    names_sep = "_"
  )

# Calculate score groups
hads_long %<>%
  dplyr::mutate(
    anxiety_group = cut(
      anxiety, 
      breaks = c(0, 7, 21), 
      labels = c("0-7", "8-21"), 
      include.lowest = TRUE),
    depression_group = cut(
      depression, 
      breaks = c(0, 7, 21), 
      labels = c("0-7", "8-21"), 
      include.lowest = TRUE)
  )

# Select columns for table
data_table <- hads_long %>%
  dplyr::select(
    ParticipantNo, diagnosis2, Sex, age, FC, Smoke, IMD, month, anxiety, depression, anxiety_group, depression_group
  )

# Charlie wanted anxiety and depression at baseline to answer e.g., if you're depressed 
# at baseline are you less likely to complete follow up

data_table %<>%
  dplyr::left_join(
    data_table %>%
      dplyr::filter(month == '0') %>%
      dplyr::select(ParticipantNo, anxiety, anxiety_group, depression, depression_group) %>%
      dplyr::rename(
        baseline_anxiety = anxiety,
        baseline_anxiety_group = anxiety_group,
        baseline_depression = depression,
        baseline_depression_group = depression_group,
    )
)

# Remove missing anxiety scores
data_table %<>%
  dplyr::filter(
    !is.na(anxiety)
  )

# Tidy up columns
data_table %<>%
  dplyr::mutate(IMD = as.character(IMD)) %>%
  dplyr::mutate(
    IMD = dplyr::case_match(
      IMD,
      '1' ~ '1 (most deprived)',
      '2' ~ '2',
      '3' ~ '3',
      '4' ~ '4',
      '5' ~ '5 (least deprived)'
    )
  )

# Month as factor
data_table %<>%
  dplyr::mutate(
    month = dplyr::case_match(
      month,
      '0' ~ 'Baseline',
      '12' ~ '12 months',
      '24' ~ '24 months'
    )
  ) %>%
  dplyr::mutate(
    month = forcats::as_factor(month)
  )

# Exponentiate FC
data_table %<>%
  dplyr::mutate(
    FC = exp(FC)
  )


# Table
variables <- c(
  'age',
  'Sex', 
  'Smoke', 
  'IMD',
  'FC',
  'baseline_anxiety_group',
  'baseline_depression_group',
  'anxiety_group',
  'depression_group'
)

tbl <- data_table %>%
  gtsummary::tbl_strata(
    strata = diagnosis2,
    .tbl_fun = ~
      .x %>% 
      gtsummary::tbl_summary(
        by = month,
        include = variables,
        missing_text = 'Missing data',
        label = list(
          age ~ "Age (years)",
          Sex ~ 'Sex',
          Smoke ~ 'Smoking status',
          IMD ~ 'Index of multiple deprivation',
          FC ~ 'Fecal calprotectin (ug/g)',
          baseline_anxiety_group ~ 'Anxiety HADS at baseline',
          baseline_depression_group ~ 'Depression HADS at baseline',
          anxiety_group ~ 'Anxiety HADS',
          depression_group ~ 'Depression HADS'
      )
    ) %>%
      gtsummary::add_p(
        test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE, B = 1e5)
      ) %>%
      gtsummary::add_q(method = 'fdr') %>%
      gtsummary::bold_p(q = TRUE)
)
  
tbl

# Save as word
filepath <- "/Users/arudge/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Predicct/Tables/Longitudinal analysis/"

tbl %>%
  gtsummary::as_gt() %>%
  gt::gtsave(
    filename = paste0(filepath, "Table1 HADS.docx")
  )


