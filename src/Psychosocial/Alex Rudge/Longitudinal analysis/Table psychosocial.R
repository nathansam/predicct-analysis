library(tidyverse)
library(magrittr)
library(survival)
library(gtsummary)


setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/")

# Demographics for patients who answered baseline, 12 and 24 month questionnaires

filepath <- '/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Longitudinal analysis/'

# Load follow up data
data <- readr::read_rds(glue::glue("{filepath}followup.rds"))


# Table of psychosocial variables and 12 and 24 months

data_table <- data

# Tidy columns
data_table %<>%
  dplyr::mutate(
  somatisation = factor(somatisation, levels = c('None', 'Mild', 'ModSev')),
  somatisation = forcats::fct_recode(somatisation, 'Moderate/Severe' = 'ModSev')
  ) 

data_table %<>%
  dplyr::mutate(
    month = factor(month, levels = c(0,12,24)),
    month = forcats::fct_recode(
      month, 
      'Baseline' = '0',
      '12 months' = '12',
      '24 months' = '24'
      )
  )



variables = c(
  'anxiety_group',
  'depression_group',
  'somatisation',
  'SleepDisturbance',
  'MinimumExercise'
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
          anxiety_group ~ 'Anxiety HADS',
          depression_group ~ 'Depression HADS',
          somatisation ~ 'Somatisation',
          MinimumExercise ~ 'Meets recommended exercise',
          SleepDisturbance ~ 'Experiences sleep disturbance'
        )
      ) %>% 
    modify_header(
      label ~ "**Variable**",
      all_stat_cols() ~ "**{level}**") %>%
      gtsummary::add_p(
        test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE, B = 1e5)
      ) %>%
      gtsummary::add_q(method = 'fdr') %>%
      gtsummary::bold_p(q = TRUE),
    .header = "**{strata}**, N = {n/3}"
  ) %>% gtsummary::as_gt() %>%
  gt::tab_spanner(
    label = gt::md("**Questionnaire**"),
    columns = c(stat_1_1, stat_2_1, stat_3_1, stat_1_2, stat_2_2, stat_3_2),
    level = 2,
    gather = FALSE
  ) %>%
  {tbl <- .
  
  # Swap spanner hierarchy
  tbl$`_spanners` <- tbl$`_spanners` %>%
    dplyr::mutate(spanner_level =
                    dplyr::case_match(
                      spanner_level,
                      1 ~ 2,
                      2 ~ 1))
  
  tbl
  }

tbl


# Save as word
filepath_save <- "/Users/arudge/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Predicct/Tables/Longitudinal analysis/"

tbl %>%
  gt::gtsave(
    filename = glue::glue("{filepath_save}Table psychosocial.docx")
  )
