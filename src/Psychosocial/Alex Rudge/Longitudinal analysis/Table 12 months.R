library(tidyverse)
library(magrittr)
library(survival)
library(gtsummary)


setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/")

# Demographics for patients who answered baseline, 12 and 24 month questionnaires

filepath <- '/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Longitudinal analysis/'

# Load follow up data
data <- readr::read_rds(glue::glue("{filepath}followup.rds"))


# Charlie wanted anxiety and depression at baseline to answer e.g., if you're depressed 
# at baseline are you less likely to complete follow up

# HADS
hads_baseline <- data %>%
    dplyr::filter(month == 0) %>%
    dplyr::select(ParticipantNo, anxiety, anxiety_group, depression, depression_group) %>%
    dplyr::rename(
      baseline_anxiety = anxiety,
      baseline_anxiety_group = anxiety_group,
      baseline_depression = depression,
      baseline_depression_group = depression_group,
  ) %>%
  # factors
  dplyr::mutate(
    baseline_anxiety_group = factor(baseline_anxiety_group, levels = c('0-7', '8-21')),
    baseline_depression_group = factor(baseline_depression_group, levels = c('0-7', '8-21'))
  )

# PHQ
phq_baseline <- data %>%
  dplyr::filter(month == 0) %>%
  dplyr::select(ParticipantNo, somatisation) %>%
  dplyr::rename(
    baseline_somatisation = somatisation
    ) %>%
  dplyr::mutate(
    baseline_somatisation = factor(baseline_somatisation, levels = c('None', 'Mild', 'Mod/Sev'))
  )

# Exercise
exercise_baseline <- data %>%
  dplyr::filter(month == 0) %>%
  dplyr::select(ParticipantNo, MinimumExercise) %>%
  dplyr::rename(
    baseline_MinimumExercise = MinimumExercise
  ) %>%
  dplyr::mutate(
    baseline_MinimumExercise = factor(baseline_MinimumExercise, levels = c('Yes', 'No'))
  )

# Sleep
psqi_baseline <- data %>%
  dplyr::filter(month == 0) %>%
  dplyr::select(ParticipantNo, SleepDisturbance) %>%
  dplyr::rename(
    baseline_SleepDisturbance = SleepDisturbance
  ) %>%
  dplyr::mutate(
    baseline_SleepDisturbance = factor(baseline_SleepDisturbance, levels = c('No', 'Yes'))
  )

# Data for the table
data_table <- data %>%
  dplyr::filter(month == 12)

# Who answered at least 1 questionnaire
data_table %<>%
  dplyr::mutate(
    questionnaire_response = dplyr::case_match(
      !dplyr::if_all(anxiety:SleepDisturbance, is.na),
      TRUE ~ "Yes",
      FALSE ~ "No"
    ),
    questionnaire_response = factor(questionnaire_response, levels = c("No", "Yes"))
) 

# Soft flare flag
data_table %<>%
  dplyr::mutate(soft_flare_flag = dplyr::case_when(
    softflare == 1 & softflare_time <= 365 ~ "Yes",
    TRUE ~ "No"
  )) %>%
  dplyr::mutate(soft_flare_flag = factor(soft_flare_flag, levels = c('No', 'Yes')))

# Hard flare flag
data_table %<>%
  dplyr::mutate(hard_flare_flag = dplyr::case_when(
    hardflare == 1 & hardflare_time <= 365 ~ "Yes",
    TRUE ~ "No"
  )) %>%
  dplyr::mutate(hard_flare_flag = factor(hard_flare_flag, levels = c('No', 'Yes')))


# Columns for the table
data_table %<>%
  dplyr::select(
    ParticipantNo, diagnosis2, Age, Sex, Ethnicity, IMD, Smoke, FC, questionnaire_response, soft_flare_flag, hard_flare_flag
  )

# Add baseline psychosocial variables
data_table %<>%
  dplyr::left_join(hads_baseline) %>%
  dplyr::left_join(phq_baseline) %>%
  dplyr::left_join(exercise_baseline) %>%
  dplyr::left_join(psqi_baseline)

# Tidy up columns
data_table %<>%
  dplyr::mutate(
    Sex = factor(Sex, levels = c('Male', 'Female'))
  )

data_table %<>%
  dplyr::mutate(
    Smoke = factor(Smoke, levels = c('Never', 'Previous', 'Current'))
  )

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


# Table
variables <- c(
  'Age',
  'Sex', 
  'IMD',
  'Ethnicity',
  'Smoke',
  'FC',
  'baseline_anxiety_group',
  'baseline_depression_group',
  'baseline_somatisation',
  'baseline_MinimumExercise',
  'baseline_SleepDisturbance',
  'soft_flare_flag',
  'hard_flare_flag'
)


tbl <- data_table %>%
  gtsummary::tbl_strata(
    strata = diagnosis2,
    .tbl_fun = ~
      .x %>% 
      gtsummary::tbl_summary(
        by = questionnaire_response,
        include = variables,
        missing_text = 'Missing data',
        label = list(
          Age ~ "Age (years)",
          Sex ~ 'Sex',
          IMD ~ 'Index of multiple deprivation',
          Ethnicity ~ 'Ethnicity',
          Smoke ~ 'Smoking status',
          FC ~ 'Fecal calprotectin (ug/g)',
          baseline_anxiety_group ~ 'Anxiety HADS at baseline',
          baseline_depression_group ~ 'Depression HADS at baseline',
          baseline_somatisation ~ 'Somatisation at baseline',
          baseline_MinimumExercise ~ 'Recommended exercise at baseline',
          baseline_SleepDisturbance ~ 'Sleep disturbance at baseline',
          soft_flare_flag ~ 'Patient reported flare before 12 months',
          hard_flare_flag ~ 'Objective flare before 12 months'
        )
      ) %>% 
      gtsummary::add_p(
        test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE, B = 1e5)
      ) %>%
      gtsummary::add_q(method = 'fdr') %>%
      gtsummary::bold_p(q = TRUE)
  ) %>% gtsummary::as_gt() %>%
  gt::tab_spanner(
    label = gt::md("**12-month questionnaire**"),
    columns = c(stat_1_1, stat_2_1, stat_1_2, stat_2_2),
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
    filename = glue::glue("{filepath_save}Table 12 months.docx")
  )
