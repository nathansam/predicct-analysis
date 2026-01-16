library(gt)
library(gtsummary)

# Control-8 components

variables <- c(
  'TreatmentUseful',
  'MissPlannedActivities',
  'WakeUpAtNight',
  'SignificantPain',
  'OftenLackEnergy',
  'AnxiousDepressed',
  'NeedChangeTreatment'
)

# Calculating the score
data_control <- IBD %>%
  dplyr::select(
    ParticipantId,
    ParticipantNo,
    TreatmentUseful,
    MissPlannedActivities,
    WakeUpAtNight,
    SignificantPain,
    OftenLackEnergy,
    AnxiousDepressed,
    NeedChangeTreatment, 
    OverallControl
  ) 

# Answers are coded as Yes = 1, No = 2, Not sure = 3.
# Current treatment has option 4, not on any treatment.

data_control <- IBD %>%
  dplyr::select(
    ParticipantNo,
    tidyselect::all_of(variables)
  ) 

# Answers are coded as Yes = 1, No = 2, Not sure = 3.
# Current treatment useful has option 4, not on any treatment.

# Recode
data_control %<>%
  dplyr::mutate(
    TreatmentUseful = dplyr::case_match(
      TreatmentUseful,
      1 ~ 'Yes',
      2 ~ 'No',
      3 ~ 'Not sure',
      4 ~ 'Not on any treatment'
    )
  ) %>%
  dplyr::mutate(TreatmentUseful = factor(TreatmentUseful)) %>%
  dplyr::mutate(
    TreatmentUseful = forcats::fct_relevel(
      TreatmentUseful, 
      'Yes', 
      'No', 
      'Not sure', 
      'Not on any treatment')
  )

data_control %<>%
  dplyr::mutate(dplyr::across(
    .cols = c('MissPlannedActivities',
              'WakeUpAtNight',
              'SignificantPain',
              'OftenLackEnergy',
              'AnxiousDepressed',
              'NeedChangeTreatment'),
    .fns = function(x) dplyr::case_match(
      x,
      1 ~ 'Yes',
      2 ~ 'No',
      3 ~ 'Not sure'
    ) %>%
      forcats::as_factor() %>%
      forcats::fct_relevel('Yes', 'No', 'Not sure')
  )
  )

# Add control score components and final score
data_baseline_anxiety_table <- data_baseline_anxiety %>%
  dplyr::select(ParticipantNo, diagnosis2, score_group) %>%
  dplyr::left_join(data_control, by = 'ParticipantNo') %>%
  dplyr::left_join(IBD_C %>% dplyr::select(ParticipantNo, control_8),
                   by = 'ParticipantNo') %>%
  dplyr::mutate(
    diagnosis2 = dplyr::case_match(
      diagnosis2,
      "CD" ~ "Crohn's Disease",
      "UC/IBDU" ~ 'Ulcerative Colitis'
    )
  )

# Anxiety
data_baseline_anxiety_table %>%
  gtsummary::tbl_strata(
    strata = diagnosis2,
    .tbl_fun = ~
      .x %>%
      gtsummary::tbl_summary(
        by = score_group,
        include = c(variables, 'control_8'),
        missing_text = 'Missing data',
        label = list(
        )
      ) %>%
      gtsummary::add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE, B = 1e5)) %>%
      gtsummary::add_q(method = 'fdr') %>%
      gtsummary::bold_p(q = TRUE),
    .header = "**{strata}**, N = {n}"
  ) %>%
  gtsummary::as_gt() %>%
  gt::tab_spanner(
    label = gt::md("**HADS Anxiety Score**"),
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

# Depression
# Tidy up the data ready for the table
data_baseline_depression_table <- data_baseline_depression %>%
  dplyr::select(ParticipantNo, diagnosis2, score_group) %>%
  dplyr::left_join(data_control, by = 'ParticipantNo') %>%
  dplyr::left_join(IBD_C %>% dplyr::select(ParticipantNo, control_8),
                   by = 'ParticipantNo') %>%
  dplyr::mutate(
    diagnosis2 = dplyr::case_match(
      diagnosis2,
      "CD" ~ "Crohn's Disease",
      "UC/IBDU" ~ 'Ulcerative Colitis'
    )
  )

data_baseline_depression_table %>%
  gtsummary::tbl_strata(
    strata = diagnosis2,
    .tbl_fun = ~
      .x %>%
      gtsummary::tbl_summary(
        by = score_group,
        include = c(variables, 'control_8'),
        missing_text = 'Missing data',
        label = list(
        )
      ) %>%
      gtsummary::add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE, B = 1e5)) %>%
      gtsummary::add_q(method = 'fdr') %>%
      gtsummary::bold_p(q = TRUE),
    .header = "**{strata}**, N = {n}"
  ) %>%
  gtsummary::as_gt() %>%
  gt::tab_spanner(
    label = gt::md("**HADS Depression Score**"),
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
