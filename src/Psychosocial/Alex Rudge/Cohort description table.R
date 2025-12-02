library(tidyverse)
library(magrittr)
library(gtsummary)

# Comparing the psychosocial cohort to the entire Predicct cohort


# Load in the Predicct cohort
data_cohort <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/predicct/processed/demo-full.rds"
)

data_cd <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/predicct/processed/demo-cd.rds"
)

data_uc <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/predicct/processed/demo-uc.rds"
)

# Control scores
data_control <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/people/chiara/IBD_C.rds"
)


# Psychosocial cohort
# Load participants

participants <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/participants.rds"
)

# Select relevant columns
data_cohort %<>%
  dplyr::select(
    ParticipantNo,
    SiteNo,
    diagnosis,
    diagnosis2,
    Sex,
    Age,
    Ethnicity,
    BMI,
    IMD,
    `IBD Duration`,
    Treatment,
    Biologic,
    Smoke,
    FC,
    CReactiveProtein
  )

# Rename IBD duration
data_cohort %<>%
  dplyr::rename(
    IBD_duration = `IBD Duration`
  )

# Flag if a patient is in the psychosocial cohort
data_cohort %<>%
  dplyr::mutate(
    psychosocial = dplyr::case_when(
      ParticipantNo %in% participants$ParticipantNo ~ 'Yes',
      .default = 'No'
    )
  )

# How many in psychosocial cohort
data_cohort %>%
  dplyr::count(psychosocial)


# Control score
data_cohort %<>%
  dplyr::left_join(
    data_control %>%
      dplyr::select(ParticipantNo, OverallControl, control_8),
    by = "ParticipantNo"
  )


# Trick to compare entire cohort to a subset
data_table <- dplyr::bind_rows(
  data_cohort %>%
  dplyr::mutate(cohort = 'predicct'),
  data_cohort %>%
    dplyr::filter(psychosocial == 'Yes') %>%
    dplyr::mutate(cohort = 'psychosocial')
)

# Tidy up variables
# IBD
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

# Cohort
data_table %<>%
  dplyr::mutate(
    cohort = dplyr::case_match(
      cohort,
      'predicct' ~ 'Total PREdiCCt Cohort',
      'psychosocial' ~ 'Psychosocial Cohort'
    )
  ) %>%
  dplyr::mutate(
    cohort = factor(cohort),
    cohort = forcats::fct_relevel(cohort, 'Total PREdiCCt Cohort')
  )

# Table
variables <- c(
  'Age',
  'Sex',
  'Ethnicity',
  'BMI',
  'IMD',
  'diagnosis',
  'IBD_duration',
  'control_8',
  'OverallControl',
  'FC',
  'CReactiveProtein',
  'Biologic'
)

data_table %>%
  gtsummary::tbl_summary(
    by = cohort,
    include = variables,
    missing_text = 'Missing data',
    label = list(
      Age ~ "Age",
      Sex ~ 'Sex',
      Ethnicity ~ "Ethnicity",
      IMD ~ 'Index of multiple deprivation',
      diagnosis ~ 'IBD Type',
      IBD_duration ~ 'IBD Duration',
      control_8 ~ 'IBD-Control-8',
      OverallControl ~ 'IBD-Control-VAS',
      FC ~ 'Fecal calprotectin (ug/g)',
      CReactiveProtein ~ 'C-reactive protein (mg/L)',
      Biologic ~ 'Biologic use'
    )
  )




# Crohn's specific variables

data_cd %<>%
  dplyr::select(
    ParticipantNo,
    Location,
    Behaviour,
    Perianal,
    HBI,
    Surgery,
    Smoke,
    Biologic
)
  
# Flag if a patient is in the psychosocial cohort
data_cd %<>%
  dplyr::mutate(
    psychosocial = dplyr::case_when(
      ParticipantNo %in% participants$ParticipantNo ~ 'Yes',
      .default = 'No'
    )
  )

# Trick to compare entire cohort to a subset
data_table_cd <- dplyr::bind_rows(
  data_cd %>%
    dplyr::mutate(cohort = 'predicct'),
  data_cd %>%
    dplyr::filter(psychosocial == 'Yes') %>%
    dplyr::mutate(cohort = 'psychosocial')
)

# Tidy up variables
# Harvey Bradshaw as categorical
data_table_cd %<>%
  dplyr::mutate(
    HBI_cat = dplyr::case_when(
      HBI < 5 ~ '<5',
      HBI %in% c(5, 6, 7) ~ '5-7',
      (HBI >= 8) & (HBI <= 16) ~ '8-16',
      HBI > 16 ~ '>16'
    )) %>%
  dplyr::mutate(HBI_cat = factor(HBI_cat)) %>%
  dplyr::mutate(HBI_cat = forcats::fct_relevel(
    HBI_cat,
    '<5', '5-7', '8-16', '>16'
  ))

# Cohort
data_table_cd %<>%
  dplyr::mutate(
    cohort = dplyr::case_match(
      cohort,
      'predicct' ~ 'Total PREdiCCt Cohort',
      'psychosocial' ~ 'Psychosocial Cohort'
    )
  ) %>%
  dplyr::mutate(
    cohort = factor(cohort),
    cohort = forcats::fct_relevel(cohort, 'Total PREdiCCt Cohort')
  )

variables_cd <- c(
  'Location',
  'Behaviour',
  'Perianal',
  'HBI_cat',
  'Surgery',
  'Smoke',
  'Biologic'
)

data_table_cd %>%
  gtsummary::tbl_summary(
    by = cohort,
    include = variables_cd,
    missing_text = 'Missing data',
    label = list(
      Location ~ 'Montreal location',
      Behaviour ~ 'Montreal behaviour',
      Perianal ~ 'Perianal disease',
      HBI_cat ~ 'Harvey-Bradshaw Index',
      Surgery ~ 'Previous surgery for Crohn’s disease',
      Smoke ~ 'Smoking status (CD)',
      Biologic ~ 'Biologic use (CD)'
    )
  )


# UC specific variables

data_uc %<>%
  dplyr::select(
    ParticipantNo,
    Extent, 
    Mayo,
    Smoke,
    Biologic
  )

# Flag if a patient is in the psychosocial cohort
data_uc %<>%
  dplyr::mutate(
    psychosocial = dplyr::case_when(
      ParticipantNo %in% participants$ParticipantNo ~ 'Yes',
      .default = 'No'
    )
  )

# Trick to compare entire cohort to a subset
data_table_uc <- dplyr::bind_rows(
  data_uc %>%
    dplyr::mutate(cohort = 'predicct'),
  data_uc %>%
    dplyr::filter(psychosocial == 'Yes') %>%
    dplyr::mutate(cohort = 'psychosocial')
)

# Tidy up variables

# Partial Mayo as categorical
data_table_uc %<>%
  dplyr::mutate(
    Mayo_cat = dplyr::case_when(
    Mayo %in% c(0, 1) ~ '0-1',
    Mayo %in% c(2, 3, 4) ~ '2-4',
    Mayo %in% c(5, 6) ~ '5-6',
    Mayo %in% c(7, 8, 9) ~ '7-9'
    
  ))

# Cohort
data_table_uc %<>%
  dplyr::mutate(
    cohort = dplyr::case_match(
      cohort,
      'predicct' ~ 'Total PREdiCCt Cohort',
      'psychosocial' ~ 'Psychosocial Cohort'
    )
  ) %>%
  dplyr::mutate(
    cohort = factor(cohort),
    cohort = forcats::fct_relevel(cohort, 'Total PREdiCCt Cohort')
  )

variables_cd <- c(
  'Extent', 
  'Mayo_cat',
  'Smoke',
  'Biologic'
)

data_table_uc %>%
  gtsummary::tbl_summary(
    by = cohort,
    include = variables_cd,
    missing_text = 'Missing data',
    label = list(
      Extent ~ 'Montreal extent',
      Mayo_cat ~ 'Partial Mayo score',
      Smoke ~ 'Smoking status (UC)',
      Biologic ~ 'Biologic use (UC)'
    )
  )



# Try all together
data_table_all <- data_table %>%
  dplyr::left_join(
    data_table_uc %>%
      dplyr::select(-Smoke, -Biologic, -psychosocial), by = c('ParticipantNo', 'cohort')) %>%
  dplyr::left_join(
    data_table_cd %>%
      dplyr::select(-Smoke, -Biologic, -psychosocial), by = c('ParticipantNo', 'cohort'))

variables_all <- c(
  variables, 
  'Location',
  'Behaviour',
  'Perianal',
  'HBI_cat',
  'Surgery',
  'Extent', 
  'Mayo_cat'
  )

tbl <- data_table_all %>%
  gtsummary::tbl_strata(
    strata = cohort,
    .tbl_fun = ~
      .x %>% 
      gtsummary::tbl_summary(
        by = diagnosis2,
        include = variables_all,
        missing_text = 'Missing data'
      )
  ) 

tbl$table_body %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = c(stat_1_1, stat_2_1, stat_1_2, stat_2_2),
      .fns = function(x) {
        dplyr::case_match(
          x,
          '0 (NA%)' ~ '-',
          .default = x
        )
      }
    )
  )

tbl
