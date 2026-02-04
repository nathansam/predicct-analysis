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
    BMIcat,
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

# Crohn's specific variables
data_cd %<>%
  dplyr::select(
    ParticipantNo,
    Location,
    Behaviour,
    Perianal,
    HBI,
    Surgery
  )

data_cohort %<>%
  dplyr::left_join(
    data_cd,
    by = "ParticipantNo"
  )



# UC specific variables
data_uc %<>%
  dplyr::select(
    ParticipantNo,
    Extent, 
    Mayo
  )

data_cohort %<>%
  dplyr::left_join(
    data_uc,
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
      'predicct' ~ 'Total PREdiCCt cohort',
      'psychosocial' ~ 'Psychosocial sub-cohort'
    )
  ) %>%
  dplyr::mutate(
    cohort = factor(cohort),
    cohort = forcats::fct_relevel(cohort, 'Total PREdiCCt cohort')
  )

# Harvey Bradshaw as categorical
data_table %<>%
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

# Partial Mayo as categorical
data_table %<>%
  dplyr::mutate(
    Mayo_cat = dplyr::case_when(
      Mayo %in% c(0, 1) ~ '0-1',
      Mayo %in% c(2, 3, 4) ~ '2-4',
      Mayo %in% c(5, 6) ~ '5-6',
      Mayo %in% c(7, 8, 9) ~ '7-9'
    ))

# Table
variables <- c(
  'Age',
  'Sex',
  'BMIcat',
  'Smoke',
  'IMD',
  'Ethnicity',
  'FC',
  'IBD_duration',
  'control_8',
  'OverallControl',
  'CReactiveProtein',
  'Biologic',
  'Location',
  'Behaviour',
  'Perianal',
  'HBI_cat',
  'Surgery',
  'Extent', 
  'Mayo_cat'
)





tbl <- data_table %>%
  gtsummary::tbl_strata(
    strata = diagnosis2,
    .tbl_fun = ~
      .x %>% 
      gtsummary::tbl_summary(
        by = cohort,
        include = variables,
        missing_text = 'Missing data',
        label = list(
          Age ~ "Age (years)",
          Sex ~ 'Sex',
          BMIcat ~ 'Body mass index',
          Smoke ~ 'Smoking status',
          Ethnicity ~ "Ethnicity",
          IMD ~ 'Index of multiple deprivation',
          IBD_duration ~ 'IBD Duration (years)',
          FC ~ 'Fecal calprotectin (ug/g)',
          control_8 ~ 'IBD-Control-8',
          OverallControl ~ 'IBD-Control-VAS',
          CReactiveProtein ~ 'C-reactive protein (mg/L)',
          Biologic ~ 'Biologic use',
          Location ~ 'Montreal location',
          Behaviour ~ 'Montreal behaviour',
          Perianal ~ 'Perianal disease',
          HBI_cat ~ 'Harvey-Bradshaw Index',
          Surgery ~ 'Previous surgery for Crohn’s disease',
          Extent ~ 'Montreal extent',
          Mayo_cat ~ 'Partial Mayo score'
        )
      ) %>%
      gtsummary::add_p(
        test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE, B = 1e5)
      ) 
  ) 

# Fix CD columns 
tbl$table_body %<>%
  dplyr::mutate(
    dplyr::across(
      # Select CD columns
      .cols = c(stat_1_1, stat_2_1),
      .fns = function(x) {
        dplyr::case_when(
          # Set UC specific variables to a dash
          variable == 'Extent' ~ NA,
          variable == 'Mayo' ~ NA,
          variable == 'Mayo_cat' ~ NA,
          .default = x
        )
      }
    )
  )

# Fix UC columns 
tbl$table_body %<>%
  dplyr::mutate(
    dplyr::across(
      # Select UC columns
      .cols = c(stat_1_2, stat_2_2),
      .fns = function(x) {
        # Set CD specific variables to a dash
        dplyr::case_when(
          variable == 'Location' ~ NA,
          variable == 'Behaviour' ~ NA,
          variable == 'Perianal' ~ NA,
          variable == 'HBI' ~ NA,
          variable == 'HBI_cat' ~ NA,
          variable == 'Surgery' ~ NA,
          .default = x
        )
      }
    )
  )

# Remove empty rows caused by disease specific binary variables
tbl$table_body %<>% 
  dplyr::filter(!(variable %in% c('Perianal', 'Surgery') & (label %in% c('No', 'Yes'))))


# Save as word
filepath <- "/Users/arudge/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Predicct/Tables/"

tbl %>%
  gtsummary::as_gt() %>%
  gt::gtsave(
    filename = paste0(filepath, "Table1.docx")
  )
