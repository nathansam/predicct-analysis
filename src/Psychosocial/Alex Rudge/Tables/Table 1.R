library(tidyverse)
library(magrittr)
library(gtsummary)


# Comparing the psychosocial cohort to the entire Predicct cohort

# Paths to PREdiCCt data
processed <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
alex_data <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

# Load in the Predicct cohort
data_cohort <- readr::read_rds(
  file = glue::glue("{alex_data}common_variables.rds")
)

data_cd <- readr::read_rds(
  file = glue::glue("{processed}demo-cd.rds")
)

data_uc <- readr::read_rds(
  file = glue::glue("{processed}demo-uc.rds")
)



# Psychosocial cohort
# Load participants

participants <- readr::read_rds(
  file = glue::glue("{alex_data}participants.rds")
)

# Select relevant columns
data_cohort %<>%
  dplyr::select(
    ParticipantNo,
    SiteNo,
    diagnosis2,
    Sex,
    age,
    Ethnicity,
    BMIcat,
    IMD,
    IBD_duration,
    Biologic,
    Smoke,
    FC_raw,
    CReactiveProtein,
    OverallControl,
    control_8
  )

# Remove all patients < 18
# Else the age signal could be due to the exclusion of minors.
data_cohort %<>%
  dplyr::filter(age >= 18)

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


# Do not compare entire cohort to subset
data_table <- data_cohort %>%
  dplyr::mutate(
    cohort = dplyr::case_when(
      psychosocial == "Yes" ~ "psychosocial",
      TRUE ~ "non psychosocial"
    )
  ) %>%
  dplyr::select(-psychosocial)

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
      'non psychosocial' ~ 'No',
      'psychosocial' ~ 'Yes'
    )
  ) %>%
  dplyr::mutate(
    cohort = factor(cohort),
    cohort = forcats::fct_relevel(cohort, 'Yes')
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
    )) %>%
  dplyr::mutate(
    Mayo_cat = factor(Mayo_cat),
    Mayo_cat = forcats::fct_relevel(
      Mayo_cat,
      '0-1',
      '2-4',
      '5-6',
      '7-9'
    )
  )

# Table
variables <- c(
  'age',
  'Sex',
  'BMIcat',
  'Smoke',
  'IMD',
  'Ethnicity',
  'FC_raw',
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
          age ~ "Age (years)",
          Sex ~ 'Sex',
          BMIcat ~ 'Body mass index',
          Smoke ~ 'Smoking status',
          Ethnicity ~ "Ethnicity",
          IMD ~ 'Index of multiple deprivation',
          IBD_duration ~ 'IBD duration (years)',
          FC_raw ~ 'Fecal calprotectin (ug/g)',
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
      ) %>%
      gtsummary::add_q(method = 'fdr') %>%
      gtsummary::bold_p(q = TRUE)
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

# Convert to gt after editing the gtsummary table body
tbl <- tbl %>%
  gtsummary::as_gt() %>%
  gt::tab_spanner(
    label = gt::md("**Completed a psychosocial questionnaire**"),
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

# Save as Word and HTML
filepath <- "/Users/arudge/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Predicct/Tables/"

tbl %>%
  gt::gtsave(
    filename = paste0(filepath, "Table1.docx")
  )

tbl %>%
  gt::gtsave(
    filename = paste0(filepath, "Table1.html")
  )
