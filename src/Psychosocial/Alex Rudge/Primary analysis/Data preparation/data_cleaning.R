# Common data cleaning for the primary psychosocial analyses
#
# This script creates one participant-level dataframe, data_common, containing
# the covariates shared by the analyses. Questionnaire-specific
# data and scoring remain in the individual Quarto documents.


# Paths to PREdiCCt data
data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"
outdir <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"
alex_data <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

# Load source data used by both primary analyses
demographics <- readxl::read_xlsx(
  glue::glue("{data.path}Baseline2022/demographics2022.xlsx")
)

IBD <- readxl::read_xlsx(
  glue::glue("{data.path}Baseline2022/IBD.xlsx")
)

demo_full <- readr::read_rds(glue::glue("{outdir}demo-full.rds"))
smoking <- readr::read_rds(glue::glue("{chiara}smoking.rds"))
IMD <- readr::read_rds(glue::glue("{chiara}IMD.rds"))
IBD_C <- readr::read_rds(glue::glue("{chiara}IBD_C.RDS"))

# Clean the demographics data and use it as the starting dataframe.
data_common <- demographics %>%
  filter(!is.na(ParticipantId)) %>%
  select(ParticipantNo, Sex, age, diagnosis)

# Clean the IBD data before joining it to the shared dataframe.
IBD_clean <- IBD %>%
  filter(!is.na(ParticipantId)) %>%
  select(ParticipantNo, FlaresInPastYear)

# Clean demo
demo_clean <- demo_full %>%
  select(
    ParticipantNo,
    SiteNo,
    FC,
    cat,
    Ethnicity,
    BMI,
    BMIcat,
    `IBD Duration`,
    CReactiveProtein,
    Biologic
  ) %>%
  dplyr::rename(
    FC_raw = FC,
    FC_cat = cat,
    IBD_duration = `IBD Duration`
  ) %>%
  # Log FC
  dplyr::mutate(
    FC = log(FC_raw)
  )

IBD_C_clean <- IBD_C %>%
  select(
    ParticipantNo,
    OverallControl,
    control_8
  )


# Make a clean common variables df
# Recode diagnosis.
data_common <- data_common %>%
  mutate(
    diagnosis2 = case_when(
      diagnosis == 1 ~ 1,
      diagnosis == 2 ~ 2,
      diagnosis == 3 ~ 2,
      diagnosis == 4 ~ 2
    ),
    diagnosis2 = factor(
      diagnosis2,
      levels = c("1", "2"),
      labels = c("CD", "UC/IBDU")
    )
  ) %>%
  dplyr::select(-diagnosis)

# Recode sex.
data_common <- data_common %>%
  mutate(
    Sex = factor(
      Sex,
      levels = c(1, 2),
      labels = c("Male", "Female")
    )
  )

# Recode age.
data_common <- data_common %>%
  mutate(
    age_decade = age / 10
  )

# Add previous-year flare history and create flare groups.
data_common <- data_common %>%
  left_join(IBD_clean, by = "ParticipantNo") %>%
  mutate(
    flare_group = factor(
      case_when(
        FlaresInPastYear == 0 ~ "No Flares",
        FlaresInPastYear == 1 ~ "1 Flare",
        FlaresInPastYear >= 2 ~ "2 or More Flares",
        .default = NA_character_
      ),
      levels = c("No Flares", "1 Flare", "2 or More Flares")
    )
  )

# Add faecal calprotectin. Keep the original measurement as FC_raw.
data_common <- data_common %>%
  left_join(demo_clean, by = "ParticipantNo")

# Add smoking status.
smoking_clean <- smoking %>%
  select(ParticipantNo, Smoke)

data_common <- data_common %>%
  left_join(smoking_clean, by = "ParticipantNo")

# Add index of multiple deprivation.
data_common <- data_common %>%
  left_join(IMD, by = "ParticipantNo")

# Add IBD control scores and convert grouped scores to factors.
data_common <- data_common %>%
  dplyr::left_join(IBD_C_clean, by = 'ParticipantNo')

# Convert diagnosis to a factor and set its level order.
data_common <- data_common %>%
  dplyr::mutate(
    diagnosis2 = factor(
      diagnosis2,
      levels = c("CD", "UC/IBDU")
    )
  )

# Keep site number as a character.
data_common <- data_common %>%
  dplyr::mutate(
    SiteNo = as.character(SiteNo)
  )

# Convert sex to a factor and set its level order.
data_common <- data_common %>%
  dplyr::mutate(
    Sex = factor(
      Sex,
      levels = c("Male", "Female")
    )
  )

# Convert ethnicity to a factor and set its level order.
data_common <- data_common %>%
  dplyr::mutate(
    Ethnicity = factor(
      Ethnicity,
      levels = c("White", "Non-white")
    )
  )

# Convert BMI category to a factor and set its level order.
data_common <- data_common %>%
  dplyr::mutate(
    BMIcat = factor(
      BMIcat,
      levels = c("Underweight", "Normal", "Overweight", "Obese")
    )
  )

# Convert flare group to a factor and set its level order.
data_common <- data_common %>%
  dplyr::mutate(
    flare_group = factor(
      flare_group,
      levels = c("No Flares", "1 Flare", "2 or More Flares")
    )
  )

# Convert biologic status to a factor and set its level order.
data_common <- data_common %>%
  dplyr::mutate(
    Biologic = factor(
      Biologic,
      levels = c("Never prescribed", "Previously", "Current")
    )
  )

# Convert smoking status to a factor and set its level order.
data_common <- data_common %>%
  dplyr::mutate(
    Smoke = factor(
      Smoke,
      levels = c("Never", "Previous", "Current")
    )
  )

# Convert IMD to a factor and set its level order.
data_common <- data_common %>%
  dplyr::mutate(
    IMD = factor(
      as.character(IMD),
      levels = as.character(1:5)
    )
  )

# Convert FC category to a factor and set its level order.
data_common <- data_common %>%
  dplyr::mutate(
    FC_cat = factor(
      FC_cat,
      levels = c("FC < 50", "FC 50-250", "FC > 250")
    )
  )

# Keep the shared variables in a consistent logical order.
data_common <- data_common %>%
  dplyr::select(
    ParticipantNo,
    SiteNo,
    diagnosis2,
    age,
    age_decade,
    Sex,
    Ethnicity,
    BMI,
    BMIcat,
    FlaresInPastYear,
    flare_group,
    IBD_duration,
    Biologic,
    Smoke,
    IMD,
    FC_raw,
    FC,
    FC_cat,
    CReactiveProtein,
    OverallControl,
    control_8
  )


# Save
readr::write_rds(
  data_common,
  file = glue::glue("{alex_data}common_variables.rds")
)
