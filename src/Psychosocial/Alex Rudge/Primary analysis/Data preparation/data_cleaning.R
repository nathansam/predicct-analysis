# Common data cleaning for the primary psychosocial analyses
#
# This script creates one participant-level dataframe, data_common, containing
# the covariates shared by the analyses. Questionnaire-specific
# data and scoring remain in the individual Quarto documents.


# Paths to PREdiCCt data
if (file.exists("/.dockerenv")) {
  data.path <- "data/final/20221004/"
  outdir <- "data/processed"
  chiara <- "data/people/chiara/"
} else {
  data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"
  outdir <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
  chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"
}

# Load source data used by both primary analyses
demographics <- readxl::read_xlsx(
  file.path(data.path, "Baseline2022", "demographics2022.xlsx")
)

IBD <- readxl::read_xlsx(
  file.path(data.path, "Baseline2022", "IBD.xlsx")
)

demo <- readRDS(file.path(outdir, "demo.RDS"))
smoking <- readRDS(file.path(chiara, "smoking.rds"))
IMD <- readRDS(file.path(chiara, "IMD.rds"))
IBD_C <- readRDS(file.path(chiara, "IBD_C.RDS"))

# Clean the demographics data and use it as the starting dataframe.
data_common <- demographics %>%
  filter(!is.na(ParticipantId)) %>%
  select(ParticipantNo, Sex, age, diagnosis)

# Recode diagnosis, sex, and age variables.
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
    ),
    Sex = factor(
      Sex,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    AgeGroup = cut(
      age,
      breaks = c(18, 24, 34, 44, 54, 65, Inf),
      labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
      include.lowest = TRUE
    ),
    age_decade = age / 10
  )

# Clean the IBD data before joining it to the shared dataframe.
IBD_clean <- IBD %>%
  filter(!is.na(ParticipantId)) %>%
  select(ParticipantNo, FlaresInPastYear)

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
demo_clean <- demo %>%
  select(ParticipantNo, FC, cat) %>%
  rename(FC_raw = FC) %>%
  # Log FC
  dplyr::mutate(
    FC = log(FC_raw)
  )

data_common <- data_common %>%
  left_join(demo_clean, by = "ParticipantNo")

# Add smoking status and set the intended factor order.
smoking_clean <- smoking %>%
  select(ParticipantNo, Smoke)

data_common <- data_common %>%
  left_join(smoking_clean, by = "ParticipantNo") %>%
  mutate(
    Smoke = forcats::as_factor(Smoke),
    Smoke = forcats::fct_relevel(Smoke, "Never", "Previous", "Current")
  )

# Add index of multiple deprivation and convert it to a factor.
data_common <- data_common %>%
  left_join(IMD, by = "ParticipantNo") %>%
  mutate(IMD = factor(IMD))

# Add IBD control scores and convert grouped scores to factors.
IBD_C_clean <- IBD_C %>%
  select(
    ParticipantNo,
    OverallControl,
    control_8
  )

data_common <- data_common %>%
  dplyr::left_join(IBD_C_clean, by = 'ParticipantNo')


# Save
readr::write_rds(
  data_common,
  file = "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/common_variables.rds"
)
