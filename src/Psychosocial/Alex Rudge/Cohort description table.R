library(tidyverse)
library(magrittr)

# Comparing the psychosocial cohort to the entire Predicct cohort


# Load in the Predicct cohort
data_cohort <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/predicct/processed/flares-controlled.RDS"
)

# Psychosocial cohort
# I will just use the HADS cohort for now

# So run HADS data cleaning

# Select relevant columns
data_cohort %<>%
  dplyr::select(
    ParticipantNo,
    SiteNo,
    diagnosis2,
    Sex,
    Age,
    Ethnicity,
    BMI,
    IMD,
    `IBD Duration`,
    Treatment,
    Smoke,
    FC,
    softflare,
    softflare_time,
    hardflare,
    hardflare_time
  )

# Flag if a patient is in the psychosocial cohort
data_cohort %<>%
  dplyr::mutate(
    psychosocial = dplyr::case_when(
      ParticipantNo %in% hads$ParticipantNo ~ 'Yes',
      .default = 'No'
    )
  )

# How many in psychosocial cohort
data_cohort %>%
  dplyr::count(psychosocial)


# Is missing being in the psychosocial cohort informative of flare?

# Soft
data_cohort %>%
  survfit(Surv(softflare_time, softflare) ~ psychosocial, data = .) %>%
  ggsurvplot(
    ., 
    data = data_cohort, 
    conf.int = TRUE, 
    risk.table = TRUE,
    pval = TRUE,
    pval.method = TRUE,
    legend.title = 'Psychosocial cohort',
    xlab = "Time from study recruitment (days)",
    ggtheme = theme_minimal())

# Very - needs mentioning in the paper.


# Hard
data_cohort %>%
  survfit(Surv(hardflare_time, hardflare) ~ psychosocial, data = .) %>%
  ggsurvplot(
    ., 
    data = data_cohort, 
    conf.int = TRUE, 
    risk.table = TRUE,
    pval = TRUE,
    pval.method = TRUE,
    legend.title = 'Psychosocial cohort',
    xlab = "Time from study recruitment (days)",
    ggtheme = theme_minimal())
