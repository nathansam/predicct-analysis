library(tidyverse)
library(magrittr)
library(readxl)
library(openxlsx)


# Filepaths
data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"
redcap.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20231030/"
prefix <- "/Volumes/igmm/cvallejo-predicct/predicct/end-of-follow-up/"
outdir <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
flare.dir <- paste0("/Volumes/igmm/cvallejo-predicct/predicct/",
                    "final/20240308/Followup/")
chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"


# Load data
lifeevents <- read.xlsx(paste0(data.path, "Baseline2022/lifeevents.xlsx"))

# Follow up
lifeevents_followup <- read.xlsx(paste0(data.path, "Followup/lifeevents.xlsx"))


demographics <- read.xlsx(paste0(
  data.path,
  "Baseline2022/demographics2022.xlsx"
))

IBD <- read.xlsx(paste0(data.path, "Baseline2022/IBD.xlsx"))

all_flares <- read.xlsx(paste0(flare.dir, "all-flares.xlsx"))
demo <- readRDS(paste0(outdir, "demo.RDS"))
flares <- readRDS(paste0(outdir, "flares/cutoff-flares.RDS"))
smoking <- readRDS(paste0(chiara, "smoking.rds"))
IMD <- readRDS(paste0(chiara, "IMD.rds"))
flares_soft <- readRDS(paste0(chiara, "flares_soft.RDS"))
flares_hard <- readRDS(paste0(chiara, "flares_hard.RDS"))
IBD_C <- readRDS(paste0(chiara, "IBD_C.RDS"))


# Data cleaning
# Remove NA
lifeevents <- lifeevents %>%
  filter(!is.na(ParticipantId))

# Remove anyone without a baseline
lifeevents_followup <- lifeevents_followup %>%
  filter(
    !is.na(ParticipantId),
    ParticipantNo %in% lifeevents$ParticipantNo
    )

lifeevents %<>% 
  dplyr::filter(!is.na(AnyLifeEvents))

lifeevents_followup %<>% 
  dplyr::filter(!is.na(AnyLifeEvents))

# Any life events
lifeevents %<>%
  dplyr::mutate(AnyLifeEvents = factor(AnyLifeEvents, levels = c(1, 2), labels = c("Yes", "No"))) %>%
  dplyr::mutate(AnyLifeEvents = forcats::fct_relevel(AnyLifeEvents, "No", "Yes"))

lifeevents_followup %<>%
  dplyr::mutate(AnyLifeEvents = factor(AnyLifeEvents, levels = c(1, 2), labels = c("Yes", "No"))) %>%
  dplyr::mutate(AnyLifeEvents = forcats::fct_relevel(AnyLifeEvents, "No", "Yes"))

# Select columns
lifeevents %<>%
  dplyr::select(
    ParticipantNo, SiteNo, AnyLifeEvents
  )

lifeevents_followup %<>%
  dplyr::select(
    ParticipantNo, AnyLifeEvents, Q_month
  )


# Month 0 at baseline
lifeevents %<>%
  dplyr::mutate(month = 0)

lifeevents_followup %<>%
  dplyr::rename(month = Q_month)

# Join together
lifeevents %<>%
  dplyr::bind_rows(
    lifeevents_followup
  )

# Fill NA SiteNo
lifeevents %<>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(SiteNo) %>%
  dplyr::ungroup()



# add age and sex data from demographics
lifeevents <- lifeevents %>%
  dplyr::inner_join(
    demographics %>% select(ParticipantNo, Sex, age, diagnosis),
    by = "ParticipantNo"
  )

# Recode diagnosis
lifeevents %<>%
  dplyr::mutate(
    diagnosis2 = dplyr::case_when(
      diagnosis == 1 ~ 1,
      diagnosis == 2 ~ 2,
      diagnosis == 3 ~ 2,
      diagnosis == 4 ~ 2
    )
  ) %>%
  dplyr::mutate(diagnosis2 = factor(
    diagnosis2,
    levels = c("1", "2"),
    labels = c("CD", "UC/IBDU")
  ))

# exclude <18 yo = 53 participants (1910 -> 1857)
lifeevents %<>% dplyr::filter(age > 17)

# make sex into factor
lifeevents %<>% dplyr::mutate(Sex = factor(
  Sex,
  levels = c(1, 2),
  labels = c("Male", "Female"))
)


lifeevents <- lifeevents %>%
  dplyr::left_join(
    IBD %>% dplyr::select(ParticipantNo, FlaresInPastYear)
  )

# create flare groups
lifeevents <- lifeevents %>%
  mutate(flare_group = ifelse(FlaresInPastYear == 0,
                              "No Flares",
                              "1 or More Flares"
  ))

# Make flares into levels
lifeevents %<>% 
  dplyr::mutate(flare_group = factor(
    flare_group,
    levels = c(
      "No Flares",
      "1 or More Flares"
    )
  ))

# FC data
lifeevents <- lifeevents %>% 
  dplyr::left_join(
    demo %>% dplyr::select(ParticipantNo, FC, cat), 
    by = "ParticipantNo")

# Add smoking
lifeevents %<>% 
  dplyr::left_join(
    smoking %>% dplyr::select(ParticipantNo, Smoke), 
    by = "ParticipantNo")

# Smoke as a factor
lifeevents %<>%
  dplyr::mutate(Smoke = forcats::as_factor(Smoke)) %>%
  dplyr::mutate(Smoke = forcats::fct_relevel(Smoke, 'Never', 'Previous', 'Current'))

# Add IMD
lifeevents %<>% 
  dplyr::left_join(
    IMD, 
    by = "ParticipantNo") 

# IMD as a factor
lifeevents %<>%
  dplyr::mutate(
    IMD = as.factor(IMD)
  ) 

# IBD control scores
lifeevents %<>%
  dplyr::left_join(
    IBD_C %>% dplyr::select(
      ParticipantNo,
      control_score,
      OverallControl,
      control_8,
      control_grouped,
      vas_control
    ),
    by = "ParticipantNo"
  )

#change control_grouped and vas_control from character into factors
lifeevents %<>% 
  dplyr::mutate(control_grouped = factor(control_grouped))

lifeevents %<>% 
  dplyr::mutate(vas_control = factor(vas_control))




# Age in decades
lifeevents %<>%
  dplyr::mutate(
    age_decade = age/10
  )


# FC maximum detectable value is 1250
lifeevents %<>%
  dplyr::mutate(
    FC = dplyr::case_when(
      FC > 1250 ~ 1250,
      .default = FC
    )
  )

# Log transform FC due to extreme positive skew
lifeevents %<>%
  dplyr::mutate(
    FC = log(FC)
  )

# Arrange
lifeevents %<>%
  dplyr::arrange(ParticipantNo, month)

# Survival
data_soft_long <- lifeevents %>% 
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)

data_hard_long <- lifeevents %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = hardflare, time = hardflare_time)
