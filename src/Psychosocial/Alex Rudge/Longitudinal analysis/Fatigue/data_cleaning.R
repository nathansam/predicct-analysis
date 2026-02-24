library(tidyverse)
library(magrittr)
library(readxl)
library(openxlsx)


# File paths
data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"
redcap.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20231030/"
prefix <- "/Volumes/igmm/cvallejo-predicct/predicct/end-of-follow-up/"
outdir <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
flare.dir <- paste0("/Volumes/igmm/cvallejo-predicct/predicct/",
                    "final/20240308/Followup/")
chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"


# Monthly questionnaires 
fatigue_followup <- read.xlsx(paste0(data.path, "Followup/monthlyQ.xlsx"))

demographics <- read.xlsx(paste0(data.path, "Baseline2022/demographics2022.xlsx"))
IBD <- read.xlsx(paste0(data.path, "Baseline2022/IBD.xlsx"))
demo <- readRDS(paste0(outdir, "demo.RDS"))
flares <- readRDS(paste0(outdir, "flares/cutoff-flares.RDS"))
smoking <- readRDS(paste0(chiara, "smoking.rds"))
IMD <- readRDS(paste0(chiara, "IMD.rds"))
flares_hard <- readRDS(paste0(chiara, "flares_hard.RDS"))
flares_soft <- readRDS(paste0(chiara, "flares_soft.RDS"))
IBD_C <- readRDS(paste0(chiara, "IBD_C.RDS"))



# Data cleaning

# Fatigue question from IBD control score
fatigue <- IBD %>%
  dplyr::select(
    ParticipantId,
    ParticipantNo,
    SiteNo,
    OftenLackEnergy)

# Exclude missing
fatigue <- fatigue %>% 
  filter(!is.na(ParticipantId)) %>%
  dplyr::select(-ParticipantId)

fatigue %<>%
  dplyr::filter(
    !is.na(OftenLackEnergy)
  )

fatigue %<>%
  dplyr::mutate(
    OftenLackEnergy = dplyr::case_match(
      OftenLackEnergy,
      1 ~ 'Yes',
      2 ~ 'No',
      3 ~ 'Not sure'
    ) 
  ) %>%
  # Group "Not sure" with "No" due to low counts
  dplyr::mutate(
    OftenLackEnergy = dplyr::case_when(
      OftenLackEnergy == 'Not sure' ~ 'No',
      TRUE ~ OftenLackEnergy
    )
  ) %>%
  dplyr::mutate(
    OftenLackEnergy = forcats::fct_relevel(OftenLackEnergy, 'No', 'Yes')
  )

# Monthly fatigue
fatigue_followup %<>%
  dplyr::select(ParticipantNo, OftenLackEnergy, Q_month) %>%
  # Remove NA energy
  dplyr::filter(!is.na(OftenLackEnergy)) %>%
  dplyr::mutate(
    OftenLackEnergy = dplyr::case_match(
      OftenLackEnergy,
      1 ~ 'Yes',
      2 ~ 'No',
      3 ~ 'Not sure'
    ) 
  ) %>%
  # Group "Not sure" with "No" due to low counts
  dplyr::mutate(
    OftenLackEnergy = dplyr::case_when(
      OftenLackEnergy == 'Not sure' ~ 'No',
      TRUE ~ OftenLackEnergy
    )
  ) %>%
  dplyr::mutate(
    OftenLackEnergy = forcats::fct_relevel(OftenLackEnergy, 'No', 'Yes')
  )

# Combine with baseline

# Only include those with a value at baseline
fatigue_followup %<>%
  dplyr::filter(ParticipantNo %in% fatigue$ParticipantNo)

# Baseline fatigue is month 0
fatigue %<>%
  dplyr::mutate(Q_month = 0)

# Stack
fatigue %<>%
  dplyr::bind_rows(fatigue_followup)

# Rename Q_month to month
fatigue %<>%
  dplyr::rename(month = Q_month)

# Fill SiteNo
fatigue %<>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(SiteNo) %>% 
  dplyr::ungroup()


#add age and sex data from demographics
fatigue <- fatigue %>%
  left_join(
    demographics %>% select(ParticipantNo, Sex, age, diagnosis), 
    by  = "ParticipantNo")

# Recode diagnosis
fatigue %<>%
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

#exclude <18 yo = 53 participants (1899 -> 1846)
fatigue %<>% dplyr::filter(age >= 18)

#make sex into level
fatigue %<>% dplyr::mutate(Sex = factor(
  Sex,
  levels = c(1, 2),
  labels = c("Male", "Female"))
)

# Age groups
fatigue %<>%
  dplyr::mutate(AgeGroup = cut(age, 
                               breaks = c(18, 24, 34, 44, 54, 65, Inf), 
                               labels = c("18-24",
                                          "25-34",
                                          "35-44",
                                          "45-54",
                                          "55-64",
                                          "65+"),
                               include.lowest = TRUE))

#add flare data, FC, smoking
#flare data from IBD data set
fatigue <- fatigue %>% 
  left_join(
    IBD %>% select(ParticipantNo, FlaresInPastYear),
    by = 'ParticipantNo')


#create flare groups
fatigue <- fatigue %>%
  mutate(flare_group = ifelse(FlaresInPastYear == 0,
                              "No Flares",
                              "1 or More Flares"))

#make flares into levels
fatigue %<>% 
  dplyr::mutate(
    flare_group = factor(flare_group,
                         levels = c( "No Flares", "1 or More Flares"))
  )

# FC data
fatigue <- fatigue %>% 
  dplyr::left_join(
    demo %>% dplyr::select(ParticipantNo, FC, cat), 
    by = "ParticipantNo")

# Add smoking
fatigue %<>% 
  dplyr::left_join(
    smoking %>% dplyr::select(ParticipantNo, Smoke), 
    by = "ParticipantNo")

# Smoke as a factor
fatigue %<>%
  dplyr::mutate(Smoke = forcats::as_factor(Smoke)) %>%
  dplyr::mutate(Smoke = forcats::fct_relevel(Smoke, 'Never', 'Previous', 'Current'))

# Add IMD
fatigue %<>% 
  dplyr::left_join(
    IMD, 
    by = "ParticipantNo") 

# IMD as a factor
fatigue %<>%
  dplyr::mutate(
    IMD = as.factor(IMD)
  )

# IBD control scores
fatigue %<>%
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
fatigue %<>% 
  dplyr::mutate(control_grouped = factor(control_grouped))

fatigue %<>% 
  dplyr::mutate(vas_control = factor(vas_control))

# Age in decades
fatigue %<>%
  dplyr::mutate(
    age_decade = age/10
  )


# FC maximum detectable value is 1250
fatigue %<>%
  dplyr::mutate(
    FC = dplyr::case_when(
      FC > 1250 ~ 1250,
      .default = FC
    )
  )

# Log transform FC due to extreme positive skew
fatigue %<>%
  dplyr::mutate(
    FC = log(FC)
  )

# Arrange
fatigue %<>%
  dplyr::arrange(ParticipantNo, month)

# Survival data
data_soft_long <- fatigue %>% 
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)

data_hard_long <- fatigue %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = hardflare, time = hardflare_time)
