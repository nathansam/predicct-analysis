library(tidyverse)
library(magrittr)
library(naniar)
library(ggmice)

# Missingness

# HADS
# Raw data is called hads

# Need to get data without filtering out missing hads

# Filter id because these are just empty rows
data_hads <- hads %>%
  filter(!is.na(ParticipantId))

# join demographics
data_hads %<>%
  dplyr::left_join(demographics %>%
                     dplyr::select(ParticipantNo, Sex, age, diagnosis),
                   by = "ParticipantNo")

#anxiety HADS composite score
data_hads %<>% 
  dplyr::mutate(anxiety_hads = FeelTense + 
                  FrightenedFeelingSomethingAwful + 
                  WorryingThoughts +
                  SitAtEase +
                  FrightenedFeelingButterflies +
                  FeelRestless +
                  SuddenFeelingsPanic)

#depression HADS composite score
data_hads %<>% 
  dplyr::mutate(depression_hads = EnjoyThings +
                  CanLaugh +
                  FeelCheerful +
                  FeelSlowedDown +
                  LostInterestAppearance +
                  LookForward +
                  CanEnjoyBookTV)

#remove breakdown hads columns
data_hads %<>%
  dplyr::select(-c(FeelTense:CanEnjoyBookTV))

#add FC data
data_hads %<>% 
  dplyr::left_join(
    demo %>% dplyr::select(ParticipantNo, FC, cat, IMD, Smoke), 
    by = "ParticipantNo")

#Join flare data
data_hads %<>% 
  dplyr::left_join(
    IBD %>% select(ParticipantNo, FlaresInPastYear), 
    by = "ParticipantNo"
  )

#create flare groups
data_hads %<>%
  dplyr::mutate(
    flare_group = ifelse(FlaresInPastYear == 0, "No flares", "1 or More Flares")
  )

# IBD control scores
data_hads %<>%
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

# Filtering age < 18 is inclusion criteria
data_hads %<>%
  dplyr::filter(age > 17)

# Survival outcome
# Soft flares
data_hads %<>% dplyr::left_join(
  flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
  by = 'ParticipantNo'
)

# Hard flares
data_hads %<>% dplyr::left_join(
  flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
  by = 'ParticipantNo'
)

# Select columns needed
data_hads %<>%
  dplyr::select(
    diagnosis,
    age,
    Sex,
    IMD,
    FC,
    flare_group,
    OverallControl,
    Smoke,
    SiteNo,
    anxiety_hads,
    depression_hads,
    softflare,
    softflare_time,
    hardflare,
    hardflare_time
  )

# Visualise missingness
gg_miss_var(data_hads)

ggmice::plot_pattern(data_hads, rotate = TRUE)

ggmice::plot_corr(data_hads, rotate = TRUE)
