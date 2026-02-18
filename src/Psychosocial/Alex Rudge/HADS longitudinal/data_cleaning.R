library(tidyverse)
library(magrittr)
library(readxl)
library(openxlsx)


# Load data ####
data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"
redcap.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20231030/"
prefix <- "/Volumes/igmm/cvallejo-predicct/predicct/end-of-follow-up/"
outdir <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"

# Baseline hads
hads <- readxl::read_xlsx(paste0(data.path, "Baseline2022/hads.xlsx"))

# Longitudinal hads
hads_followup <- readxl::read_xlsx(paste0(data.path, "Followup/hads.xlsx"))

# Demographics
demographics <- read.xlsx(paste0(data.path, "Baseline2022/demographics2022.xlsx"))
IBD <- read.xlsx(paste0(data.path, "Baseline2022/IBD.xlsx"))
smoking <- readRDS(paste0(chiara, "smoking.rds"))
IMD <- readRDS(paste0(chiara, "IMD.rds"))
demo <- readRDS(paste0(outdir, "demo.RDS"))
flares <- readRDS(paste0(outdir, "flares/cutoff-flares.RDS"))
IBD_C <- readRDS(paste0(chiara, "IBD_C.RDS"))
IBD_FC <- readRDS(paste0(chiara, "IBD_FC.RDS"))
flares_soft <- readRDS(paste0(chiara, "flares_soft.RDS"))
flares_hard <- readRDS(paste0(chiara, "flares_hard.RDS"))



# Data cleaning ####

# Cleaning the longitudinal hads
hads_followup %<>% 
  filter(!is.na(ParticipantId))

# Rescale
hads_followup %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = FeelTense:CanEnjoyBookTV,
      .fns = ~. - 1)
  )

#correcting inversely coded HADS
hads_followup %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = c("FeelTense", 
                "FrightenedFeelingSomethingAwful", 
                "WorryingThoughts", 
                "FeelCheerful", 
                "FeelSlowedDown",
                "LostInterestAppearance",
                "FeelRestless",
                "SuddenFeelingsPanic"),
      .fns = ~ 3 - .
    )
  )

#anxiety HADS composite score
hads_followup %<>% 
  dplyr::mutate(anxiety_hads = FeelTense + 
                  FrightenedFeelingSomethingAwful + 
                  WorryingThoughts +
                  SitAtEase +
                  FrightenedFeelingButterflies +
                  FeelRestless +
                  SuddenFeelingsPanic)

#depression HADS composite score
hads_followup %<>% 
  dplyr::mutate(depression_hads = EnjoyThings +
                  CanLaugh +
                  FeelCheerful +
                  FeelSlowedDown +
                  LostInterestAppearance +
                  LookForward +
                  CanEnjoyBookTV)

#remove breakdown hads columns
hads_followup %<>%
  dplyr::select(ParticipantNo, Q_month, anxiety_hads, depression_hads)

# Wide format
hads_followup %<>%
  tidyr::pivot_wider(names_from = Q_month, values_from = c(anxiety_hads, depression_hads))



hads <- hads %>%
  filter(!is.na(ParticipantId))

demographics <- demographics %>% 
  filter(!is.na(ParticipantId))

IBD <- IBD %>% 
  filter(!is.na(ParticipantId))

# join demographics
hads %<>%
  dplyr::left_join(demographics %>%
                     dplyr::select(ParticipantNo, Sex, age, diagnosis),
                   by = "ParticipantNo")

# Convert diagnosis column to factor with labels
hads %<>%
  dplyr::mutate(
    diagnosis2 = dplyr::case_when(
      diagnosis == 1 ~ 1,
      diagnosis == 2 ~ 2,
      diagnosis == 3 ~ 2,
      diagnosis == 4 ~ 2
    )
  ) %>%
  dplyr::mutate(
    diagnosis2 = factor(
      diagnosis2,
      levels = c("1", "2"), 
      labels = c("CD", "UC/IBDU")
    )
  )

hads %<>%
  dplyr::mutate(Sex = factor(
    Sex,
    levels = c(1, 2),
    labels = c("Male", "Female")
  )
  )

# Exclude underage
hads %<>%
  dplyr::filter(age > 17)

# Rescale
hads %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = FeelTense:CanEnjoyBookTV,
      .fns = ~. - 1)
  )

#correcting inversely coded HADS
hads %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = c("FeelTense", 
                "FrightenedFeelingSomethingAwful", 
                "WorryingThoughts", 
                "FeelCheerful", 
                "FeelSlowedDown",
                "LostInterestAppearance",
                "FeelRestless",
                "SuddenFeelingsPanic"),
      .fns = ~ 3 - .
    )
  )

#anxiety HADS composite score
hads %<>% 
  dplyr::mutate(anxiety_hads = FeelTense + 
                  FrightenedFeelingSomethingAwful + 
                  WorryingThoughts +
                  SitAtEase +
                  FrightenedFeelingButterflies +
                  FeelRestless +
                  SuddenFeelingsPanic)

#depression HADS composite score
hads %<>% 
  dplyr::mutate(depression_hads = EnjoyThings +
                  CanLaugh +
                  FeelCheerful +
                  FeelSlowedDown +
                  LostInterestAppearance +
                  LookForward +
                  CanEnjoyBookTV)

#remove breakdown hads columns
hads %<>%
  dplyr::select(-c(FeelTense:CanEnjoyBookTV))

# Add the follow up hads
hads %<>% 
  dplyr::left_join(
    hads_followup, by = "ParticipantNo"
  )
  
# FC data
hads <- hads %>% 
  dplyr::left_join(
    demo %>% dplyr::select(ParticipantNo, FC, cat), 
    by = "ParticipantNo")

# Add smoking
hads %<>% 
  dplyr::left_join(
    smoking %>% dplyr::select(ParticipantNo, Smoke), 
    by = "ParticipantNo")

# Smoke as a factor
hads %<>%
  dplyr::mutate(Smoke = forcats::as_factor(Smoke)) %>%
  dplyr::mutate(Smoke = forcats::fct_relevel(Smoke, 'Never', 'Previous', 'Current'))

# Add IMD
hads %<>% 
  dplyr::left_join(
    IMD, 
    by = "ParticipantNo") 

# IMD as a factor
hads %<>%
  dplyr::mutate(
    IMD = as.factor(IMD)
  )

# Join flare data
# hads <- hads %>% 
#   dplyr::left_join(
#     IBD %>% select(ParticipantNo, FlaresInPastYear), 
#     by = "ParticipantNo"
#   )
# 
# #create flare groups
# hads <- hads %>%
#   dplyr::mutate(
#     flare_group = ifelse(FlaresInPastYear == 0, "No flares", "1 or More Flares")
#   )

# make flares into levels
# hads %<>%
#   dplyr::mutate(flare_group = factor(flare_group, levels = c("No flares", "1 or More Flares")))
# 
# # IBD control scores
# hads %<>%
#   dplyr::left_join(
#     IBD_C %>% dplyr::select(
#       ParticipantNo,
#       control_score,
#       OverallControl,
#       control_8,
#       control_grouped,
#       vas_control
#     ),
#     by = "ParticipantNo"
#   )
# 
# #change control_grouped and vas_control from character into factors
# hads %<>% 
#   dplyr::mutate(control_grouped = factor(control_grouped))
# 
# hads %<>% 
#   dplyr::mutate(vas_control = factor(vas_control))

# Pivot longer
# hads <- hads %>%
#   tidyr::pivot_longer(
#     cols = c(anxiety_hads, depression_hads),
#     names_to = "hads_type", values_to = "hads_score"
#   ) 

#score groups
# hads <- hads %>% 
#   mutate(
#     score_group = cut(
#       hads_score, 
#       breaks = c(0, 7, 21), 
#       labels = c("0-7", "8-21"), 
#       include.lowest = TRUE)
#   )

# Age in decades
hads %<>%
  dplyr::mutate(
    age_decade = age/10
  )


# FC maximum detectable value is 1250
hads %<>%
  dplyr::mutate(
    FC = dplyr::case_when(
      FC > 1250 ~ 1250,
      .default = FC
    )
  )

# Log transform FC due to extreme positive skew
hads %<>%
  dplyr::mutate(
    FC = log(FC)
  )

hads %<>%
  dplyr::filter(!(is.na(anxiety_hads) & is.na(depression_hads)))


# Survival data
data_anxiety_soft <- hads %>% 
  dplyr::select(-tidyselect::starts_with("depression")) %>%
  dplyr::inner_join(
  flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
  by = 'ParticipantNo'
) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)

data_anxiety_hard <- hads %>% 
  dplyr::select(-tidyselect::starts_with("depression")) %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = hardflare, time = hardflare_time)

# Depression
data_depression_soft <- hads %>% 
  dplyr::select(-tidyselect::starts_with("anxiety")) %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)

data_depression_hard <- hads %>% 
  dplyr::select(-tidyselect::starts_with("anxiety")) %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = hardflare, time = hardflare_time)


# Long format
# Anxiety
data_anxiety_soft_long <- data_anxiety_soft %>%
  dplyr::rename(anxiety_hads_0 = anxiety_hads) %>%
  tidyr::pivot_longer(
    cols = c(anxiety_hads_0, anxiety_hads_12, anxiety_hads_24),
    names_to = "month",
    names_prefix = "anxiety_hads_",
    values_to = "anxiety_hads"
  )

data_anxiety_hard_long <- data_anxiety_hard %>%
  dplyr::rename(anxiety_hads_0 = anxiety_hads) %>%
  tidyr::pivot_longer(
    cols = c(anxiety_hads_0, anxiety_hads_12, anxiety_hads_24),
    names_to = "month",
    names_prefix = "anxiety_hads_",
    values_to = "anxiety_hads"
  )

# Depression
data_depression_soft_long <- data_depression_soft %>%
  dplyr::rename(depression_hads_0 = depression_hads) %>%
  tidyr::pivot_longer(
    cols = c(depression_hads_0, depression_hads_12, depression_hads_24),
    names_to = "month",
    names_prefix = "depression_hads_",
    values_to = "depression_hads"
  )

data_depression_hard_long <- data_depression_hard %>%
  dplyr::rename(depression_hads_0 = depression_hads) %>%
  tidyr::pivot_longer(
    cols = c(depression_hads_0, depression_hads_12, depression_hads_24),
    names_to = "month",
    names_prefix = "depression_hads_",
    values_to = "depression_hads"
  )