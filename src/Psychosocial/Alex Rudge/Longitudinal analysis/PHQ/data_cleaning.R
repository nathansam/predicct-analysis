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
phq <- read.xlsx(paste0(data.path, "Baseline2022/phq.xlsx"))
phq_followup <- read.xlsx(paste0(data.path, "Followup/phq.xlsx"))

health <- read.xlsx(paste0(data.path, "Baseline2022/health.xlsx"))
demographics <- read.xlsx(paste0(data.path, "Baseline2022/demographics2022.xlsx"))
IBD <- read.xlsx(paste0(data.path, "Baseline2022/IBD.xlsx"))
phq <- read.xlsx(paste0(data.path, "Baseline2022/phq.xlsx"))
demo <- readRDS(paste0(outdir, "demo.RDS"))
flares <- readRDS(paste0(outdir, "flares/cutoff-flares.RDS"))
smoking <- readRDS(paste0(chiara, "smoking.rds"))
IMD <- readRDS(paste0(chiara, "IMD.rds"))
flares_hard <- readRDS(paste0(chiara, "flares_hard.RDS"))
flares_soft <- readRDS(paste0(chiara, "flares_soft.RDS"))
IBD_C <- readRDS(paste0(chiara, "IBD_C.RDS"))



# Data cleaning

phq <- phq %>% 
  filter(!is.na(ParticipantId))

#add age and sex data from demographics
phq <- phq %>%
  left_join(
    demographics %>% select(ParticipantNo, Sex, age, diagnosis), 
    by  = "ParticipantNo")

# Recode diagnosis
phq %<>%
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
phq %<>% dplyr::filter(age >= 18)

#make sex into level
phq %<>% dplyr::mutate(Sex = factor(
  Sex,
  levels = c(1, 2),
  labels = c("Male", "Female"))
)

# Age groups
phq %<>%
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
phq <- phq %>% 
  left_join(
    IBD %>% select(ParticipantNo, FlaresInPastYear),
    by = 'ParticipantNo')


#create flare groups
phq <- phq %>%
  mutate(flare_group = ifelse(FlaresInPastYear == 0,
                              "No Flares",
                              "1 or More Flares"))

#make flares into levels
phq %<>% 
  dplyr::mutate(
    flare_group = factor(flare_group,
                         levels = c( "No Flares", "1 or More Flares"))
  )

# FC data
phq <- phq %>% 
  dplyr::left_join(
    demo %>% dplyr::select(ParticipantNo, FC, cat), 
    by = "ParticipantNo")

# Add smoking
phq %<>% 
  dplyr::left_join(
    smoking %>% dplyr::select(ParticipantNo, Smoke), 
    by = "ParticipantNo")

# Smoke as a factor
phq %<>%
  dplyr::mutate(Smoke = forcats::as_factor(Smoke)) %>%
  dplyr::mutate(Smoke = forcats::fct_relevel(Smoke, 'Never', 'Previous', 'Current'))

# Add IMD
phq %<>% 
  dplyr::left_join(
    IMD, 
    by = "ParticipantNo") 

# IMD as a factor
phq %<>%
  dplyr::mutate(
    IMD = as.factor(IMD)
  )


# IBD control scores
phq %<>%
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
phq %<>% 
  dplyr::mutate(control_grouped = factor(control_grouped))

phq %<>% 
  dplyr::mutate(vas_control = factor(vas_control))


# PHQ numbering scale was shifted by 1 - fix here
phq %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = StomachPain:TroubleSleeping,
      .fns = ~. - 1)
  )

# Menstrual cramps replace NA with 0 (Men)
phq %<>%
  tidyr::replace_na(
    list(MenstrualCramps = 0)
  )

# Add all PHQ-15 columns to make final score
phq <- phq %>% 
  mutate(
    TotalPHQ = StomachPain +
      BackPain +
      PainInArms +
      MenstrualCramps +
      Headaches +
      ChestPain +
      Dizziness +
      FaintingSpells +
      HeartPound +
      ShortnessBreath +
      SexualIntercourse +
      ConstipationDiarrhoea +
      NauseaIndigestion +
      FeelingTired +
      TroubleSleeping)

# Remove patients with any missing values
phq %<>%
  dplyr::filter(!is.na(TotalPHQ))


# Make new variable for no, mild, moderate and severe levels of somatisation
phq <- phq %>% 
  mutate(
    somatisation = cut(
      TotalPHQ, 
      breaks = c(0, 4, 9, 30), 
      labels = c("None", "Mild", "ModSev"), 
      include.lowest = TRUE)
  )

# Clean longitudinal phq
phq_followup <- phq_followup %>% 
  filter(!is.na(ParticipantId))

# PHQ numbering scale was shifted by 1 - fix here
phq_followup %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = StomachPain:TroubleSleeping,
      .fns = ~. - 1)
  )

# Menstrual cramps replace NA with 0 (Men)
phq_followup %<>%
  tidyr::replace_na(
    list(MenstrualCramps = 0)
  )

# Add all PHQ-15 columns to make final score
phq_followup <- phq_followup %>% 
  mutate(
    TotalPHQ = StomachPain +
      BackPain +
      PainInArms +
      MenstrualCramps +
      Headaches +
      ChestPain +
      Dizziness +
      FaintingSpells +
      HeartPound +
      ShortnessBreath +
      SexualIntercourse +
      ConstipationDiarrhoea +
      NauseaIndigestion +
      FeelingTired +
      TroubleSleeping)

# Remove patients with any missing values
phq_followup %<>%
  dplyr::filter(!is.na(TotalPHQ))


# Make new variable for no, mild, moderate and severe levels of somatisation
phq_followup <- phq_followup %>% 
  mutate(
    somatisation = cut(
      TotalPHQ, 
      breaks = c(0, 4, 9, 30), 
      labels = c("None", "Mild", "ModSev"), 
      include.lowest = TRUE)
  )

# Join longitudinal and baseline

phq_followup %<>%
  dplyr::select(
    ParticipantNo, TotalPHQ, somatisation, Q_month
  )

# Pivot wider
phq_followup %<>%
  tidyr::pivot_wider(names_from = Q_month, values_from = c(TotalPHQ, somatisation))

phq %<>%
  dplyr::left_join(
    phq_followup, by = "ParticipantNo"
  )

# Remove unneeded columns
phq %<>%
  dplyr::select(-c(StomachPain:counter))

# Age in decades
phq %<>%
  dplyr::mutate(
    age_decade = age/10
  )


# FC maximum detectable value is 1250
phq %<>%
  dplyr::mutate(
    FC = dplyr::case_when(
      FC > 1250 ~ 1250,
      .default = FC
    )
  )

# Log transform FC due to extreme positive skew
phq %<>%
  dplyr::mutate(
    FC = log(FC)
  )



# Save phq in long format

phq_long <- phq %>%
  dplyr::rename(
    somatisation_0 = somatisation,
    TotalPHQ_0 = TotalPHQ
  ) %>%
  tidyr::pivot_longer(
    cols = c(somatisation_0, somatisation_12, somatisation_24, TotalPHQ_0, TotalPHQ_12, TotalPHQ_24),
    names_to = c(".value", "month"),
    names_sep = "_"
  ) %>%
  # Month numeric
  dplyr::mutate(
    month = as.numeric(month)
  )

filepath_save <- '/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Longitudinal analysis/'

readr::write_rds(
  x = phq_long,
  file = glue::glue("{filepath_save}phq_long.rds")
)

# Survival data
data_soft <- phq %>% 
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)

data_hard <- phq %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = hardflare, time = hardflare_time)

# Long form
data_soft_long <- data_soft %>%
  dplyr::rename(
    somatisation_0 = somatisation,
    TotalPHQ_0 = TotalPHQ
    ) %>%
  tidyr::pivot_longer(
    cols = c(somatisation_0, somatisation_12, somatisation_24, TotalPHQ_0, TotalPHQ_12, TotalPHQ_24),
    names_to = c(".value", "month"),
    names_sep = "_"
  )

data_hard_long <- data_hard %>%
  dplyr::rename(
    somatisation_0 = somatisation,
    TotalPHQ_0 = TotalPHQ
  ) %>%
  tidyr::pivot_longer(
    cols = c(somatisation_0, somatisation_12, somatisation_24, TotalPHQ_0, TotalPHQ_12, TotalPHQ_24),
    names_to = c(".value", "month"),
    names_sep = "_"
  )