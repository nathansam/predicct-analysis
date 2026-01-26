library(tidyverse)
library(magrittr)


# Run PHQ loading data only.

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
      breaks = c(0, 4, 9, 14, 30), 
      labels = c("None", "Mild", "Moderate", "Severe"), 
      include.lowest = TRUE)
  )


# Counts
phq %>%
  dplyr::group_by(diagnosis2, somatisation) %>%
  dplyr::count() %>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::mutate(p = n/sum(n))
