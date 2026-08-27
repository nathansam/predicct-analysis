library(tidyverse)
library(magrittr)


# Run HADS loading data only.


# Exclude cases with missing ParticipantId
# 60 removed - but they are all completely empty rows.
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

#exclude participants <17, 53 participants excluded, tot 1847

n_underage <- hads %>%
  dplyr::filter(age <= 17) %>%
  nrow()

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


#create age groups
hads %<>%
  dplyr::mutate(AgeGroup = cut(
    age,
    breaks = c(18, 24, 34, 44, 54, 65, Inf),
    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    include.lowest = TRUE
  ))

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
hads <- hads %>% 
  dplyr::left_join(
    IBD %>% select(ParticipantNo, FlaresInPastYear), 
    by = "ParticipantNo"
  )

#create flare groups
hads <- hads %>%
  dplyr::mutate(
    flare_group = ifelse(FlaresInPastYear == 0, "No flares", "1 or More Flares")
  )

# make flares into levels
hads %<>%
  dplyr::mutate(flare_group = factor(flare_group, levels = c("No flares", "1 or More Flares")))

# IBD control scores
hads %<>%
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
hads %<>% 
  dplyr::mutate(control_grouped = factor(control_grouped))

hads %<>% 
  dplyr::mutate(vas_control = factor(vas_control))

# Pivot longer
hads <- hads %>%
  tidyr::pivot_longer(
    cols = c(anxiety_hads, depression_hads),
    names_to = "hads_type", values_to = "hads_score"
  ) 

#score groups
hads <- hads %>% 
  mutate(
    score_group = cut(
      hads_score, 
      breaks = c(0, 7, 10, 21), 
      labels = c("0-7", "8-10", "11-21"), 
      include.lowest = TRUE)
  )

hads %<>%
  dplyr::filter(!is.na(score_group))


# Counts
# Anxiety
hads %>%
  dplyr::group_by(hads_type, diagnosis2, score_group) %>%
  dplyr::count() %>%
  dplyr::group_by(hads_type, diagnosis2) %>%
  dplyr::mutate(p = n/sum(n))
