library(tidyverse)
library(magrittr)

# Calculating control 8 score from the IBD file

# Control 8 questionnaire
# Answering a question with Yes/no/not sure. Good response scores 2, bad response scores 0, not sure scores 1.

# Questions
# 1. Controlled in past 2 weeks? 
  # We are missing this.
# 2. Current treatment useful?
# 3. Missed any activities?
# 4. Wake up at night?
# 5. Suffer from significant pain?
# 6. Lack in energy?
# 7. Anxious or depressed?
# 8. Think you need to change treatment?

data_control <- IBD %>%
  dplyr::select(
    ParticipantId,
    ParticipantNo,
    TreatmentUseful,
    MissPlannedActivities,
    WakeUpAtNight,
    SignificantPain,
    OftenLackEnergy,
    AnxiousDepressed,
    NeedChangeTreatment, 
    OverallControl
  ) 

# Answers are coded as Yes = 1, No = 2, Not sure = 3.
# Current treatment has option 4, not on any treatment.

# Recode
data_control %<>%
  dplyr::mutate(
    TreatmentUseful = dplyr::case_match(
      TreatmentUseful,
      1 ~ 2,
      2 ~ 0,
      3 ~ 1,
      4 ~ 2 # Chiara assigned a value of 2 if not on treatment, which makes sense as its controlled
    ) 
  )

data_control %<>%
  dplyr::mutate(
    MissPlannedActivities = dplyr::case_match(
      MissPlannedActivities,
      1 ~ 0,
      2 ~ 2,
      3 ~ 1
    ) 
  )

data_control %<>%
  dplyr::mutate(
    WakeUpAtNight = dplyr::case_match(
      WakeUpAtNight,
      1 ~ 0,
      2 ~ 2,
      3 ~ 1
    ) 
  )

data_control %<>%
  dplyr::mutate(
    SignificantPain = dplyr::case_match(
      SignificantPain,
      1 ~ 0,
      2 ~ 2,
      3 ~ 1
    ) 
  )

data_control %<>%
  dplyr::mutate(
    OftenLackEnergy = dplyr::case_match(
      OftenLackEnergy,
      1 ~ 0,
      2 ~ 2,
      3 ~ 1
    ) 
  )

data_control %<>%
  dplyr::mutate(
    AnxiousDepressed = dplyr::case_match(
      AnxiousDepressed,
      1 ~ 0,
      2 ~ 2,
      3 ~ 1
    ) 
  )

data_control %<>%
  dplyr::mutate(
    NeedChangeTreatment = dplyr::case_match(
      NeedChangeTreatment,
      1 ~ 0,
      2 ~ 2,
      3 ~ 1
    ) 
  )

# Sum to get the score
data_control %<>%
  dplyr::mutate(
    control_8 = 
      TreatmentUseful +
      MissPlannedActivities +
      WakeUpAtNight +
      SignificantPain +
      OftenLackEnergy +
      AnxiousDepressed +
      NeedChangeTreatment +
      2
    )

# Scatter plot
data_control %>%
  ggplot(aes(x = OverallControl, y = control_8)) +
  geom_point() +
  geom_smooth()

# Pearson's correlation
data_control %$%
  cor(x = OverallControl, y = control_8, use = "complete.obs", method = 'pearson')
