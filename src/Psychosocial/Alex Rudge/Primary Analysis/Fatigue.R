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
      1 ~ 'Yes',
      2 ~ 'No',
      3 ~ 'Not sure',
      4 ~ 'Not on treatment'
    ) 
  )

data_control %<>%
  dplyr::mutate(
    MissPlannedActivities = dplyr::case_match(
      MissPlannedActivities,
      1 ~ 'Yes',
      2 ~ 'No',
      3 ~ 'Not sure'
    ) 
  )

data_control %<>%
  dplyr::mutate(
    WakeUpAtNight = dplyr::case_match(
      WakeUpAtNight,
      1 ~ 'Yes',
      2 ~ 'No',
      3 ~ 'Not sure'
    ) 
  )

data_control %<>%
  dplyr::mutate(
    SignificantPain = dplyr::case_match(
      SignificantPain,
      1 ~ 'Yes',
      2 ~ 'No',
      3 ~ 'Not sure'
    ) 
  )

data_control %<>%
  dplyr::mutate(
    OftenLackEnergy = dplyr::case_match(
      OftenLackEnergy,
      1 ~ 'Yes',
      2 ~ 'No',
      3 ~ 'Not sure'
    ) 
  ) %>%
  dplyr::mutate(
    OftenLackEnergy = forcats::fct_relevel(OftenLackEnergy, 'No', 'Not sure', 'Yes')
  )

data_control %<>%
  dplyr::mutate(
    AnxiousDepressed = dplyr::case_match(
      AnxiousDepressed,
      1 ~ 'Yes',
      2 ~ 'No',
      3 ~ 'Not sure'
    ) 
  )

data_control %<>%
  dplyr::mutate(
    NeedChangeTreatment = dplyr::case_match(
      NeedChangeTreatment,
      1 ~ 'Yes',
      2 ~ 'No',
      3 ~ 'Not sure'
    ) 
  )



data_control <- data_control %>% 
  filter(!is.na(ParticipantId))

#add age and sex data from demographics
data_control <- data_control %>%
  left_join(
    demographics %>% select(ParticipantNo, Sex, age, diagnosis), 
    by  = "ParticipantNo")

# Recode diagnosis
data_control %<>%
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
data_control %<>% dplyr::filter(age >= 18)

#make sex into level
data_control %<>% dplyr::mutate(Sex = factor(
  Sex,
  levels = c(1, 2),
  labels = c("Male", "Female"))
)

# Age groups
data_control %<>%
  dplyr::mutate(AgeGroup = cut(age, 
                               breaks = c(18, 24, 34, 44, 54, 65, Inf), 
                               labels = c("18-24",
                                          "25-34",
                                          "35-44",
                                          "45-54",
                                          "55-64",
                                          "65+"),
                               include.lowest = TRUE))


# FC data
data_control <- data_control %>% 
  dplyr::left_join(
    demo %>% dplyr::select(ParticipantNo, FC, cat), 
    by = "ParticipantNo")

# Add smoking
data_control %<>% 
  dplyr::left_join(
    smoking %>% dplyr::select(ParticipantNo, Smoke), 
    by = "ParticipantNo")

# Smoke as a factor
data_control %<>%
  dplyr::mutate(Smoke = forcats::as_factor(Smoke)) %>%
  dplyr::mutate(Smoke = forcats::fct_relevel(Smoke, 'Never', 'Previous', 'Current'))

# Add IMD
data_control %<>% 
  dplyr::left_join(
    IMD, 
    by = "ParticipantNo") 

# IMD as a factor
data_control %<>%
  dplyr::mutate(
    IMD = as.factor(IMD)
  )

# Age in decades
data_control %<>%
  dplyr::mutate(
    age_decade = age/10
  )


# FC maximum detectable value is 1250
data_control %<>%
  dplyr::mutate(
    FC = dplyr::case_when(
      FC > 1250 ~ 1250,
      .default = FC
    )
  )

# Log transform FC due to extreme positive skew
data_control %<>%
  dplyr::mutate(
    FC = log(FC)
  )

# Survival 
data_survival_soft <- data_control %>% dplyr::inner_join(
  flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
  by = 'ParticipantNo'
) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)

# UC
data_survival_soft_uc <- data_survival_soft %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU')

# CD
data_survival_soft_cd <- data_survival_soft %>%
  dplyr::filter(diagnosis2 == 'CD')


# Hard flares
data_survival_hard <- data_control %>% dplyr::inner_join(
  flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
  by = 'ParticipantNo'
) %>%
  dplyr::mutate(DiseaseFlareYN = hardflare, time = hardflare_time)

# UC
data_survival_hard_uc <- data_survival_hard %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU')

# CD
data_survival_hard_cd <- data_survival_hard %>%
  dplyr::filter(diagnosis2 == 'CD')



# Kaplan-Meier
# Plotting Kaplan-Meier curves
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

legend.title = 'Fatigue'
legend.labs = c('Yes', 'No', 'Not sure')
palette = okabe_ito
dependent = 'OftenLackEnergy'

custom_theme = theme_minimal() + 
  theme(
    title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

# Kaplan-Meier
# Patient-reported
summon_km_curves(
  data = data_survival_soft,
  dependent = dependent,
  title = "Time to patient-reported flare in IBD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

# Objective
summon_km_curves(
  data = data_survival_hard,
  dependent = dependent,
  title = "Time to objective flare in IBD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

# UC
# Patient-reported
summon_km_curves(
  data = data_survival_soft_uc,
  dependent = dependent,
  title = "Time to patient-reported flare in ulcerative colitis",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

# Objective
summon_km_curves(
  data = data_survival_hard_uc,
  dependent = dependent,
  title = "Time to objective flare in ulcerative colitis",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

# CD
# Patient-reported
summon_km_curves(
  data = data_survival_soft_cd,
  dependent = dependent,
  title = "Time to patient-reported flare in Crohn's disease",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

# Objective
summon_km_curves(
  data = data_survival_hard_cd,
  dependent = dependent,
  title = "Time to objective flare in Crohn's disease",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

