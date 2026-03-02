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
physicalactivity <- read.xlsx(paste0(
  data.path,
  "Baseline2022/physicalactivity.xlsx"
))

# Follow up
physicalactivity_followup <- read.xlsx(paste0(
  data.path,
  "Followup/physicalactivity.xlsx"
))

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
exercise <- physicalactivity %>%
  filter(!is.na(ParticipantId))

exercise_followup <- physicalactivity_followup %>%
  filter(!is.na(ParticipantId))


# Tidy - find and exclude if NA in VigorousWork, ModerateWork, WalkBicycle,
# VigrorousSport, and ModerateSport
exercise %<>%
  filter(!dplyr::if_all(
    .cols = c(
      VigorousWork,
      ModerateWork,
      WalkBicycle,
      VigorousSport,
      ModerateSport
    ),
    .fns = is.na
  ))

# look at rows where there are NA values in key questions, exclude those with
# inconsistent answers (no valid response) - manually done, 6 values excluded
# exercise %>%
#   filter(
#     dplyr::if_any(
#       .cols = 
#     c(
#       VigorousWork,
#       ModerateWork,
#       WalkBicycle,
#       VigorousSport,
#       ModerateSport
#     ),
#     .fns = is.na))

exercise <- exercise %>%
  filter(!(ParticipantId %in% c(1395, 4685, 4803, 1452, 4827, 9609)))

# For yes/no questions, remaining NA values can be replaced with 2
exercise %<>%
  tidyr::replace_na(
    list(
      VigorousWork = 2,
      ModerateWork = 2,
      WalkBicycle = 2,
      VigorousSport = 2,
      ModerateSport = 2
    )
  )

# Exclude 10 participants who report 16 or more hours in any one activity,
# as per WHO guidelines
exercise <- exercise %>%
  filter(
    is.na(VigorousWorkHours) | VigorousWorkHours < 16,
    is.na(ModerateWorkHours) | ModerateWorkHours < 16,
    is.na(WalkBicycleHours) | WalkBicycleHours < 16,
    is.na(VigorousSportHours) | VigorousSportHours < 16,
    is.na(ModerateSportHours) | ModerateSportHours < 16
  )


# check that there are no cases of reporting 'no' to the question whilst
# providing answers in the time boxes

# exercise %>%
#   filter(VigorousWork == 2 &
#            (VigorousWorkHours > 0 | VigorousWorkMinutes > 0)) %>%
#   nrow()
# 
# exercise %>%
#   filter(ModerateWork == 2 &
#            (ModerateWorkHours > 0 | ModerateWorkMinutes > 0)) %>%
#   nrow()
# 
# exercise %>%
#   filter(WalkBicycle == 2 &
#            (WalkBicycleHours > 0 | WalkBicycleMinutes > 0)) %>%
#   nrow()
# 
# exercise %>%
#   filter(VigorousSport == 2 &
#            (VigorousSportHours > 0 | VigorousSportMinutes > 0)) %>%
#   nrow()
# 
# exercise %>%
#   filter(ModerateSport == 2 &
#            (ModerateSportHours > 0 | ModerateSportMinutes > 0)) %>%
#   nrow()

# No cases, all previously resolved.

# Similarly, fix cases of reporting 1 to the questions but for zero/NA hours.
# ModerateWork
MW_wrong <- exercise %>%
  filter(ModerateWork == 1 & (ModerateWorkDays == 0 | is.na(ModerateWorkDays)))

# 2 can be corrected recoding 1->2 ParticipantId 4700, 14446
exercise <- exercise %>%
  mutate(ModerateWork = dplyr::case_when(
    ParticipantId %in% c(4700, 14446) ~ 2,
    TRUE ~ ModerateWork
  ))

# 8 excluded as report 0 days but have answers in other domains
# Excluding subjects who say they do moderate work but for 0 days

exercise <- exercise %>%
  filter(!(ModerateWork == 1 &
             (ModerateWorkDays == 0 | is.na(ModerateWorkDays))
  ))

# WalkBicycle
WB_wrong <- exercise %>%
  filter(WalkBicycle == 1 & (WalkBicycleDays == 0 | is.na(WalkBicycleDays)))

# 2 participants say they do but for 0 days
exercise <- exercise %>%
  filter(!(WalkBicycle == 1 & (WalkBicycleDays == 0 | is.na(WalkBicycleDays))))

# VigorousSport
VS_wrong <- exercise %>% filter(VigorousSport == 1 &
                                  (VigorousSportDays == 0 |
                                     is.na(VigorousSportDays)))

# Correct 3 participants who responded 1 but said for 0 days/hours/mins
exercise <- exercise %>%
  mutate(VigorousSport = dplyr::case_when(ParticipantId %in% c(7012, 9555, 9559) ~ 2,
                                          TRUE ~ VigorousSport))

# exclude 2 participants who did not include number of days
exercise <- exercise %>% filter(!(VigorousSport == 1 &
                                    (VigorousSportDays == 0 |
                                       is.na(VigorousSportDays))))

# ModerateSport
MS_wrong <- exercise %>%
  filter(ModerateSport == 1 & (ModerateSportDays == 0 | is.na(ModerateSportDays)))

exercise <- exercise %>%
  mutate(ModerateSport = dplyr::case_when(
    ParticipantId %in%
      c(61, 186, 3550, 7024, 7048, 9735, 12079) ~
      2,
    TRUE ~ ModerateSport
  ))

# exclude 6 participants who did not include number of days
exercise <- exercise %>% filter(!(ModerateSport == 1 &
                                    (ModerateSportDays == 0 | is.na(ModerateSportDays))
))

# check if anyone recorded days >7 in any days column - no cases
# check if any minutes >60 - no cases
# exercise %>%
#   filter(VigorousWorkDays > 7 | VigorousWorkMinutes > 60) %>%
#   nrow()
# exercise %>%
#   filter(ModerateWorkDays > 7 | ModerateWorkMinutes > 60) %>%
#   nrow()
# exercise %>%
#   filter(WalkBicycleDays > 7 | WalkBicycleMinutes > 60) %>%
#   nrow()
# exercise %>%
#   filter(VigorousSportDays > 7 | VigorousSportMinutes > 60) %>%
#   nrow()
# exercise %>%
#   filter(ModerateSportDays > 7 | ModerateSportMinutes > 60) %>%
#   nrow()

# Replace NA with zero in days/hours/minutes columns
exercise <- exercise %>% tidyr::replace_na(
  list(
    VigorousWorkDays = 0,
    VigorousWorkHours = 0,
    VigorousWorkMinutes = 0,
    ModerateWorkDays = 0,
    ModerateWorkHours = 0,
    ModerateWorkMinutes = 0,
    WalkBicycleDays = 0,
    WalkBicycleHours = 0,
    WalkBicycleMinutes = 0,
    VigorousSportDays = 0,
    VigorousSportHours = 0,
    VigorousSportMinutes = 0,
    ModerateSportDays = 0,
    ModerateSportHours = 0,
    ModerateSportMinutes = 0
  )
)

# Calculate total mins of vigorous and moderate exercise per week
exercise <- exercise %>%
  mutate(
    VigorousWeek =
      VigorousWorkDays * (VigorousWorkMinutes + (VigorousWorkHours * 60)) +
      VigorousSportDays * (VigorousSportMinutes + (VigorousSportHours * 60)),
    ModerateWeek = 
      ModerateWorkDays * (ModerateWorkMinutes + (ModerateWorkHours * 60)) +
      ModerateSportDays * (ModerateSportMinutes + (ModerateSportHours * 60)) +
      WalkBicycleDays * (WalkBicycleMinutes + (WalkBicycleHours * 60))
  )

# total mins + METs
exercise <- exercise %>% 
  mutate(
    ExerciseMins = VigorousWeek + ModerateWeek,
    METs = (VigorousWeek * 8) + (ModerateWeek * 4)
  )

# Targets for exercise (WHO) are 75 minutes vigorous activity or 150 minutes
# moderate activity or 600 METs per week
exercise <- exercise %>%
  mutate(
    MinimumExercise = dplyr::case_when(
      VigorousWeek >= 75 | ModerateWeek >= 150 | METs >= 600 ~ "Yes",
      TRUE ~ "No"
    ),
    LackExercise = dplyr::case_when(
      VigorousWeek < 75 & ModerateWeek < 150 & METs < 600 ~ "Yes",
      TRUE ~ "No"
    )
  )

# MinimumExercise as a factor
exercise %<>%
  dplyr::mutate(MinimumExercise = factor(MinimumExercise,
                                         levels = c('Yes', 'No')))


# exclude if minute exercise/week >6720 - 10 cases
# exercise %>% dplyr::filter(ExerciseMins > 6720) %>% nrow()

exercise <- exercise %>% dplyr::filter(ExerciseMins < 6720)


# Follow up


# Tidy - find and exclude if NA in VigorousWork, ModerateWork, WalkBicycle,
# VigrorousSport, and ModerateSport
exercise_followup %<>%
  filter(!dplyr::if_all(
    .cols = c(
      VigorousWork,
      ModerateWork,
      WalkBicycle,
      VigorousSport,
      ModerateSport
    ),
    .fns = is.na
  ))


# For yes/no questions, remaining NA values can be replaced with 2
exercise_followup %<>%
  tidyr::replace_na(
    list(
      VigorousWork = 2,
      ModerateWork = 2,
      WalkBicycle = 2,
      VigorousSport = 2,
      ModerateSport = 2
    )
  )

# Exclude 12 participants who report 16 or more hours in any one activity,
# as per WHO guidelines
exercise_followup <- exercise_followup %>%
  filter(
    is.na(VigorousWorkHours) | VigorousWorkHours < 16,
    is.na(ModerateWorkHours) | ModerateWorkHours < 16,
    is.na(WalkBicycleHours) | WalkBicycleHours < 16,
    is.na(VigorousSportHours) | VigorousSportHours < 16,
    is.na(ModerateSportHours) | ModerateSportHours < 16
  )


# exclude 5 participants who did not include number of days
exercise_followup <- exercise_followup %>% filter(!(ModerateSport == 1 &
                                    (ModerateSportDays == 0 | is.na(ModerateSportDays))
))

# Replace NA with zero in days/hours/minutes columns
exercise_followup <- exercise_followup %>% tidyr::replace_na(
  list(
    VigorousWorkDays = 0,
    VigorousWorkHours = 0,
    VigorousWorkMinutes = 0,
    ModerateWorkDays = 0,
    ModerateWorkHours = 0,
    ModerateWorkMinutes = 0,
    WalkBicycleDays = 0,
    WalkBicycleHours = 0,
    WalkBicycleMinutes = 0,
    VigorousSportDays = 0,
    VigorousSportHours = 0,
    VigorousSportMinutes = 0,
    ModerateSportDays = 0,
    ModerateSportHours = 0,
    ModerateSportMinutes = 0
  )
)

# Calculate total mins of vigorous and moderate exercise_followup per week
exercise_followup <- exercise_followup %>%
  mutate(
    VigorousWeek =
      VigorousWorkDays * (VigorousWorkMinutes + (VigorousWorkHours * 60)) +
      VigorousSportDays * (VigorousSportMinutes + (VigorousSportHours * 60)),
    ModerateWeek = 
      ModerateWorkDays * (ModerateWorkMinutes + (ModerateWorkHours * 60)) +
      ModerateSportDays * (ModerateSportMinutes + (ModerateSportHours * 60)) +
      WalkBicycleDays * (WalkBicycleMinutes + (WalkBicycleHours * 60))
  )

# total mins + METs
exercise_followup <- exercise_followup %>% 
  mutate(
    exercise_followupMins = VigorousWeek + ModerateWeek,
    METs = (VigorousWeek * 8) + (ModerateWeek * 4)
  )

# Targets for exercise_followup (WHO) are 75 minutes vigorous activity or 150 minutes
# moderate activity or 600 METs per week
exercise_followup <- exercise_followup %>%
  mutate(
    MinimumExercise = dplyr::case_when(
      VigorousWeek >= 75 | ModerateWeek >= 150 | METs >= 600 ~ "Yes",
      TRUE ~ "No"
    )
  )

# MinimumExercise as a factor
exercise_followup %<>%
  dplyr::mutate(MinimumExercise = factor(MinimumExercise,
                                         levels = c('Yes', 'No')))


# exclude if minute exercise_followup/week >6720 - 27 cases
exercise_followup <- exercise_followup %>% dplyr::filter(exercise_followupMins < 6720)


# Select columns
exercise %<>%
  dplyr::select(
    ParticipantNo, SiteNo, MinimumExercise
  )

exercise_followup %<>%
  dplyr::select(
    ParticipantNo, MinimumExercise, Q_month
  )


# Month 0 at baseline
exercise %<>%
  dplyr::mutate(month = 0)

exercise_followup %<>%
  dplyr::rename(month = Q_month)

# Join together
exercise %<>%
  dplyr::bind_rows(
    exercise_followup
  )

# Fill NA SiteNo
exercise %<>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(SiteNo) %>%
  dplyr::ungroup()



# add age and sex data from demographics
exercise <- exercise %>%
  dplyr::inner_join(
    demographics %>% select(ParticipantNo, Sex, age, diagnosis),
    by = "ParticipantNo"
  )

# Recode diagnosis
exercise %<>%
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
exercise %<>% dplyr::filter(age > 17)

# make sex into factor
exercise %<>% dplyr::mutate(Sex = factor(
  Sex,
  levels = c(1, 2),
  labels = c("Male", "Female"))
)


exercise <- exercise %>%
  dplyr::left_join(
    IBD %>% dplyr::select(ParticipantNo, FlaresInPastYear)
  )

# create flare groups
exercise <- exercise %>%
  mutate(flare_group = ifelse(FlaresInPastYear == 0,
                              "No Flares",
                              "1 or More Flares"
  ))

# Make flares into levels
exercise %<>% 
  dplyr::mutate(flare_group = factor(
    flare_group,
    levels = c(
      "No Flares",
      "1 or More Flares"
    )
  ))

# FC data
exercise <- exercise %>% 
  dplyr::left_join(
    demo %>% dplyr::select(ParticipantNo, FC, cat), 
    by = "ParticipantNo")

# Add smoking
exercise %<>% 
  dplyr::left_join(
    smoking %>% dplyr::select(ParticipantNo, Smoke), 
    by = "ParticipantNo")

# Smoke as a factor
exercise %<>%
  dplyr::mutate(Smoke = forcats::as_factor(Smoke)) %>%
  dplyr::mutate(Smoke = forcats::fct_relevel(Smoke, 'Never', 'Previous', 'Current'))

# Add IMD
exercise %<>% 
  dplyr::left_join(
    IMD, 
    by = "ParticipantNo") 

# IMD as a factor
exercise %<>%
  dplyr::mutate(
    IMD = as.factor(IMD)
  ) 

# IBD control scores
exercise %<>%
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
exercise %<>% 
  dplyr::mutate(control_grouped = factor(control_grouped))

exercise %<>% 
  dplyr::mutate(vas_control = factor(vas_control))




# Age in decades
exercise %<>%
  dplyr::mutate(
    age_decade = age/10
  )


# FC maximum detectable value is 1250
exercise %<>%
  dplyr::mutate(
    FC = dplyr::case_when(
      FC > 1250 ~ 1250,
      .default = FC
    )
  )

# Log transform FC due to extreme positive skew
exercise %<>%
  dplyr::mutate(
    FC = log(FC)
  )

# Arrange
exercise %<>%
  dplyr::arrange(ParticipantNo, month)


# Save
filepath_save <- '/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Longitudinal analysis/'

readr::write_rds(
  x = exercise,
  file = glue::glue("{filepath_save}exercise_long.rds")
)


# Survival
data_soft_long <- exercise %>% 
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)

data_hard_long <- exercise %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = hardflare, time = hardflare_time)
