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
psqi <- read.xlsx(paste0(data.path, "Baseline2022/psqi.xlsx"))

# Follow up
psqi_followup <- read.xlsx(paste0(data.path, "Followup/psqi.xlsx"))


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
psqi <- psqi %>%
  filter(!is.na(ParticipantId))

psqi_followup <- psqi_followup %>%
  filter(!is.na(ParticipantId))

# Calculate sleep disturbance
psqi %<>%
  dplyr::filter(
    !dplyr::if_any(.cols = c(7, 10, 12, 14:23, 25:28), .fns = is.na)
  )

psqi %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = c("CantGetToSleep",
                "WakeUpMiddleNight",
                "UseBathroom",
                "CannotBreath",
                "CoughOrSnore",
                "FeelTooCold",
                "FeelTooWarm",
                "BadDreams",
                "HavePain",
                "OtherTroubleSleeping",
                "SleepQuality",
                "SleepMedicines",
                "StayingAwake",
                "EnoughEnthusiasm"),
      .fns = ~. - 1)
  )

#comp1 (mutate SleepQuality to comp1)
psqi <- psqi %>% 
  dplyr::rename(comp1 = SleepQuality)

#comp2
psqi <- psqi %>%
  dplyr::mutate(FallAsleepMns = case_when(
    FallAsleepMns <= 14 ~ 0,
    FallAsleepMns <= 30 ~ 1,
    FallAsleepMns <= 60 ~ 2,
    TRUE ~ 3
  ))

psqi <- psqi %>%
  dplyr::mutate(comp2 = case_when(
    (FallAsleepMns + CantGetToSleep) %in% c(0) ~ 0,
    (FallAsleepMns + CantGetToSleep) %in% c(1, 2) ~ 1,
    (FallAsleepMns + CantGetToSleep) %in% c(3, 4) ~ 2,
    (FallAsleepMns + CantGetToSleep) %in% c(5, 6) ~ 3
  ))

psqi %<>%
  tidyr::replace_na(
    list(SleepEachNightMns = 0)
  )

psqi <- psqi %>%
  dplyr::mutate(TotalSleepMinutes = SleepEachNightHrs * 60 + SleepEachNightMns,
                comp3 = case_when(
                  TotalSleepMinutes < 300 ~ 3,
                  TotalSleepMinutes >= 300 & TotalSleepMinutes <= 359 ~ 2,
                  TotalSleepMinutes >= 360 & TotalSleepMinutes <= 419 ~ 1,
                  TotalSleepMinutes >= 420 ~ 0)
  ) 

# Correcting non 24h clock entries.
psqi<- psqi %>%
  dplyr::mutate(
    BedTimeHrs = dplyr::case_when(
      ParticipantNo %in% c(110415, 110463) ~ BedTimeHrs,
      BedTimeHrs == 7 ~ 19,
      BedTimeHrs == 8 ~ 20,
      BedTimeHrs == 9 ~ 21,
      BedTimeHrs == 10 ~ 22,
      BedTimeHrs == 11 ~ 23,
      BedTimeHrs == 12 ~ 00,
      BedTimeHrs == 13 ~ 1,
      BedTimeHrs == 0 ~ 00,
      BedTimeHrs == 1 ~ 01,
      BedTimeHrs == 2 ~ 02,
      BedTimeHrs == 3 ~ 03,
      BedTimeHrs == 4 ~ 04,
      TRUE ~ BedTimeHrs
    )
  )

#if mins bed time/wake up time is na assume 0
psqi %<>%
  tidyr::replace_na(
    list(BedTimeMns = 0, 
         WokenUpMns = 0)
  )

#Three obvious typos found, 2 corrected above + 1 excluded (found when looking at TotalSleepMins - calculated later)
#Participant 170004 Bed time 22.30 wake up 20.00 reported 5 hours total sleep - exclude
psqi <- psqi %>%
  dplyr::filter(ParticipantNo != 170004)

#Participant No 110351 woken up hours 16, should be 6 - corrected above

#Participant 120042 woken up hours 19 but with other data provided would be logical for this to be 9 - correct this
psqi <- psqi %>%
  mutate(WokenUpHrs = dplyr::case_when(
    ParticipantNo == 110351 ~ 6, 
    ParticipantNo == 120042 ~ 9,
    .default = WokenUpHrs))

# Calculate time spent in bed as difference between bed time and wake up time
# Create date times using lubridate and calculate interval
psqi <- psqi %>%
  dplyr::mutate(
    # Bed Time - automatically sets to 1970-01-01
    BedTimeDT = lubridate::make_datetime(hour = BedTimeHrs, min = BedTimeMns),
    # Wake up next morning 1970-01-02
    WokenUpDT = lubridate::make_datetime(day = 2, hour = WokenUpHrs, min = WokenUpMns))

# If asleep after midnight, correct the day
psqi %<>%
  dplyr::mutate(
    BedTimeDT = dplyr::case_when(
      BedTimeHrs >= 0 & BedTimeHrs <= 8 ~ BedTimeDT + lubridate::days(1),
      TRUE ~ BedTimeDT
    )
  )

# Calculate total minutes in bed
psqi %<>%
  dplyr::mutate(
    TotalBedMinutes = lubridate::interval(BedTimeDT, WokenUpDT)/lubridate::minutes()
  )

#calculate comp 4
# Sleep efficiency
psqi <- psqi %>% 
  mutate(SleepEff = TotalSleepMinutes/TotalBedMinutes * 100)

# Convert to a 0-3 scale score.
psqi <- psqi %>%
  mutate(comp4 = case_when(
    SleepEff < 65 ~ 3, 
    SleepEff < 75 ~ 2,
    SleepEff < 85 ~ 1,
    SleepEff >= 85 ~ 0, 
  ))


#comp 5 
psqi <- psqi %>%
  mutate(TotalTrouble = rowSums(
    select(., WakeUpMiddleNight:OtherTroubleSleeping)))

# Categorise
psqi %<>%
  dplyr::mutate(
    comp5 = dplyr::case_when(
      TotalTrouble == 0 ~ 0, 
      TotalTrouble <= 9 ~ 1,
      TotalTrouble <= 18 ~ 2,
      TotalTrouble <= 27 ~ 3
    )
  )

psqi <- psqi %>% 
  dplyr::select(-TotalTrouble)

#comp6
psqi <- psqi %>% 
  dplyr::rename(comp6 = SleepMedicines)

#comp7
psqi <- psqi %>% 
  dplyr::mutate(
    comp7 = dplyr::case_when(
      (StayingAwake + EnoughEnthusiasm) %in% c(0) ~ 0,
      (StayingAwake + EnoughEnthusiasm) %in% c(1, 2) ~ 1,
      (StayingAwake + EnoughEnthusiasm) %in% c(3, 4) ~ 2,
      (StayingAwake + EnoughEnthusiasm) %in% c(5, 6) ~ 3
    )
  )

#TotalComp
psqi <- psqi %>% 
  dplyr::mutate(TotalScore = comp1 + comp2 + comp3 + comp4 + comp5 + comp6 + comp7)

#score > 5 is "poor sleep"
psqi <- psqi %>%
  dplyr::mutate(SleepDisturbance = if_else(TotalScore <= 5, "No", "Yes"))

#tidy psqi}
psqi <- psqi %>% dplyr::select(-(7:35))

# SleepDisturbance as a factor

psqi %<>%
  dplyr::mutate(
    SleepDisturbance = factor(SleepDisturbance, levels = c("No", "Yes"))
  )


# Same for follow up data
# Luckily sleep columns are the same number 
psqi_followup %<>%
  dplyr::filter(
    !dplyr::if_any(.cols = c(7, 10, 12, 14:23, 25:28), .fns = is.na)
  )

psqi_followup %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = c("CantGetToSleep",
                "WakeUpMiddleNight",
                "UseBathroom",
                "CannotBreath",
                "CoughOrSnore",
                "FeelTooCold",
                "FeelTooWarm",
                "BadDreams",
                "HavePain",
                "OtherTroubleSleeping",
                "SleepQuality",
                "SleepMedicines",
                "StayingAwake",
                "EnoughEnthusiasm"),
      .fns = ~. - 1)
  )

#comp1 (mutate SleepQuality to comp1)
psqi_followup <- psqi_followup %>% 
  dplyr::rename(comp1 = SleepQuality)

#comp2
psqi_followup <- psqi_followup %>%
  dplyr::mutate(FallAsleepMns = case_when(
    FallAsleepMns <= 14 ~ 0,
    FallAsleepMns <= 30 ~ 1,
    FallAsleepMns <= 60 ~ 2,
    TRUE ~ 3
  ))

psqi_followup <- psqi_followup %>%
  dplyr::mutate(comp2 = case_when(
    (FallAsleepMns + CantGetToSleep) %in% c(0) ~ 0,
    (FallAsleepMns + CantGetToSleep) %in% c(1, 2) ~ 1,
    (FallAsleepMns + CantGetToSleep) %in% c(3, 4) ~ 2,
    (FallAsleepMns + CantGetToSleep) %in% c(5, 6) ~ 3
  ))

psqi_followup %<>%
  tidyr::replace_na(
    list(SleepEachNightMns = 0)
  )

psqi_followup <- psqi_followup %>%
  dplyr::mutate(TotalSleepMinutes = SleepEachNightHrs * 60 + SleepEachNightMns,
                comp3 = case_when(
                  TotalSleepMinutes < 300 ~ 3,
                  TotalSleepMinutes >= 300 & TotalSleepMinutes <= 359 ~ 2,
                  TotalSleepMinutes >= 360 & TotalSleepMinutes <= 419 ~ 1,
                  TotalSleepMinutes >= 420 ~ 0)
  ) 

# Correcting non 24h clock entries.
# Need to manually check night shift workers e.g., 7:30 am sleep wakes up at 14
psqi_followup<- psqi_followup %>%
  dplyr::mutate(
    BedTimeHrs = dplyr::case_when(
      BedTimeHrs == 8 ~ 20,
      BedTimeHrs == 9 ~ 21,
      BedTimeHrs == 10 ~ 22,
      BedTimeHrs == 11 ~ 23,
      BedTimeHrs == 12 ~ 00,
      BedTimeHrs == 13 ~ 1,
      BedTimeHrs == 0 ~ 00,
      BedTimeHrs == 1 ~ 01,
      BedTimeHrs == 2 ~ 02,
      BedTimeHrs == 3 ~ 03,
      BedTimeHrs == 4 ~ 04,
      TRUE ~ BedTimeHrs
    )
  )

#if mins bed time/wake up time is na assume 0
psqi_followup %<>%
  tidyr::replace_na(
    list(BedTimeMns = 0, 
         WokenUpMns = 0)
  )

# Calculate time spent in bed as difference between bed time and wake up time
# Create date times using lubridate and calculate interval
psqi_followup <- psqi_followup %>%
  dplyr::mutate(
    # Bed Time - automatically sets to 1970-01-01
    BedTimeDT = lubridate::make_datetime(hour = BedTimeHrs, min = BedTimeMns),
    # Wake up next morning 1970-01-02
    WokenUpDT = lubridate::make_datetime(day = 2, hour = WokenUpHrs, min = WokenUpMns))

# If asleep after midnight, correct the day
psqi_followup %<>%
  dplyr::mutate(
    BedTimeDT = dplyr::case_when(
      BedTimeHrs >= 0 & BedTimeHrs <= 8 ~ BedTimeDT + lubridate::days(1),
      TRUE ~ BedTimeDT
    )
  )

# Calculate total minutes in bed
psqi_followup %<>%
  dplyr::mutate(
    TotalBedMinutes = lubridate::interval(BedTimeDT, WokenUpDT)/lubridate::minutes()
  )

#calculate comp 4
# Sleep efficiency
psqi_followup <- psqi_followup %>% 
  mutate(SleepEff = TotalSleepMinutes/TotalBedMinutes * 100)

# Convert to a 0-3 scale score.
psqi_followup <- psqi_followup %>%
  mutate(comp4 = case_when(
    SleepEff < 65 ~ 3, 
    SleepEff < 75 ~ 2,
    SleepEff < 85 ~ 1,
    SleepEff >= 85 ~ 0, 
  ))


#comp 5 
psqi_followup <- psqi_followup %>%
  mutate(TotalTrouble = rowSums(
    select(., WakeUpMiddleNight:OtherTroubleSleeping)))

# Categorise
psqi_followup %<>%
  dplyr::mutate(
    comp5 = dplyr::case_when(
      TotalTrouble == 0 ~ 0, 
      TotalTrouble <= 9 ~ 1,
      TotalTrouble <= 18 ~ 2,
      TotalTrouble <= 27 ~ 3
    )
  )

psqi_followup <- psqi_followup %>% 
  dplyr::select(-TotalTrouble)

#comp6
psqi_followup <- psqi_followup %>% 
  dplyr::rename(comp6 = SleepMedicines)

#comp7
psqi_followup <- psqi_followup %>% 
  dplyr::mutate(
    comp7 = dplyr::case_when(
      (StayingAwake + EnoughEnthusiasm) %in% c(0) ~ 0,
      (StayingAwake + EnoughEnthusiasm) %in% c(1, 2) ~ 1,
      (StayingAwake + EnoughEnthusiasm) %in% c(3, 4) ~ 2,
      (StayingAwake + EnoughEnthusiasm) %in% c(5, 6) ~ 3
    )
  )

#TotalComp
psqi_followup <- psqi_followup %>% 
  dplyr::mutate(TotalScore = comp1 + comp2 + comp3 + comp4 + comp5 + comp6 + comp7)

#score > 5 is "poor sleep"
psqi_followup <- psqi_followup %>%
  dplyr::mutate(SleepDisturbance = if_else(TotalScore <= 5, "No", "Yes"))

#tidy psqi_followup
psqi_followup <- psqi_followup %>% dplyr::select(-(7:35))

# SleepDisturbance as a factor

psqi_followup %<>%
  dplyr::mutate(
    SleepDisturbance = factor(SleepDisturbance, levels = c("No", "Yes"))
  )




# Select columns
psqi %<>%
  dplyr::select(
    ParticipantNo, SiteNo, SleepDisturbance
  )

psqi_followup %<>%
  dplyr::select(
    ParticipantNo, SleepDisturbance, Q_month
  )


# Month 0 at baseline
psqi %<>%
  dplyr::mutate(month = 0)

psqi_followup %<>%
  dplyr::rename(month = Q_month)

# Join together
psqi %<>%
  dplyr::bind_rows(
    psqi_followup
  )

# Fill NA SiteNo
psqi %<>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(SiteNo) %>%
  dplyr::ungroup()



# add age and sex data from demographics
psqi <- psqi %>%
  dplyr::inner_join(
    demographics %>% select(ParticipantNo, Sex, age, diagnosis),
    by = "ParticipantNo"
  )

# Recode diagnosis
psqi %<>%
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
psqi %<>% dplyr::filter(age > 17)

# make sex into factor
psqi %<>% dplyr::mutate(Sex = factor(
  Sex,
  levels = c(1, 2),
  labels = c("Male", "Female"))
)


psqi <- psqi %>%
  dplyr::left_join(
    IBD %>% dplyr::select(ParticipantNo, FlaresInPastYear)
  )

# create flare groups
psqi <- psqi %>%
  mutate(flare_group = ifelse(FlaresInPastYear == 0,
                              "No Flares",
                              "1 or More Flares"
  ))

# Make flares into levels
psqi %<>% 
  dplyr::mutate(flare_group = factor(
    flare_group,
    levels = c(
      "No Flares",
      "1 or More Flares"
    )
  ))

# FC data
psqi <- psqi %>% 
  dplyr::left_join(
    demo %>% dplyr::select(ParticipantNo, FC, cat), 
    by = "ParticipantNo")

# Add smoking
psqi %<>% 
  dplyr::left_join(
    smoking %>% dplyr::select(ParticipantNo, Smoke), 
    by = "ParticipantNo")

# Smoke as a factor
psqi %<>%
  dplyr::mutate(Smoke = forcats::as_factor(Smoke)) %>%
  dplyr::mutate(Smoke = forcats::fct_relevel(Smoke, 'Never', 'Previous', 'Current'))

# Add IMD
psqi %<>% 
  dplyr::left_join(
    IMD, 
    by = "ParticipantNo") 

# IMD as a factor
psqi %<>%
  dplyr::mutate(
    IMD = as.factor(IMD)
  ) 

# IBD control scores
psqi %<>%
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
psqi %<>% 
  dplyr::mutate(control_grouped = factor(control_grouped))

psqi %<>% 
  dplyr::mutate(vas_control = factor(vas_control))




# Age in decades
psqi %<>%
  dplyr::mutate(
    age_decade = age/10
  )


# FC maximum detectable value is 1250
psqi %<>%
  dplyr::mutate(
    FC = dplyr::case_when(
      FC > 1250 ~ 1250,
      .default = FC
    )
  )

# Log transform FC due to extreme positive skew
psqi %<>%
  dplyr::mutate(
    FC = log(FC)
  )

# Arrange
psqi %<>%
  dplyr::arrange(ParticipantNo, month)



# Save
filepath_save <- '/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Longitudinal analysis/'

readr::write_rds(
  x = psqi,
  file = glue::glue("{filepath_save}psqi_long.rds")
)


# Survival
data_soft_long <- psqi %>% 
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)

data_hard_long <- psqi %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = hardflare, time = hardflare_time)
