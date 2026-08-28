library(tidyverse)
library(magrittr)


# Load in the Predicct cohort
data_cohort <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/predicct/processed/demo-full.rds"
)

# Control scores
data_control <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/people/chiara/IBD_C.rds"
)

# Load participants
participants <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/participants.rds"
)

# Select relevant columns
data_cohort %<>%
  dplyr::select(
    ParticipantNo,
    Age,
    SiteNo,
    diagnosis,
    diagnosis2,
    FC,
    cat
  )

# Remove all patients < 18
# Else the age signal could be due to the exclusion of minors.
data_cohort %<>%
  dplyr::filter(Age >= 18)

# Flag if a patient is in the psychosocial cohort
data_cohort %<>%
  dplyr::mutate(
    psychosocial = dplyr::case_when(
      ParticipantNo %in% participants$ParticipantNo ~ 'Yes',
      .default = 'No'
    )
  )

# Control score
data_cohort %<>%
  dplyr::left_join(
    data_control %>%
      dplyr::select(ParticipantNo, OverallControl, vas_control, control_8, flare_group),
    by = "ParticipantNo"
  )


# Psychosocial
data_cohort %<>%
  dplyr::filter(psychosocial == "Yes") %>%
  dplyr::filter(!is.na(FC))


data_cohort %<>%
  dplyr::select(ParticipantNo, FC, cat, control_8, OverallControl, vas_control, flare_group)


data_cohort %>%
  ggplot(aes(x = OverallControl, colour = FC > 250)) +
  geom_density()

data_cohort %>%
  ggplot(aes(x = control_8, colour = FC > 250)) +
  geom_density()

data_cohort %>%
  dplyr::filter(!is.na(flare_group)) %>%
  dplyr::count(flare_group, FC > 250) %>%
  dplyr::group_by(flare_group) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  ggplot(aes(x = flare_group, y = p, group = `FC > 250`, fill = `FC > 250`)) +
  geom_col(position = 'dodge')

data_cohort %$%
  cor.test(FC, OverallControl, method = 'kendall')

data_cohort %$%
  cor.test(FC, control_8, method = 'kendall')

data_cohort %>%
  wilcox_test(FC ~ flare_group)

# If restricted to FC < 250
data_cohort %>%
  dplyr::filter(FC < 250) %$%
  cor.test(FC, OverallControl, method = 'kendall')

data_cohort %>%
  dplyr::filter(FC < 250) %$%
  cor.test(FC, control_8, method = 'kendall')

data_cohort %>%
  dplyr::filter(FC < 250) %>%
  wilcox_test(FC ~ flare_group)


# Discretised
data_cohort %>%
  dplyr::filter(complete.cases(cat, vas_control)) %>%
  dplyr::count(cat, vas_control) %>%
  dplyr::group_by(cat) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  dplyr::ungroup()

data_cohort %$%
  chisq.test(x = cat, y = vas_control)
