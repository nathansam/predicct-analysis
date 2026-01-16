library(tidyverse)


variables = c('score_group',
              'IMD',
              'Sex',
              'age_decade',
              'FC',
              'Smoke',
              'SiteNo')


# Anxiety
# Soft 
# UC
data_survival_anxiety_soft_uc %>%
  dplyr::select(tidyselect::all_of(c(
    'DiseaseFlareYN', 'time', variables)
  )) %>%
  dplyr::summarise(
    n = n(),
    cc = sum(complete.cases(.)),
    missing = n - cc,
    missing_percent = 100*missing/n
  )

# 11.7% missing

# CD
data_survival_anxiety_soft_cd %>%
  dplyr::select(tidyselect::all_of(c(
    'DiseaseFlareYN', 'time', variables)
  )) %>%
  dplyr::summarise(
    n = n(),
    cc = sum(complete.cases(.)),
    missing = n - cc,
    missing_percent = 100*missing/n
  )

# 11.0% missing

# Hard
# UC
data_survival_anxiety_hard_uc %>%
  dplyr::select(tidyselect::all_of(c(
    'DiseaseFlareYN', 'time', variables)
  )) %>%
  dplyr::summarise(
    n = n(),
    cc = sum(complete.cases(.)),
    missing = n - cc,
    missing_percent = 100*missing/n
  )

# 11.4% missing

# CD
data_survival_anxiety_hard_cd %>%
  dplyr::select(tidyselect::all_of(c(
    'DiseaseFlareYN', 'time', variables)
  )) %>%
  dplyr::summarise(
    n = n(),
    cc = sum(complete.cases(.)),
    missing = n - cc,
    missing_percent = 100*missing/n
  )

# 10.7% missing

# Depression
# Soft 
# UC
data_survival_depression_soft_uc %>%
  dplyr::select(tidyselect::all_of(c(
    'DiseaseFlareYN', 'time', variables)
  )) %>%
  dplyr::summarise(
    n = n(),
    cc = sum(complete.cases(.)),
    missing = n - cc,
    missing_percent = 100*missing/n
  )

# 11.7% missing

# CD
data_survival_depression_soft_cd %>%
  dplyr::select(tidyselect::all_of(c(
    'DiseaseFlareYN', 'time', variables)
  )) %>%
  dplyr::summarise(
    n = n(),
    cc = sum(complete.cases(.)),
    missing = n - cc,
    missing_percent = 100*missing/n
  )

# 10.9% missing

# Hard
# UC
data_survival_depression_hard_uc %>%
  dplyr::select(tidyselect::all_of(c(
    'DiseaseFlareYN', 'time', variables)
  )) %>%
  dplyr::summarise(
    n = n(),
    cc = sum(complete.cases(.)),
    missing = n - cc,
    missing_percent = 100*missing/n
  )

# 11.5% missing

# CD
data_survival_depression_hard_cd %>%
  dplyr::select(tidyselect::all_of(c(
    'DiseaseFlareYN', 'time', variables)
  )) %>%
  dplyr::summarise(
    n = n(),
    cc = sum(complete.cases(.)),
    missing = n - cc,
    missing_percent = 100*missing/n
  )

# 10.6% missing
