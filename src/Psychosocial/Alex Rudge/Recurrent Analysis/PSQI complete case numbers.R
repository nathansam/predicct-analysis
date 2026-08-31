library(tidyverse)


variables = c('SleepDisturbance',
              'IMD',
              'Sex',
              'age_decade',
              'FC',
              'Smoke',
              'SiteNo')


# Anxiety
# Soft 
# UC
data_survival_soft_uc %>%
  dplyr::select(tidyselect::all_of(c(
    'DiseaseFlareYN', 'time', variables)
  )) %>%
  dplyr::summarise(
    n = n(),
    cc = sum(complete.cases(.)),
    missing = n - cc,
    missing_percent = 100*missing/n
  )

# 11.8% missing

# CD
data_survival_soft_cd %>%
  dplyr::select(tidyselect::all_of(c(
    'DiseaseFlareYN', 'time', variables)
  )) %>%
  dplyr::summarise(
    n = n(),
    cc = sum(complete.cases(.)),
    missing = n - cc,
    missing_percent = 100*missing/n
  )

# 11.2% missing

# Hard
# UC
data_survival_hard_uc %>%
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
data_survival_hard_cd %>%
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