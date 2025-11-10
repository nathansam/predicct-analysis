library(tidyverse)
library(magrittr)



# Looking at medication data
# Mainly interested in Q25 - do you sometimes forget to take your pills?
# Q26 also maybe interesting - miss medication for other reason?

data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"

medications <- readxl::read_xlsx(
  paste0(data.path, "Baseline2022/medications.xlsx")
)

# Select relevant columns
medications %<>%
  dplyr::select(ParticipantNo, SiteNo, ForgetPills:TakeMedicineYesterday, DifficultyRemembering)

# Factor recode
medications %<>%
  dplyr::mutate(ForgetPills = forcats::as_factor(ForgetPills)) %>%
  dplyr::mutate(ForgetPills = forcats::fct_recode(ForgetPills, Yes = '1', No = '2'))


# Counts
# Forgot pills
medications %>%
  dplyr::count(ForgetPills)
# 1 is yes, 2 is no

# Missed for other reason
medications %>%
  dplyr::count(MissMedication)

# How often do you have difficult remembering to take all your medicine
medications %>%
  dplyr::count(DifficultyRemembering)
# Seems to be a better version of ForgetPills


# But, medication and pills are different no?
# Lets see how many people do forget to take medication but not their pills
medications %>%
  dplyr::group_by(ForgetPills) %>%
  dplyr::count(DifficultyRemembering)
# So some people don't forget to take their pills 
# but sometimes forget to take their medication and vice versa

