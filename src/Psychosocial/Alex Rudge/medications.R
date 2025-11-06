
# Looking at medication data
# Mainly interested in Q25 - do you sometimes forget to take your pills?
# Q26 also maybe interesting - miss medication for other reason?


data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"

data_meds <- readxl::read_xlsx(
  paste0(data.path, "Baseline2022/medications.xlsx")
)

# Select relevant columns
data_meds %<>%
  dplyr::select(ParticipantNo, ForgetPills:TakeMedicineYesterday)


# Counts
# Forgot pills
data_meds %>%
  dplyr::count(ForgetPills)
# 1 is yes, 2 is no

# Missed for other reason
data_meds %>%
  dplyr::count(MissMedication)
