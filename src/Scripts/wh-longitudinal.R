
library(datefixR)
later.fcal <- read_xlsx("/Volumes/igmm/cvallejo-predicct/predicct/end-of-follow-up/EOF_fcal.xlsx") %>%
  as.data.frame() %>%
  filter(ParticipantNo %in% demo$ParticipantNo)

later.fcal <- merge(x = later.fcal,
      y = demo %>% select(ParticipantNo, meno2),
      all.x = TRUE,
      all.y = FALSE,
      by = "ParticipantNo") %>%
  filter(FCALDate != ".")

later.fcal <- fix_date_df(later.fcal, "FCALDate", excel = TRUE) %>%
  filter(FCALDate != "3033-03-17") %>%
  filter(FCALDate != "1900-01-03")

later.fcal %>%
  ggplot(
    aes(
      x = FCALDate,
      y = as.numeric(FCALLevel),
      group = ParticipantNo,
      color = meno2)) +
  facet_grid(rows = vars(meno2)) +
  geom_line(alpha = 0.25) +
  theme_minimal()
