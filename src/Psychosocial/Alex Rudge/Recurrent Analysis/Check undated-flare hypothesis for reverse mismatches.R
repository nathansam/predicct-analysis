# Checks whether the "recurrent says flare, Cox doesn't" mismatches
# (review_missing_in_cox, from "Compare soft flares vs Cox data.R") line up
# with monthly-questionnaire rows where a flare is indicated
# (DiseaseControlled == "No", or the implicit-No rule) but NEITHER
# DiseaseWorsenedDate NOR ActualDate is given, so episode_date is NA.
#
# Note on mechanism: per build_event_counts.R's own logic, an NA episode_date
# makes soft_flare_month/new_episode NA rather than TRUE, and
# episodes_soft_portal's dplyr::filter(new_episode) drops NA rows - so on a
# direct read, an undated flare more likely SUPPRESSES a count than
# manufactures an extra one. This script checks the hypothesis empirically
# rather than relying on that reading, since NA can also bleed into the
# *next* month's new_episode via the lag()-based adjacency check.
#
# Output policy: no ParticipantNo, no raw dates - only aggregate counts.

library(tidyverse)
library(readxl)

source("Compare soft flares vs Cox data.R")  # -> review_missing_in_cox, monthly_soft

undated_flare_rows <- monthly_soft %>%
  dplyr::mutate(
    undated_flare_signal = soft_flare_raw & is.na(DiseaseWorsenedDate) & is.na(ActualDate)
  )

cat("---- Cohort-wide ----\n")
cat("Monthly rows with a flare indicated but no date at all:",
    sum(undated_flare_rows$undated_flare_signal, na.rm = TRUE), "\n")
cat("Distinct participants with at least one such row:",
    undated_flare_rows %>% dplyr::filter(undated_flare_signal) %>% dplyr::distinct(ParticipantNo) %>% nrow(),
    "\n\n")

cat("---- Among the 3 reverse mismatches (review_missing_in_cox) ----\n")
mismatch_rows <- undated_flare_rows %>%
  dplyr::filter(ParticipantNo %in% review_missing_in_cox$ParticipantNo)

n_with_undated_signal <- mismatch_rows %>%
  dplyr::filter(undated_flare_signal) %>%
  dplyr::distinct(ParticipantNo) %>%
  nrow()

cat("Participants in review_missing_in_cox:", nrow(review_missing_in_cox), "\n")
cat("Of those, participants with >=1 undated flare-indication row:", n_with_undated_signal, "\n")
cat("(If this is 3 of 3, the hypothesis is strongly supported; if 0, it's ruled out;\n")
cat(" anything in between means it's a partial explanation.)\n")
