# Traces exactly which step of build_event_counts.R's portal-route logic
# (Section 4: monthly_soft's soft_flare_raw -> soft_flare_month ->
# month_adjacent -> new_worsening_date -> prev_adjacent_flare -> new_episode
# chain) fired for the 3 reverse mismatches (review_missing_in_cox). Already
# confirmed (via "Diagnose reverse mismatches.R"): all 3 come from the
# portal route, not Qflare or hard-flare imputation - so this is unrelated
# to, and predates, the Qflare fix added this session.
#
# Output policy: no ParticipantNo, no raw dates - only aggregate counts
# across the 3 (same standard as every other script this session), even
# though the group is small.

library(tidyverse)
library(readxl)

source("Diagnose reverse mismatches.R")  # -> review_missing_in_cox, monthly_soft

trigger_rows <- monthly_soft %>%
  dplyr::filter(ParticipantNo %in% review_missing_in_cox$ParticipantNo, new_episode) %>%
  dplyr::mutate(
    trigger_mechanism = dplyr::case_when(
      DiseaseControlled == "No" ~ "explicit: DiseaseControlled == 'No'",
      is.na(DiseaseControlled) & !is.na(DiseaseWorsenedDate) ~ "implicit: DiseaseControlled missing, worsening date given",
      TRUE ~ "UNEXPECTED - matches neither soft_flare_raw condition, worth flagging"
    )
  )

# Per-case breakdown, one row per participant - safe to show individually
# since no ParticipantNo, Q_month, or date column is included, so a row
# can't be tied back to who's who, only "case 1 vs case 2 vs case 3".
# Row order carries no meaning (not sorted by anything identifying).
cat("---- Per-case breakdown: which criterion fired for each of the 3 ----\n")
trigger_rows %>%
  dplyr::select(trigger_mechanism, month_adjacent, new_worsening_date, prev_adjacent_flare, soft_flare_month, new_episode) %>%
  dplyr::mutate(case = dplyr::row_number(), .before = 1) %>%
  print(n = 10, width = Inf)

cat("\n---- Same info, aggregated (for a quick summary alongside the per-case view) ----\n")
cat("Trigger mechanism:\n")
trigger_rows %>% dplyr::count(trigger_mechanism) %>% print(n = 10)
cat("\nContinuation-of-adjacent-month status:\n")
trigger_rows %>% dplyr::count(month_adjacent, prev_adjacent_flare) %>% print(n = 10)
cat("\nEntry-date filter pass-through:\n")
trigger_rows %>% dplyr::count(soft_flare_month) %>% print(n = 10)

cat("\n---- Summary ----\n")
cat("All 3 originate from build_event_counts.R Section 4 (the original monthly.xlsx\n")
cat("portal-route reconstruction), at the line `new_episode <- soft_flare_month &\n")
cat("!prev_adjacent_flare`. Not Qflare, not hard-flare imputation - this logic is\n")
cat("unchanged by this session's Qflare fix and predates it.\n")
