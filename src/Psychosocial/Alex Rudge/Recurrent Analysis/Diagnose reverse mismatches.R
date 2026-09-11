# Diagnoses the 3 "recurrent says flare, Cox doesn't" reverse mismatches
# (review_missing_in_cox, from "Compare soft flares vs Cox data.R"). The
# undated-flare hypothesis has already been ruled out (zero such rows exist
# cohort-wide) - this checks several other candidate explanations:
#
#   1. Are they even present in all-flares.xlsx at all (ParticipantNo join
#      match), or could this be a silent ID-format mismatch like the ~1
#      participant already found missing from the overall overlap check?
#      If so, "Cox says no flare" would be wrong - Cox just isn't matched.
#   2. Which route produced their recurrent-build episode: portal
#      (episodes_soft_portal), Qflare (should be impossible here - Qflare
#      implies Cox's own data agrees a flare happened), or hard-flare
#      imputation (imputed_soft)?
#   3. For portal-sourced episodes: how late in the participant's own
#      monthly follow-up did it land? 0 months before their last submission
#      is a candidate for the build's known gap - post-withdrawal
#      questionnaires aren't filtered out (reference step 3).
#   4. For hard-flare-imputed episodes: how close to the 730.5-day
#      imputation censoring cutoff is it (candidate for a boundary/rounding
#      disagreement with Cox's own build)?
#
# Output policy: no ParticipantNo, no raw dates - only aggregate counts.

library(tidyverse)
library(readxl)

source("Compare soft flares vs Cox data.R")  # -> review_missing_in_cox, monthly_soft, episodes_soft_portal,
                                              # qflare_episodes, imputed_soft, demo_tbl, event_counts_soft

# ---- 1. Are they even present in the Cox extract at all? -----------------------
if (file.exists("/.dockerenv")) {
  all_flare_path <- "data/final/20240308/Followup/"
} else {
  all_flare_path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20240308/Followup/"
}
cox_flares_full <- readxl::read_xlsx(paste0(all_flare_path, "all-flares.xlsx"), na = ".", sheet = 1) %>%
  dplyr::mutate(ParticipantNo = as.character(ParticipantNo))

n_present_in_cox_extract <- sum(review_missing_in_cox$ParticipantNo %in% cox_flares_full$ParticipantNo)
cat("---- 1. Presence in the Cox extract ----\n")
cat("Of the 3 reverse mismatches, present at all in all-flares.xlsx (any row):",
    n_present_in_cox_extract, "of", nrow(review_missing_in_cox), "\n")
cat("(If < 3, that participant may be a silent ParticipantNo format mismatch,\n")
cat(" not a genuine 'Cox says no flare' case.)\n\n")

# ---- 2. Which route produced their recurrent-build episode? --------------------
source_summary <- review_missing_in_cox %>%
  dplyr::select(ParticipantNo) %>%
  dplyr::mutate(
    from_portal = ParticipantNo %in% episodes_soft_portal$ParticipantNo,
    from_qflare = ParticipantNo %in% qflare_episodes$ParticipantNo,
    from_hard_imputed = ParticipantNo %in% imputed_soft$ParticipantNo
  )

cat("---- 2. Episode source (not mutually exclusive) ----\n")
cat("From portal (monthly questionnaire):", sum(source_summary$from_portal), "\n")
cat("From Qflare (expect 0 - implies Cox would agree):", sum(source_summary$from_qflare), "\n")
cat("From hard-flare imputation:", sum(source_summary$from_hard_imputed), "\n\n")

# ---- 3. Portal-sourced: how late relative to their own last submitted month? ---
portal_timing <- monthly_soft %>%
  dplyr::filter(ParticipantNo %in% review_missing_in_cox$ParticipantNo) %>%
  dplyr::group_by(ParticipantNo) %>%
  dplyr::summarise(
    last_q_month = max(Q_month, na.rm = TRUE),
    flare_q_month = suppressWarnings(max(dplyr::if_else(new_episode, Q_month, NA_real_), na.rm = TRUE)),
    months_from_last = last_q_month - flare_q_month,
    .groups = "drop"
  ) %>%
  dplyr::filter(is.finite(flare_q_month))

cat("---- 3. Timing of portal-sourced episodes vs. each participant's last submitted month ----\n")
cat("(0 = flare reported in their very last questionnaire - candidate for the\n")
cat(" known gap: post-withdrawal questionnaires aren't filtered out.)\n")
if (nrow(portal_timing) > 0) {
  portal_timing %>% dplyr::count(months_from_last) %>% print(n = 50)
} else {
  cat("(none of the 3 have a portal-sourced episode)\n")
}
cat("\n")

# ---- 4. Hard-flare-imputed: how close to the 730.5-day censoring cutoff? -------
hard_imputed_timing <- imputed_soft %>%
  dplyr::filter(ParticipantNo %in% review_missing_in_cox$ParticipantNo) %>%
  dplyr::left_join(demo_tbl %>% dplyr::select(ParticipantNo, entry_date), by = "ParticipantNo") %>%
  dplyr::mutate(
    days_from_entry = as.numeric(flare_start_date - as.Date(entry_date)),
    days_before_cutoff = 730.5 - days_from_entry
  )

cat("---- 4. Timing of hard-flare-imputed episodes vs. the 730.5-day cutoff ----\n")
if (nrow(hard_imputed_timing) > 0) {
  cat("Days from entry: min =", round(min(hard_imputed_timing$days_from_entry), 1),
      ", max =", round(max(hard_imputed_timing$days_from_entry), 1), "\n")
  cat("Days remaining before the cutoff: min =", round(min(hard_imputed_timing$days_before_cutoff), 1),
      ", max =", round(max(hard_imputed_timing$days_before_cutoff), 1), "\n")
} else {
  cat("(none of the 3 have a hard-flare-imputed episode)\n")
}
cat("\n")

# ---- 5. Total episode count per participant -------------------------------------
cat("---- 5. n_events distribution across the 3 ----\n")
event_counts_soft %>%
  dplyr::filter(ParticipantNo %in% review_missing_in_cox$ParticipantNo) %>%
  dplyr::count(n_events) %>%
  print()
