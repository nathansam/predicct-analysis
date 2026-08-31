# Cross-checks participants flagged with a patient-reported (soft) flare in
# the Cox/survival dataset (all-flares.xlsx, built for
# Survival/Soft-hard-flares.qmd - Nathan Constantine-Cooke's reference
# first-flare-ever definition) against this analysis's recurrent-event
# dataset (event_counts_soft, from build_event_counts.R). Produces a
# per-participant table for manual review of where the two disagree.
#
# Note: not every disagreement is necessarily a bug here. The Cox dataset
# applies its own rules our recurrent build doesn't replicate (pre-entry
# flares deleted, post-withdrawal questionnaires removed, hard-flare-priority
# date override) - some of these could legitimately explain why Cox says a
# participant flared but our data doesn't, or vice versa. This script is for
# spotting candidates, not for concluding the discrepancy is an error.

library(tidyverse)
library(readxl)

# ---- 1. This analysis's recurrent-event dataset -------------------------------
source("build_event_counts.R")  # -> event_counts_soft, monthly_soft, population_cohort

recurrent_flare <- event_counts_soft %>%
  dplyr::transmute(ParticipantNo, recurrent_n_events = n_events, recurrent_has_flare = n_events >= 1)

# ---- 2. Cox/survival dataset (all-flares.xlsx) ---------------------------------
# Same docker/OS path pattern as build_event_counts.R, but the 20240308 pull
# used for the Cox analysis, not the 20221004 one used above.
if (file.exists("/.dockerenv")) {
  all_flare_path <- "data/final/20240308/Followup/"
} else {
  all_flare_path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20240308/Followup/"
}

cox_flares <- readxl::read_xlsx(
  paste0(all_flare_path, "all-flares.xlsx"),
  na = ".", sheet = 1
) %>%
  dplyr::transmute(
    # population_cohort$ParticipantNo is character, not numeric - match that,
    # not the other way round, since population_cohort is the trusted source.
    ParticipantNo = as.character(ParticipantNo),
    cox_softflare = softflare == 1,
    softflare_time
  )

# Sanity check before joining: a type-safe join can still silently produce
# zero matches if the string formats don't actually agree (e.g. "0123" vs
# "123"). Confirm real overlap exists first.
n_overlap <- length(intersect(recurrent_flare$ParticipantNo, cox_flares$ParticipantNo))
cat("ParticipantNo overlap between the two datasets:", n_overlap, "of", nrow(recurrent_flare), "cohort participants\n")
if (n_overlap < nrow(recurrent_flare) * 0.9) {
  warning("Overlap looks too low - check ParticipantNo formatting (leading zeros, whitespace) before trusting the comparison below.")
}

# ---- 3. Compare -----------------------------------------------------------------
# population_cohort (participants.rds) is the trusted analysis population -
# monthly.xlsx/all-flares.xlsx cover a wider set of respondents who didn't
# meet this paper's inclusion criteria, so restrict comparisons to the cohort
# rather than flagging those extra participants as discrepancies.
comparison <- recurrent_flare %>%
  dplyr::full_join(cox_flares, by = "ParticipantNo") %>%
  dplyr::mutate(
    recurrent_has_flare = tidyr::replace_na(recurrent_has_flare, FALSE),
    cox_softflare = tidyr::replace_na(cox_softflare, FALSE),
    in_population_cohort = ParticipantNo %in% population_cohort$ParticipantNo
  ) %>%
  dplyr::filter(in_population_cohort)

cat("Cox softflare total, within cohort:", sum(comparison$cox_softflare), "(compare against the paper's 638)\n")
cat("Cox says flare, recurrent doesn't:", sum(comparison$cox_softflare & !comparison$recurrent_has_flare), "\n")
cat("Recurrent says flare, Cox doesn't:", sum(comparison$recurrent_has_flare & !comparison$cox_softflare), "\n")
cat("Agree (either both flare or both no flare):", sum(comparison$cox_softflare == comparison$recurrent_has_flare), "\n")

# ---- 4. Main list for review: Cox flare, missing from our recurrent data --------
review_missing_in_recurrent <- comparison %>%
  dplyr::filter(cox_softflare, !recurrent_has_flare) %>%
  dplyr::arrange(ParticipantNo)

review_missing_in_recurrent

# Raw monthly questionnaire rows for those participants, to see why
# soft_flare_month/new_episode never fired for them.
monthly_rows_for_review <- monthly_soft %>%
  dplyr::filter(ParticipantNo %in% review_missing_in_recurrent$ParticipantNo) %>%
  dplyr::select(ParticipantNo, Q_month, DiseaseControlled, DiseaseWorsenedDate, ActualDate,
                soft_flare_month, new_episode)

monthly_rows_for_review

# ---- 5. Reverse direction, for completeness --------------------------------------
review_missing_in_cox <- comparison %>%
  dplyr::filter(recurrent_has_flare, !cox_softflare) %>%
  dplyr::arrange(ParticipantNo)

review_missing_in_cox
