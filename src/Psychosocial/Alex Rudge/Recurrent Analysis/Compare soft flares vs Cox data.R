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
# review_missing_in_recurrent and monthly_rows_for_review below carry
# ParticipantNo and raw dates - kept in the environment for your own
# interactive inspection (View(), filter(), etc.), but deliberately NOT
# printed here. Don't paste their output elsewhere; only the aggregate
# counts in sections 3 and 6 are safe to share.
review_missing_in_recurrent <- comparison %>%
  dplyr::filter(cox_softflare, !recurrent_has_flare) %>%
  dplyr::arrange(ParticipantNo)

cat("review_missing_in_recurrent:", nrow(review_missing_in_recurrent), "participants (not printed - contains ParticipantNo)\n")

# Raw monthly questionnaire rows for those participants, to see why
# soft_flare_month/new_episode never fired for them.
monthly_rows_for_review <- monthly_soft %>%
  dplyr::filter(ParticipantNo %in% review_missing_in_recurrent$ParticipantNo) %>%
  dplyr::select(ParticipantNo, Q_month, DiseaseControlled, DiseaseWorsenedDate, ActualDate,
                soft_flare_month, new_episode)

# ---- 5. Reverse direction, for completeness --------------------------------------
review_missing_in_cox <- comparison %>%
  dplyr::filter(recurrent_has_flare, !cox_softflare) %>%
  dplyr::arrange(ParticipantNo)

cat("review_missing_in_cox:", nrow(review_missing_in_cox), "participants (not printed - contains ParticipantNo)\n")

# ---- 6. Attribute each missing-from-recurrent case to a candidate rule ----------
# soft_flare_month/new_episode guarantee at least one event fires for a
# participant whenever any of their raw flags survive the entry-date filter
# (the first surviving flagged month can never be "adjacent" to an earlier
# flagged one). So everyone in review_missing_in_recurrent must fall into one
# of a small number of buckets: their raw flags exist but are all pre-entry
# (rule 1 alone explains it); their only objective-flare candidate falls
# outside the 2-year imputation window (rules 4/6); they have no portal
# signal at all, so Cox's flare must come from something this build doesn't
# implement (the middle fallback tier, or the hard-flare-priority date
# override); or nothing here explains it, which needs a manual look.
# Buckets aren't proof of what actually happened on Cox's side - they just
# say which of *our* known gaps could plausibly account for each case.

soft_raw_by_participant <- monthly_soft %>%
  dplyr::filter(ParticipantNo %in% review_missing_in_recurrent$ParticipantNo) %>%
  dplyr::group_by(ParticipantNo) %>%
  dplyr::summarise(
    has_monthly_row = TRUE,
    any_soft_raw = any(soft_flare_raw, na.rm = TRUE),
    # soft_flare_month already has the entry-date filter applied - if this is
    # ever TRUE for someone in review_missing_in_recurrent, the reasoning
    # above is wrong somewhere and it's worth knowing.
    any_soft_raw_post_entry = any(soft_flare_month, na.rm = TRUE),
    .groups = "drop"
  )

eos_censored_by_participant <- dplyr::bind_rows(
    flare_dates_hard,
    furtherflares_linked %>% dplyr::select(ParticipantNo, flare_start_date = FlareStartDate)
  ) %>%
  dplyr::filter(ParticipantNo %in% review_missing_in_recurrent$ParticipantNo) %>%
  dplyr::left_join(demo_tbl %>% dplyr::select(ParticipantNo, entry_date), by = "ParticipantNo") %>%
  dplyr::group_by(ParticipantNo) %>%
  dplyr::summarise(
    has_objective_episode = TRUE,
    all_objective_censored = all(
      !is.na(entry_date) & as.numeric(flare_start_date - as.Date(entry_date)) > 730.5
    ),
    .groups = "drop"
  )

# review_missing_in_recurrent_reasons (with ParticipantNo) stays in the
# environment for local inspection only - never printed. The count() below,
# which has no ParticipantNo/date column, is the piece that's safe to share.
review_missing_in_recurrent_reasons <- review_missing_in_recurrent %>%
  dplyr::left_join(soft_raw_by_participant, by = "ParticipantNo") %>%
  dplyr::left_join(
    event_counts_hard %>% dplyr::transmute(ParticipantNo, had_hard_flare = hardflare == 1),
    by = "ParticipantNo"
  ) %>%
  dplyr::left_join(eos_censored_by_participant, by = "ParticipantNo") %>%
  dplyr::mutate(
    dplyr::across(
      c(has_monthly_row, any_soft_raw, any_soft_raw_post_entry, has_objective_episode, all_objective_censored),
      ~ tidyr::replace_na(.x, FALSE)
    ),
    candidate_rule = dplyr::case_when(
      any_soft_raw_post_entry ~ "UNEXPECTED: soft_flare_month is TRUE but n_events is 0 - check new_episode logic",
      any_soft_raw ~ "pre-entry drop (rule 1) fully explains it - all raw flags predate entry_date",
      all_objective_censored ~ "objective flare present but >2yr from entry - excluded from imputation (rules 4/6)",
      # A hard flare only survives to here (rather than being imputed, or
      # caught by all_objective_censored above) if its entry_date was missing
      # and the 2yr-censoring check couldn't evaluate - flag that explicitly
      # rather than letting it fall into the generic "no monthly rows" bucket.
      !has_monthly_row & had_hard_flare ~ "no monthly rows, but HAS a hard flare with unresolved timing - likely a missing entry_date broke the 2yr imputation check",
      !has_monthly_row ~ "no monthly rows and no hard flare - participant absent from every data source this build uses",
      !had_hard_flare ~ "no portal soft-flare signal and no hard flare - candidate: Cox's unimplemented middle fallback tier (outpatient/admission/IBD call/surgery date)",
      had_hard_flare ~ "no portal soft-flare signal, but has a hard flare - candidate: Cox's hard-flare-priority date override",
      TRUE ~ "unexplained by known rule gaps - needs manual review"
    )
  ) %>%
  dplyr::select(ParticipantNo, cox_softflare, softflare_time, recurrent_n_events,
                any_soft_raw, has_objective_episode, all_objective_censored,
                had_hard_flare, candidate_rule)

cat("\nCandidate-rule breakdown for participants Cox flags but recurrent misses (safe to share - no IDs or dates):\n")
review_missing_in_recurrent_reasons %>% dplyr::count(candidate_rule, sort = TRUE) %>% print(n = 100)
