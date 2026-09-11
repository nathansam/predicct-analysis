# Checks whether using all-flares.xlsx's Qflare/Qflare_time as the
# first-flare-ever signal (instead of re-deriving it from monthlyQ.xlsx)
# would close the gap found in "Compare soft flares vs Cox data.R" /
# "Profile Cox flares missing from recurrent.R" - all 64 of that gap's
# participants have Qflare == 1 in Cox's data despite this build's own
# monthly-questionnaire reconstruction not finding a flare for them.
#
# This only tests the hypothesis - it does NOT modify build_event_counts.R.
#
# Approach: first flare = Qflare_time days after entry_date, whenever
# Qflare == 1. Subsequent flares still come from monthly_soft's own
# new_episode detection, but a detected episode within 30 days of the
# Qflare-derived first-flare date is treated as the same flare, not double
# counted - same +/-30 day tolerance already used for hard-flare imputation
# matching in build_event_counts.R.
#
# Output policy: no ParticipantNo, no raw dates - only aggregate counts.

library(tidyverse)
library(readxl)

source("build_event_counts.R")  # -> monthly_soft, population_cohort, demo_tbl, event_counts_soft (v1)

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
    ParticipantNo = as.character(ParticipantNo),
    Qflare = Qflare == 1,
    Qflare_time
  )

# ---- 1. Build the Qflare-derived first-flare date -------------------------------
first_flare <- population_cohort %>%
  dplyr::select(ParticipantNo) %>%
  dplyr::left_join(cox_flares, by = "ParticipantNo") %>%
  dplyr::left_join(demo_tbl %>% dplyr::select(ParticipantNo, entry_date), by = "ParticipantNo") %>%
  dplyr::mutate(
    Qflare = tidyr::replace_na(Qflare, FALSE),
    first_flare_date = dplyr::if_else(Qflare, as.Date(entry_date) + Qflare_time, as.Date(NA))
  )

# ---- 2. Recurrent episodes from the monthly build, excluding a +/-30 day match
# against the Qflare-derived first-flare date (same flare, not a new one) --------
recurrent_episodes <- monthly_soft %>%
  dplyr::filter(new_episode) %>%
  dplyr::select(ParticipantNo, episode_date) %>%
  dplyr::left_join(first_flare %>% dplyr::select(ParticipantNo, first_flare_date), by = "ParticipantNo") %>%
  dplyr::mutate(
    matches_first_flare = !is.na(first_flare_date) & abs(as.numeric(episode_date - first_flare_date)) <= 30
  )

n_recurrent_after_first <- recurrent_episodes %>%
  dplyr::filter(!matches_first_flare) %>%
  dplyr::count(ParticipantNo, name = "n_after_first")

# ---- 3. New event counts: 1 (if Qflare) + non-duplicate monthly episodes -------
event_counts_soft_v2 <- first_flare %>%
  dplyr::left_join(n_recurrent_after_first, by = "ParticipantNo") %>%
  dplyr::mutate(
    n_after_first = tidyr::replace_na(n_after_first, 0),
    n_events_v2 = dplyr::if_else(Qflare, 1L, 0L) + n_after_first
  )

# ---- 4. Compare against the current build (v1) and against Cox's Qflare --------
comparison_v2 <- event_counts_soft_v2 %>%
  dplyr::left_join(
    event_counts_soft %>% dplyr::transmute(ParticipantNo, n_events_v1 = n_events),
    by = "ParticipantNo"
  ) %>%
  dplyr::mutate(
    has_flare_v1 = n_events_v1 >= 1,
    has_flare_v2 = n_events_v2 >= 1
  )

cat("v1 (current build) - participants with >=1 soft flare:", sum(comparison_v2$has_flare_v1), "\n")
cat("v2 (Qflare-based first flare) - participants with >=1 soft flare:", sum(comparison_v2$has_flare_v2), "\n")
cat("Cox Qflare==1 total (should roughly match v2, modulo any remaining edge cases):",
    sum(comparison_v2$Qflare), "\n\n")

cat("Agreement with Cox's Qflare flag:\n")
cat("  v1 agrees with Cox Qflare:", sum(comparison_v2$has_flare_v1 == comparison_v2$Qflare), "of", nrow(comparison_v2), "\n")
cat("  v2 agrees with Cox Qflare:", sum(comparison_v2$has_flare_v2 == comparison_v2$Qflare), "of", nrow(comparison_v2), "\n\n")

cat("Remaining mismatches under v2 (Cox Qflare says flare, v2 build doesn't):",
    sum(comparison_v2$Qflare & !comparison_v2$has_flare_v2), "\n")
cat("Remaining mismatches under v2 (v2 build says flare, Cox Qflare doesn't):",
    sum(comparison_v2$has_flare_v2 & !comparison_v2$Qflare), "\n\n")

cat("Distribution of n_events per participant, v1 vs v2 (aggregate counts only):\n")
dplyr::bind_rows(
  comparison_v2 %>% dplyr::count(n_events = n_events_v1) %>% dplyr::mutate(version = "v1"),
  comparison_v2 %>% dplyr::count(n_events = n_events_v2) %>% dplyr::mutate(version = "v2")
) %>%
  tidyr::pivot_wider(names_from = version, values_from = n, values_fill = 0) %>%
  dplyr::arrange(n_events) %>%
  print(n = 50)
