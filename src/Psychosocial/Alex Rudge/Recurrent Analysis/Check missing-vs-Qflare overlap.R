# Checks the overlap between two participant sets:
#   - "missing in Poisson": Cox's all-flares.xlsx says softflare == 1, but
#     this analysis's recurrent build (event_counts_soft, from
#     build_event_counts.R) has n_events == 0 for them
#   - "Qflare == 1": Cox's own questionnaire-derived flare flag
#
# Output policy: no ParticipantNo, no raw dates - only aggregate counts.

library(tidyverse)
library(readxl)

source("build_event_counts.R")  # -> event_counts_soft, population_cohort

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
    cox_softflare = softflare == 1,
    cox_hardflare = hardflare == 1,
    Qflare = Qflare == 1
  )

status <- population_cohort %>%
  dplyr::select(ParticipantNo) %>%
  dplyr::left_join(
    event_counts_soft %>% dplyr::transmute(ParticipantNo, n_events_poisson = n_events),
    by = "ParticipantNo"
  ) %>%
  dplyr::left_join(cox_flares, by = "ParticipantNo") %>%
  dplyr::mutate(
    n_events_poisson = tidyr::replace_na(n_events_poisson, 0L),
    poisson_has_flare = n_events_poisson >= 1,
    cox_softflare = tidyr::replace_na(cox_softflare, FALSE),
    cox_hardflare = tidyr::replace_na(cox_hardflare, FALSE),
    Qflare = tidyr::replace_na(Qflare, FALSE),
    missing_in_poisson = cox_softflare & !poisson_has_flare
  )

n_missing_in_poisson <- sum(status$missing_in_poisson)
n_qflare <- sum(status$Qflare)
n_overlap <- sum(status$missing_in_poisson & status$Qflare)

cat("Patients missing in the Poisson build (Cox soft flare, Poisson has none):", n_missing_in_poisson, "\n")
cat("Patients with Qflare == 1:", n_qflare, "\n")
cat("Overlap (missing in Poisson AND Qflare == 1):", n_overlap, "\n")
cat("Missing in Poisson but NOT Qflare == 1 (needs a different explanation):", n_missing_in_poisson - n_overlap, "\n")
cat("Qflare == 1 but NOT missing in Poisson (already captured by the current build):", n_qflare - n_overlap, "\n")

# ---- Reference numbers to compare against -----------------------------------
# Cox is a time-to-first-event (survival) build - it only ever has ONE flare
# per participant, so "number of events" there just means "number of
# flaring patients". The Poisson/NB build tracks every episode per patient,
# so its total event count is a different (and generally larger) number -
# that's the one that actually feeds the Poisson/NB model.
cat("\n---- Patient-level counts (how many patients had >=1 flare) ----\n")
cat("Cox soft flare patients:", sum(status$cox_softflare), "(paper: 638)\n")
cat("Cox hard flare patients:", sum(status$cox_hardflare), "(paper: 230)\n")
cat("Cox Qflare patients:    ", sum(status$Qflare), "\n")
cat("Poisson soft flare patients (n_events >= 1):", sum(status$poisson_has_flare), "\n")

cat("\n---- Total event/episode counts (what the Poisson/NB model actually sees) ----\n")
cat("Cox soft flare events (== patient count, first-flare-only):", sum(status$cox_softflare), "\n")
cat("Poisson soft flare total episodes (sum of n_events, includes recurrences):",
    sum(status$n_events_poisson), "\n")
cat("Poisson episodes among flaring patients only, mean per patient:",
    round(sum(status$n_events_poisson) / sum(status$poisson_has_flare), 2), "\n")
