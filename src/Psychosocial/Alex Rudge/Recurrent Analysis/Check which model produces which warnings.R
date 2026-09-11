# Determines definitively whether each warning type seen in the rendered
# HTML (checkConv/optwrap-bobyqa/"nearly unidentifiable"/boundary-singular/
# theta.ml/optTheta) comes from the Poisson fit, the NB fit, or both - by
# fitting each separately, wrapped in its own withCallingHandlers(), tagging
# every warning to its source model. Runs one representative cell (HADS
# anxiety, Full Cohort, soft flare) under both the primary formula and the
# sensitivity formula (adds flare_group + OverallControl + control_8), so
# you can also see whether those extra covariates are what's driving the
# sensitivity file's much heavier warning load.
#
# No privacy concern here - this is model diagnostics only, no patient-level
# output, so full warning text is printed.

setwd("/Users/vvelasco/Documents/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Recurrent Analysis")

library(tidyverse)

source("poisson_helpers.R")  # -> event_counts_soft/hard, covariates, glmer_control,
                              # population_cohort, demo_tbl, episodes_soft_portal,
                              # qflare_episodes, imputed_soft

covariates_dx <- covariates %>%
  dplyr::left_join(population_cohort %>% dplyr::select(ParticipantNo, diagnosis2), by = "ParticipantNo")

sensitivity_covariates <- population_cohort %>%
  dplyr::select(ParticipantNo) %>%
  dplyr::left_join(
    read.xlsx(paste0(data.path, "Baseline2022/IBD.xlsx")) %>%
      dplyr::transmute(
        ParticipantNo,
        flare_group = factor(
          dplyr::if_else(FlaresInPastYear == 0, "No Flares", "1 or More Flares"),
          levels = c("No Flares", "1 or More Flares")
        )
      ),
    by = "ParticipantNo"
  ) %>%
  dplyr::left_join(
    readRDS(paste0(chiara, "IBD_C.RDS")) %>%
      dplyr::select(ParticipantNo, OverallControl, control_8),
    by = "ParticipantNo"
  )
covariates_dx <- covariates_dx %>% dplyr::left_join(sensitivity_covariates, by = "ParticipantNo")

soft_episode_dates <- dplyr::bind_rows(
  episodes_soft_portal %>% dplyr::select(ParticipantNo, episode_date),
  qflare_episodes,
  imputed_soft %>% dplyr::transmute(ParticipantNo, episode_date = flare_start_date)
)
entry_dates <- demo_tbl %>% dplyr::select(ParticipantNo, entry_date)

build_person_period <- function(cohort, episode_dates, exposure_long, entry_dates, cutoff_days = 730.5) {
  exposure_locf <- exposure_long %>%
    dplyr::semi_join(cohort, by = "ParticipantNo") %>%
    dplyr::distinct(ParticipantNo, month, .keep_all = TRUE) %>%
    dplyr::arrange(ParticipantNo, month) %>%
    dplyr::group_by(ParticipantNo) %>%
    tidyr::fill(score_group, .direction = "down") %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(score_group)) %>%
    dplyr::mutate(interval_start = month * 365 / 12) %>%
    dplyr::filter(interval_start < cutoff_days)

  person_period <- exposure_locf %>%
    dplyr::group_by(ParticipantNo) %>%
    dplyr::arrange(interval_start, .by_group = TRUE) %>%
    dplyr::mutate(interval_stop = dplyr::lead(interval_start, default = cutoff_days)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(entry_dates, by = "ParticipantNo") %>%
    dplyr::mutate(t = interval_stop - interval_start)

  events_per_interval <- person_period %>%
    dplyr::select(ParticipantNo, interval_start, interval_stop, entry_date) %>%
    dplyr::left_join(episode_dates, by = "ParticipantNo") %>%
    dplyr::mutate(
      event_day = as.numeric(as.Date(episode_date) - as.Date(entry_date)),
      in_interval = !is.na(event_day) & event_day >= interval_start & event_day < interval_stop
    ) %>%
    dplyr::group_by(ParticipantNo, interval_start, interval_stop) %>%
    dplyr::summarise(Y = sum(in_interval), .groups = "drop")

  person_period %>%
    dplyr::left_join(events_per_interval, by = c("ParticipantNo", "interval_start", "interval_stop")) %>%
    dplyr::mutate(Y = tidyr::replace_na(Y, 0))
}

hads_items <- c(
  "FeelTense", "FrightenedFeelingSomethingAwful", "WorryingThoughts", "SitAtEase",
  "FrightenedFeelingButterflies", "FeelRestless", "SuddenFeelingsPanic",
  "EnjoyThings", "CanLaugh", "FeelCheerful", "FeelSlowedDown",
  "LostInterestAppearance", "LookForward", "CanEnjoyBookTV"
)
inverse_coded_items <- c(
  "FeelTense", "FrightenedFeelingSomethingAwful", "WorryingThoughts", "FeelCheerful",
  "FeelSlowedDown", "LostInterestAppearance", "FeelRestless", "SuddenFeelingsPanic"
)
score_hads <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(hads_items), ~ . - 1)) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(inverse_coded_items), ~ 3 - .)) %>%
    dplyr::mutate(
      anxiety_hads = FeelTense + FrightenedFeelingSomethingAwful + WorryingThoughts +
        SitAtEase + FrightenedFeelingButterflies + FeelRestless + SuddenFeelingsPanic,
      depression_hads = EnjoyThings + CanLaugh + FeelCheerful + FeelSlowedDown +
        LostInterestAppearance + LookForward + CanEnjoyBookTV
    )
}
hads_baseline_raw <- readxl::read_xlsx(paste0(data.path, "Baseline2022/hads.xlsx")) %>% dplyr::filter(!is.na(ParticipantId))
hads_followup_raw <- readxl::read_xlsx(paste0(data.path, "Followup/hads.xlsx")) %>% dplyr::filter(!is.na(ParticipantId))
hads_long <- dplyr::bind_rows(
  score_hads(hads_baseline_raw) %>% dplyr::transmute(ParticipantNo, month = 0, anxiety_hads, depression_hads),
  score_hads(hads_followup_raw) %>% dplyr::transmute(ParticipantNo, month = Q_month, anxiety_hads, depression_hads)
)
anxiety_long <- hads_long %>%
  dplyr::transmute(
    ParticipantNo, month,
    score_group = cut(anxiety_hads, breaks = c(0, 7, 21), labels = c("0-7", "8-21"), include.lowest = TRUE)
  )

pp_anxiety_soft <- build_person_period(population_cohort, soft_episode_dates, anxiety_long, entry_dates) %>%
  dplyr::left_join(covariates_dx, by = "ParticipantNo")

f_primary <- Y ~ score_group + Sex + age_decade + Smoke + IMD + FC +
  (1 | ParticipantNo) + (1 | SiteNo) + offset(log(t))
f_sensitivity <- Y ~ score_group + Sex + age_decade + Smoke + IMD + FC +
  flare_group + OverallControl + control_8 +
  (1 | ParticipantNo) + (1 | SiteNo) + offset(log(t))

fit_and_tag_warnings <- function(formula, data, family_type) {
  warnings_caught <- character(0)
  fit <- withCallingHandlers(
    {
      if (family_type == "poisson") {
        lme4::glmer(formula, data = data, family = poisson, control = glmer_control)
      } else {
        lme4::glmer.nb(formula, data = data, control = glmer_control)
      }
    },
    warning = function(w) {
      warnings_caught <<- c(warnings_caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(fit = fit, warnings = warnings_caught)
}

run_and_report <- function(label, formula, data) {
  cat("\n====", label, "(n =", nrow(data), ") ====\n")
  p <- fit_and_tag_warnings(formula, data, "poisson")
  nb <- fit_and_tag_warnings(formula, data, "nb")

  cat("POISSON warnings (", length(p$warnings), "):\n", sep = "")
  if (length(p$warnings) > 0) cat(paste(" -", p$warnings), sep = "\n") else cat(" (none)\n")

  cat("NB warnings (", length(nb$warnings), "):\n", sep = "")
  if (length(nb$warnings) > 0) cat(paste(" -", nb$warnings), sep = "\n") else cat(" (none)\n")
}

run_and_report("Primary formula - HADS anxiety, Full Cohort, soft flare", f_primary, pp_anxiety_soft)
run_and_report("Sensitivity formula - HADS anxiety, Full Cohort, soft flare", f_sensitivity, pp_anxiety_soft)
