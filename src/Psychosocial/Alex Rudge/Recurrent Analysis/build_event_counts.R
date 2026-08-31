# Builds the recurrent-event outcome tables (event_counts_soft,
# event_counts_hard) and their adjustment covariates. Sourced by both
# "Events per individual.R" (the standalone outcome-distribution/dispersion
# demo) and "poisson_helpers.R" (used by each exposure's Poisson analysis) -
# kept separate so exposure files don't re-run the demo's own diagnosis2-only
# models every time they source the helper.
#
# ---- Endpoint definitions (PREdiCCt protocol, section 2.2) ------------------
#
# Patient-reported flare: monthly questionnaire, "No" to "has your disease
# been well controlled in the past month?" (DiseaseControlled) - or, per
# Constantine-Cooke's reference definition (Survival/Soft-hard-flares.qmd),
# DiseaseControlled missing but a worsening date was still given, which is
# treated as an implicit "No". Flare date = reported worsening date
# (DiseaseWorsenedDate), else questionnaire date (ActualDate).
#
# Objective flare: CRP >= 5 mg/L and/or FC >= 250 ug/g plus new/escalated IBD
# therapy, ascertained via (1) a portal flare triggering confirmatory stool
# testing, or (2) end-of-study clinician phenotyping. A route-2 flare with no
# matching portal report within 24 months gets an imputed patient-reported
# flare (section 4b). Objective flares (section 3) = first flare
# (flares_hard.RDS, matches published n=230) + subsequent flares
# (EOF_furtherflares.xlsx), only for participants with a confirmed first.

library(tidyverse)
library(magrittr)
library(readxl)
library(openxlsx)

# ---- Paths --------------------------------------------------------------------

if (file.exists("/.dockerenv")) {
  data.path <- "data/final/20221004/"
  prefix <- "data/end-of-follow-up/"
  outdir <- "data/processed"
} else {
  data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"
  prefix <- "/Volumes/igmm/cvallejo-predicct/predicct/end-of-follow-up/"
  outdir <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
  chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"
  alex <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"
}

# ---- 1. Population cohort ----------------------------------------------------
#
# 1855 participants - zero-fills both event count tables so non-flaring
# participants count as 0 rather than being dropped.
population_cohort <- readRDS(paste0(alex, "participants.rds"))

# ---- 2. Monthly follow-up questionnaire (portal route) -----------------------
monthly <- readxl::read_xlsx(paste0(data.path, "Followup/monthlyQ.xlsx"))

# ---- 3. Objective (hard) flares -----------------------------------------------

furtherflares <- readxl::read_xlsx(paste0(prefix, "EOF_furtherflares.xlsx")) %>%
  dplyr::mutate(
    QuestionnaireId = as.numeric(QuestionnaireId),
    FlareStartDate = as.Date(as.numeric(FlareStartDate), origin = "1899-12-30"),
    FlareEndDate = as.Date(as.numeric(FlareEndDate), origin = "1899-12-30")
  )

dates_eof <- readxl::read_xlsx(paste0(prefix, "EOF_dates.xlsx"))  # QuestionnaireId -> ParticipantId
demo_tbl <- readRDS(paste0(outdir, "demo.RDS"))  # also has entry_date (4b), FC (section 6)

# ParticipantId -> ParticipantNo: no single source covers everyone, so
# combine demo_tbl, the raw REDCap demographics export, and monthly, then
# restrict to the population cohort (these sources cover the wider recruited
# cohort, not just the 1855 we actually need).
id_map <- dplyr::bind_rows(
  demo_tbl %>% dplyr::transmute(ParticipantId = as.numeric(ParticipantId), ParticipantNo),
  read.xlsx(paste0(data.path, "Baseline2022/demographics2022.xlsx")) %>%
    dplyr::transmute(ParticipantId = as.numeric(ParticipantId), ParticipantNo),
  monthly %>% dplyr::transmute(ParticipantId = as.numeric(ParticipantId), ParticipantNo)
) %>%
  dplyr::distinct(ParticipantId, ParticipantNo) %>%
  dplyr::semi_join(population_cohort, by = "ParticipantNo")

furtherflares_linked <- furtherflares %>%
  dplyr::inner_join(dates_eof %>% dplyr::select(QuestionnaireId, ParticipantId), by = "QuestionnaireId") %>%
  dplyr::inner_join(id_map, by = "ParticipantId")

n_further_flares <- furtherflares_linked %>%
  dplyr::count(ParticipantNo, name = "n_further")

flares_hard <- readRDS(paste0(chiara, "flares_hard.RDS")) %>%
  dplyr::right_join(population_cohort %>% dplyr::select(ParticipantNo), by = "ParticipantNo")

event_counts_hard <- population_cohort %>%
  dplyr::left_join(flares_hard %>% dplyr::select(ParticipantNo, hardflare), by = "ParticipantNo") %>%
  dplyr::left_join(n_further_flares, by = "ParticipantNo") %>%
  dplyr::mutate(
    hardflare = tidyr::replace_na(hardflare, 0),
    n_further = dplyr::if_else(hardflare == 1, tidyr::replace_na(n_further, 0), 0),
    n_events = hardflare + n_further
  )
# > sum(event_counts_hard$n_events >= 1)
# [1] 230 (matches paper)

# ---- 4. Patient-reported (soft) flares - portal route -------------------------
#
# Consecutive flagged months are one ongoing flare, not several - a row
# starts a new episode only if the preceding month wasn't also flagged.
monthly$DiseaseControlled <- ifelse(
  is.na(monthly$DiseaseControlled), NA,
  ifelse(monthly$DiseaseControlled == 1, "Yes", "No")
)

monthly_soft <- monthly %>%
  dplyr::arrange(ParticipantNo, Q_month) %>%
  dplyr::group_by(ParticipantNo) %>%
  dplyr::mutate(
    # DiseaseControlled == "No" is the explicit answer; a missing answer with
    # a worsening date still given is treated as an implicit "No" too, per
    # Survival/Soft-hard-flares.qmd's reference definition.
    soft_flare_month = dplyr::coalesce(DiseaseControlled == "No", FALSE) |
      (is.na(DiseaseControlled) & !is.na(DiseaseWorsenedDate)),
    # as.Date() here too - same POSIXct/Date mixing hazard as episode_date below.
    worsened_date_only = as.Date(DiseaseWorsenedDate),
    month_adjacent = dplyr::lag(soft_flare_month, default = FALSE) &
      (Q_month - dplyr::lag(Q_month) == 1),
    # A distinct reported worsening date on an adjacent flagged month means the
    # participant is describing a new flare, not the same one continuing - split
    # the run even though the months are consecutive. NA on either side can't be
    # compared, so it leaves the month-adjacency call unchanged.
    new_worsening_date = month_adjacent &
      !is.na(worsened_date_only) & !is.na(dplyr::lag(worsened_date_only)) &
      worsened_date_only != dplyr::lag(worsened_date_only),
    prev_adjacent_flare = month_adjacent & !new_worsening_date,
    new_episode = soft_flare_month & !prev_adjacent_flare,
    # as.Date(), not POSIXct - mixing the two in a later subtraction silently
    # returns epoch-seconds minus epoch-days instead of erroring.
    episode_date = as.Date(dplyr::if_else(!is.na(DiseaseWorsenedDate), DiseaseWorsenedDate, ActualDate))
  ) %>%
  dplyr::ungroup()

episodes_soft_portal <- monthly_soft %>%
  dplyr::filter(new_episode) %>%
  dplyr::select(ParticipantNo, Q_month, episode_date)

n_soft_flares <- monthly_soft %>%
  dplyr::group_by(ParticipantNo) %>%
  dplyr::summarise(n_events = sum(new_episode, na.rm = TRUE), .groups = "drop")

event_counts_soft <- population_cohort %>%
  dplyr::left_join(n_soft_flares, by = "ParticipantNo") %>%
  dplyr::mutate(n_events = tidyr::replace_na(n_events, 0))
# > sum(event_counts_soft$n_events >= 1)
# [1] 471 # This is before imputing hard flare -> soft flare

# ---- 4b. Soft flares imputed from route-2 objective flares ------------------
#
# hardflare_time is a duration (days from entry), not a date - convert via
# demo_tbl$entry_date before comparing to FlareStartDate.
flare_dates_hard <- flares_hard %>%
  dplyr::filter(hardflare == 1) %>%
  dplyr::left_join(demo_tbl %>% dplyr::select(ParticipantNo, entry_date), by = "ParticipantNo") %>%
  dplyr::transmute(ParticipantNo, flare_start_date = as.Date(entry_date) + hardflare_time)

eos_objective_episodes <- dplyr::bind_rows(
  flare_dates_hard,
  furtherflares_linked %>% dplyr::select(ParticipantNo, flare_start_date = FlareStartDate)
)

# +/- 30 days: a portal report and the clinician-recorded date won't line up
# exactly even for the same flare.
imputed_soft <- eos_objective_episodes %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    has_portal_match = any(
      episodes_soft_portal$ParticipantNo == ParticipantNo &
        abs(as.numeric(episodes_soft_portal$episode_date - flare_start_date)) <= 30
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!has_portal_match)

n_imputed_soft <- imputed_soft %>%
  dplyr::count(ParticipantNo, name = "n_imputed")

event_counts_soft <- event_counts_soft %>%
  dplyr::left_join(n_imputed_soft, by = "ParticipantNo") %>%
  dplyr::mutate(
    n_imputed = tidyr::replace_na(n_imputed, 0),
    n_events = n_events + n_imputed
  )
# > sum(event_counts_soft$n_events >= 1)
# [1] 590 # A few dozen short of what is reported in the paper

# ---- 5. Adjustment covariates ---------------------------------------------------
#
# Coded exactly as Primary Analysis (e.g. HADS.qmd). SiteNo isn't in
# demographics2022.xlsx/demo.RDS - Primary Analysis carries it through from
# hads.xlsx, so it's loaded from there too; swap sources if that doesn't
# cover the full population cohort.
covariates <- population_cohort %>%
  dplyr::select(ParticipantNo) %>%  # diagnosis2 already in event_counts_soft/hard - don't rejoin it, it'll collide
  dplyr::left_join(
    read.xlsx(paste0(data.path, "Baseline2022/demographics2022.xlsx")) %>%
      dplyr::transmute(ParticipantNo, Sex = factor(Sex, levels = c(1, 2), labels = c("Male", "Female")), age_decade = age / 10),
    by = "ParticipantNo"
  ) %>%
  dplyr::left_join(
    readRDS(paste0(chiara, "smoking.rds")) %>%
      dplyr::transmute(ParticipantNo, Smoke = forcats::fct_relevel(forcats::as_factor(Smoke), "Never", "Previous", "Current")),
    by = "ParticipantNo"
  ) %>%
  dplyr::left_join(readRDS(paste0(chiara, "IMD.rds")) %>% dplyr::transmute(ParticipantNo, IMD = as.factor(IMD)), by = "ParticipantNo") %>%
  dplyr::left_join(
    demo_tbl %>% dplyr::transmute(ParticipantNo, FC = log(dplyr::case_when(FC > 1250 ~ 1250, .default = FC))),
    by = "ParticipantNo"
  ) %>%
  dplyr::left_join(
    read.xlsx(paste0(data.path, "Baseline2022/hads.xlsx")) %>% dplyr::transmute(ParticipantNo, SiteNo = as.factor(SiteNo)),
    by = "ParticipantNo"
  )

event_counts_soft <- event_counts_soft %>% dplyr::left_join(covariates, by = "ParticipantNo")
event_counts_hard <- event_counts_hard %>% dplyr::left_join(covariates, by = "ParticipantNo")
