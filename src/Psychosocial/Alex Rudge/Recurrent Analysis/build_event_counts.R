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
# (DiseaseWorsenedDate), else questionnaire date (ActualDate) - the
# reference definition's middle fallback tier (earliest of outpatient
# appointment/hospital admission/IBD team call/surgery date) is NOT
# implemented here, as those fields' names in monthly.xlsx are unknown.
# A flare dated before study entry is dropped (reference step 1); flares
# used for soft-flare imputation (4b) are censored at 2 years from entry
# (reference steps 4/6). Post-withdrawal questionnaires are NOT filtered out
# (reference step 3) - the withdrawal-date field's source is unknown.
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
  # Twenty rows have FlareStartDate but no FlareEndDate (we impute FlareStartDate)
  dplyr::mutate(
    FlareEndDate = case_when(
      FlareEndDate == "." ~ FlareStartDate,
      .default = FlareEndDate
    )
  ) %>%
  # Three rows have neither FlareStartDate nor FlareEndDate (we filter those out)
  dplyr::filter(FlareEndDate != ".") %>%
  filter(QuestionnaireId != ".") %>%
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
  dplyr::left_join(demo_tbl %>% dplyr::select(ParticipantNo, entry_date), by = "ParticipantNo") %>%
  dplyr::arrange(ParticipantNo, Q_month) %>%
  dplyr::group_by(ParticipantNo) %>%
  dplyr::mutate(
    # DiseaseControlled == "No" is the explicit answer; a missing answer with
    # a worsening date still given is treated as an implicit "No" too, per
    # Survival/Soft-hard-flares.qmd's reference definition.
    soft_flare_raw = dplyr::coalesce(DiseaseControlled == "No", FALSE) |
      (is.na(DiseaseControlled) & !is.na(DiseaseWorsenedDate)),
    # as.Date(), not POSIXct - mixing the two in a later subtraction silently
    # returns epoch-seconds minus epoch-days instead of erroring.
    worsened_date_only = as.Date(DiseaseWorsenedDate),
    episode_date = as.Date(dplyr::if_else(!is.na(DiseaseWorsenedDate), DiseaseWorsenedDate, ActualDate)),
    # Reference definition step 1 (Survival/Soft-hard-flares.qmd): a flare
    # dated before study entry is a data artifact, not a real in-study flare -
    # drop it (equivalent to recoding disease as controlled) rather than count it.
    soft_flare_month = soft_flare_raw & (is.na(entry_date) | episode_date >= as.Date(entry_date)),
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
    new_episode = soft_flare_month & !prev_adjacent_flare
  ) %>%
  dplyr::ungroup()

episodes_soft_portal <- monthly_soft %>%
  dplyr::filter(new_episode) %>%
  dplyr::select(ParticipantNo, Q_month, episode_date)
# > sum(monthly_soft %>% dplyr::group_by(ParticipantNo) %>%
#     dplyr::summarise(f = any(new_episode)) %>% dplyr::pull(f))
# [1] 471 # portal route only, before adding the Qflare/hard-flare episodes below

# ---- 4b. Soft flares Cox's own questionnaire route finds that this build's
# from-scratch monthlyQ.xlsx reconstruction above doesn't (Qflare) -----------
#
# 64 participants have Qflare == 1 in all-flares.xlsx (same 20240308 pull
# used for the Cox analysis) with no corresponding episode above - confirmed
# (see "Compare soft flares vs Cox data.R" / "Check missing-vs-Qflare
# overlap.R") to fully explain that gap, not partially. Qflare appears to
# implement Constantine-Cooke's complete reference definition, including the
# "middle fallback tier" (earliest of outpatient appointment/hospital
# admission/IBD team call/surgery date) this build has no field names for
# (see the file header), and/or draws on a newer monthlyQ.xlsx pull than the
# 20221004 one used above. Rather than guess at those fields, Qflare/
# Qflare_time is treated as ground truth for whether/when that flare
# happened, and folded in as one more episode candidate alongside the portal
# route above and the hard-flare route below - not a replacement for either,
# since a participant can still have further episodes only those other
# routes catch.
if (file.exists("/.dockerenv")) {
  all_flare_path <- "data/final/20240308/Followup/"
} else {
  all_flare_path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20240308/Followup/"
}

qflare_candidates <- readxl::read_xlsx(
  paste0(all_flare_path, "all-flares.xlsx"),
  na = ".", sheet = 1
) %>%
  dplyr::transmute(ParticipantNo = as.character(ParticipantNo), Qflare = Qflare == 1, Qflare_time) %>%
  dplyr::filter(Qflare) %>%
  dplyr::left_join(demo_tbl %>% dplyr::select(ParticipantNo, entry_date), by = "ParticipantNo") %>%
  dplyr::transmute(ParticipantNo, episode_date = as.Date(entry_date) + Qflare_time) %>%
  dplyr::semi_join(population_cohort, by = "ParticipantNo")

# Same +/-30 day tolerance as the hard-flare imputation below: a portal
# report and Cox's own questionnaire-route date won't line up exactly even
# when they're the same underlying flare.
qflare_episodes <- qflare_candidates %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    has_portal_match = any(
      episodes_soft_portal$ParticipantNo == ParticipantNo &
        abs(as.numeric(episodes_soft_portal$episode_date - episode_date)) <= 30
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!has_portal_match) %>%
  dplyr::select(ParticipantNo, episode_date)

# All accepted soft-flare episodes so far (portal + Qflare) - the hard-flare
# imputation below checks against this combined set, not portal alone, so a
# hard flare that's really the same event as a Qflare-derived one isn't
# double counted.
soft_episodes_accepted <- dplyr::bind_rows(
  episodes_soft_portal %>% dplyr::select(ParticipantNo, episode_date),
  qflare_episodes
)

# ---- 4c. Soft flares imputed from route-2 objective flares -------------------
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
) %>%
  dplyr::left_join(demo_tbl %>% dplyr::select(ParticipantNo, entry_date), by = "ParticipantNo") %>%
  # Reference definition steps 4/6 (Survival/Soft-hard-flares.qmd): censor at
  # 2 years (730.5 days) from entry - same cutoff constant used there. Only
  # the portal route is naturally bounded by Q_month (<=24); objective flares
  # used for imputation have no such bound, so this needs to be explicit.
  dplyr::filter(is.na(entry_date) | as.numeric(flare_start_date - as.Date(entry_date)) <= 730.5) %>%
  dplyr::select(-entry_date)

# +/- 30 days: a portal/Qflare report and the clinician-recorded date won't
# line up exactly even for the same flare.
imputed_soft <- eos_objective_episodes %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    has_portal_match = any(
      soft_episodes_accepted$ParticipantNo == ParticipantNo &
        abs(as.numeric(soft_episodes_accepted$episode_date - flare_start_date)) <= 30
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!has_portal_match)

# ---- 4d. Final soft-flare episode counts, all three routes combined ---------
n_soft_flares <- dplyr::bind_rows(
  soft_episodes_accepted,
  imputed_soft %>% dplyr::transmute(ParticipantNo, episode_date = flare_start_date)
) %>%
  dplyr::count(ParticipantNo, name = "n_events")

event_counts_soft <- population_cohort %>%
  dplyr::left_join(n_soft_flares, by = "ParticipantNo") %>%
  dplyr::mutate(n_events = tidyr::replace_na(n_events, 0))
# Re-run "Compare soft flares vs Cox data.R" after this change to confirm
# sum(event_counts_soft$n_events >= 1) now lands at/near the paper's 638.

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
