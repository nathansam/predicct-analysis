# Profiles every column in all-flares.xlsx (not just softflare/softflare_time)
# for participants Cox flags with a soft flare that this analysis's recurrent
# build (build_event_counts.R) doesn't capture (review_missing_in_recurrent,
# from "Compare soft flares vs Cox data.R") - looking for anything in the
# wider Cox export (a flare-source/type flag, other timing fields, etc.) that
# explains where their flare signal actually comes from, since the
# monthly-questionnaire build can't see it. Compares against the profile of
# all Cox-softflare participants in the cohort, so what's distinctive about
# the missing group stands out.
#
# Output policy: no ParticipantNo, no raw dates, ever.
#   - identifier-looking columns (name matches an ID pattern) are dropped
#     before any profiling happens
#   - numeric columns get distribution summaries (mean/median/IQR), never
#     per-row values
#   - Date/POSIXct columns get presence/absence counts only - no actual date
#     values, not even min/max
#   - character/factor/logical columns get value-count tables, but any column
#     with too many distinct values relative to the group size is skipped
#     entirely, since a table of near-singleton categories is effectively a
#     per-participant list

library(tidyverse)
library(readxl)

source("Compare soft flares vs Cox data.R")  # -> review_missing_in_recurrent, population_cohort

# ---- 1. Full all-flares.xlsx, every column this time ---------------------------
if (file.exists("/.dockerenv")) {
  all_flare_path <- "data/final/20240308/Followup/"
} else {
  all_flare_path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20240308/Followup/"
}

cox_flares_full <- readxl::read_xlsx(
  paste0(all_flare_path, "all-flares.xlsx"),
  na = ".", sheet = 1
) %>%
  dplyr::mutate(ParticipantNo = as.character(ParticipantNo))

cat("---- Columns available in all-flares.xlsx (names/types only) ----\n")
tibble::tibble(
  column = names(cox_flares_full),
  type = purrr::map_chr(cox_flares_full, ~ class(.x)[1])
) %>% print(n = 200)

# ---- 2. Restrict to the missing-from-recurrent participants, drop identifiers --
id_like <- "(?i)participant|patient|^id$|_id$|nhs|record|^no$"

missing_rows <- cox_flares_full %>%
  dplyr::filter(ParticipantNo %in% review_missing_in_recurrent$ParticipantNo) %>%
  dplyr::select(-dplyr::matches(id_like))

baseline_rows <- cox_flares_full %>%
  dplyr::filter(ParticipantNo %in% population_cohort$ParticipantNo, softflare == 1) %>%
  dplyr::select(-dplyr::matches(id_like))

cat("\nProfiling", nrow(missing_rows), "missing-from-recurrent rows against",
    nrow(baseline_rows), "all Cox-softflare rows in cohort, across",
    ncol(missing_rows), "non-identifier columns.\n\n")

# ---- 3. Column-by-column profile, split by type --------------------------------
MAX_CATEGORIES <- 15          # skip character/factor columns with more distinct values than this
MIN_GROUP_FOR_SUMMARY <- 5    # refuse to summarise a group this small at all

profile_group <- function(df, group_label) {
  numeric_summary <- purrr::imap_dfr(df, function(col, name) {
    if (is.numeric(col) && !inherits(col, "Date")) {
      tibble::tibble(
        group = group_label, column = name, type = "numeric",
        n = sum(!is.na(col)), n_missing = sum(is.na(col)),
        mean = round(mean(col, na.rm = TRUE), 1),
        median = round(stats::median(col, na.rm = TRUE), 1),
        q25 = round(stats::quantile(col, 0.25, na.rm = TRUE), 1),
        q75 = round(stats::quantile(col, 0.75, na.rm = TRUE), 1)
      )
    } else {
      tibble::tibble()
    }
  })

  date_summary <- purrr::imap_dfr(df, function(col, name) {
    if (inherits(col, "Date") || inherits(col, "POSIXct")) {
      tibble::tibble(group = group_label, column = name, type = "date",
                      n_present = sum(!is.na(col)), n_missing = sum(is.na(col)))
    } else {
      tibble::tibble()
    }
  })

  categorical_summary <- purrr::imap_dfr(df, function(col, name) {
    is_cat <- is.character(col) || is.factor(col) || is.logical(col)
    if (!is_cat) return(tibble::tibble())
    n_distinct <- dplyr::n_distinct(col, na.rm = TRUE)
    if (n_distinct == 0 || n_distinct > MAX_CATEGORIES || n_distinct >= nrow(df) * 0.5) {
      return(tibble::tibble(group = group_label, column = name,
                             value = "<skipped: too many/too few distinct values to summarise safely>",
                             n = NA_integer_))
    }
    tibble::tibble(value = as.character(col)) %>%
      dplyr::count(value, name = "n") %>%
      dplyr::mutate(group = group_label, column = name, .before = 1)
  })

  list(numeric = numeric_summary, date = date_summary, categorical = categorical_summary)
}

if (nrow(missing_rows) >= MIN_GROUP_FOR_SUMMARY && nrow(baseline_rows) >= MIN_GROUP_FOR_SUMMARY) {
  missing_profile <- profile_group(missing_rows, "missing_from_recurrent")
  baseline_profile <- profile_group(baseline_rows, "all_cox_softflare_in_cohort")

  cat("\n---- Numeric / duration columns (mean, median, IQR - no raw values) ----\n")
  dplyr::bind_rows(missing_profile$numeric, baseline_profile$numeric) %>%
    dplyr::arrange(column, group) %>% print(n = 200)

  cat("\n---- Date/POSIXct columns (presence only - no actual dates) ----\n")
  dplyr::bind_rows(missing_profile$date, baseline_profile$date) %>%
    dplyr::arrange(column, group) %>% print(n = 200)

  cat("\n---- Categorical columns (value counts) ----\n")
  categorical_combined <- dplyr::bind_rows(missing_profile$categorical, baseline_profile$categorical)
  if (nrow(categorical_combined) > 0) {
    categorical_combined %>%
      dplyr::arrange(column, group, dplyr::desc(n)) %>% print(n = 500)
  } else {
    cat("(none of the non-identifier columns are character/factor/logical type)\n")
  }
} else {
  cat("One of the two groups is smaller than", MIN_GROUP_FOR_SUMMARY,
      "- refusing to summarise, as aggregate stats on a tiny group risk re-identification.\n")
}
