# Lists column NAMES (not data) from the files build_event_counts.R already
# loads, filtered to anything that might indicate study withdrawal/exit -
# looking for the field build_event_counts.R's header flags as unknown
# ("the withdrawal-date field's source is unknown"). If one of these turns
# out to be it, we can test the reverse-mismatch hypothesis properly instead
# of relying on the "flare in last submitted month" timing proxy, which
# isn't safe to generalise (see "Diagnose reverse mismatches.R").
#
# Output policy: column names only - no data, no ParticipantNo, no dates.

library(tidyverse)
library(readxl)

source("build_event_counts.R")  # -> monthly, demo_tbl, population_cohort

withdrawal_pattern <- "(?i)withdraw|status|dropout|discontinu|complete|studyend|end.?date|exit|active"

cat("---- Candidate columns in `monthly` (monthlyQ.xlsx) ----\n")
hits_monthly <- grep(withdrawal_pattern, names(monthly), value = TRUE, perl = TRUE)
if (length(hits_monthly) > 0) print(hits_monthly) else cat("(none found)\n")

cat("\n---- Candidate columns in `demo_tbl` (demo.RDS) ----\n")
hits_demo <- grep(withdrawal_pattern, names(demo_tbl), value = TRUE, perl = TRUE)
if (length(hits_demo) > 0) print(hits_demo) else cat("(none found)\n")

cat("\n---- Candidate columns in `population_cohort` (participants.rds) ----\n")
hits_pop <- grep(withdrawal_pattern, names(population_cohort), value = TRUE, perl = TRUE)
if (length(hits_pop) > 0) print(hits_pop) else cat("(none found)\n")

cat("\n---- For reference, ALL column names in each (still just names) ----\n")
cat("monthly:\n"); print(names(monthly))
cat("\ndemo_tbl:\n"); print(names(demo_tbl))
cat("\npopulation_cohort:\n"); print(names(population_cohort))
