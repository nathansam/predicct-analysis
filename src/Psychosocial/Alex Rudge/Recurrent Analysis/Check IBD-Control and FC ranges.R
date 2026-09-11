# Confirms the actual units/range of OverallControl, control_8, and FC in
# this cohort's data, rather than trusting the published instrument's
# nominal scoring convention (VAS 0-100, IBD-Control-8 0-16) unverified.
#
# Output policy: aggregate distribution stats of a validated clinical score
# and a lab value - not remotely identifying, safe to share in full.

library(tidyverse)
library(readxl)

source("build_event_counts.R")  # -> demo_tbl, chiara path

ibd_c <- readRDS(paste0(chiara, "IBD_C.RDS"))

cat("---- OverallControl (IBD-Control-VAS) ----\n")
print(summary(ibd_c$OverallControl))
cat("n non-missing:", sum(!is.na(ibd_c$OverallControl)), "of", nrow(ibd_c), "\n\n")

cat("---- control_8 (IBD-Control-8) ----\n")
print(summary(ibd_c$control_8))
cat("n non-missing:", sum(!is.na(ibd_c$control_8)), "of", nrow(ibd_c), "\n\n")

cat("---- FC (fecal calprotectin, raw, from demo_tbl) ----\n")
print(summary(demo_tbl$FC))
cat("n non-missing:", sum(!is.na(demo_tbl$FC)), "of", nrow(demo_tbl), "\n")
