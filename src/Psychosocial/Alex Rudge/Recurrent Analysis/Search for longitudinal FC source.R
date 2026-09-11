# Broader search for a repeated/longitudinal FC (fecal calprotectin) source
# than monthlyQ.xlsx or demo.RDS, which were the only two checked before
# (see the appendix in earlier versions of "Disease activity over time and
# outcomes.qmd") and turned up nothing beyond a single static value.
#
# Step 1: list every FILE in the directories this build already knows about
# (names only, no data) - to see what other files exist that haven't been
# inspected yet.
# Step 2: for any file whose name suggests FC/calprotectin/bloods/labs,
# read just its column names and check for FC-related fields.
#
# Output policy: filenames and column names only - no data values, no
# ParticipantNo, no dates.

library(tidyverse)
library(readxl)
library(openxlsx)

base_dirs <- c(
  data_path_baseline = "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/Baseline2022/",
  data_path_followup = "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/Followup/",
  data_path_root = "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/",
  outdir = "/Volumes/igmm/cvallejo-predicct/predicct/processed/",
  chiara = "/Volumes/igmm/cvallejo-predicct/people/chiara/",
  flare_dir = "/Volumes/igmm/cvallejo-predicct/predicct/final/20240308/Followup/",
  prefix_eof = "/Volumes/igmm/cvallejo-predicct/predicct/end-of-follow-up/",
  redcap_path = "/Volumes/igmm/cvallejo-predicct/predicct/final/20231030/",
  alex = "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"
)

cat("---- Step 1: files in each known base directory ----\n")
for (nm in names(base_dirs)) {
  path <- base_dirs[[nm]]
  cat("\n", nm, "(", path, "):\n", sep = "")
  if (dir.exists(path)) {
    print(list.files(path, recursive = FALSE))
  } else {
    cat("  (directory does not exist or is not accessible)\n")
  }
}

cat("\n\n---- Step 2: any filenames suggesting FC/calprotectin/bloods/labs ----\n")
fc_filename_pattern <- "(?i)calprotectin|fecal|^fc|bloods?|labs?|stool"
candidate_files <- character(0)
for (nm in names(base_dirs)) {
  path <- base_dirs[[nm]]
  if (dir.exists(path)) {
    hits <- list.files(path, recursive = FALSE, full.names = TRUE)
    hits <- hits[grepl(fc_filename_pattern, basename(hits), perl = TRUE)]
    candidate_files <- c(candidate_files, hits)
  }
}
candidate_files <- unique(candidate_files)
print(candidate_files)

if (length(candidate_files) > 0) {
  cat("\n---- Step 3: column names in each candidate file ----\n")
  for (f in candidate_files) {
    cat("\n", f, ":\n", sep = "")
    tryCatch({
      if (grepl("\\.xlsx$", f, ignore.case = TRUE)) {
        cols <- names(readxl::read_xlsx(f, n_max = 0))
      } else if (grepl("\\.rds$", f, ignore.case = TRUE)) {
        obj <- readRDS(f)
        cols <- if (is.data.frame(obj)) names(obj) else class(obj)
      } else {
        cols <- "(unrecognised file type - inspect manually)"
      }
      print(cols)
    }, error = function(e) cat("  could not read:", conditionMessage(e), "\n"))
  }
} else {
  cat("\nNo filenames matched the FC/calprotectin/bloods/labs pattern in these directories.\n")
  cat("A repeated FC source, if one exists, likely lives outside these known paths.\n")
}
