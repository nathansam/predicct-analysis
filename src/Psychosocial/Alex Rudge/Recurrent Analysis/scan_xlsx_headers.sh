#!/bin/bash
# Scans every .xlsx file under the given directories for text matching
# lab-received-date-style column names (DateReceivedAtLab, datelab,
# ReceiveDate, and close variants) - a candidate signature for a repeated
# FC/calprotectin lab-results file this build doesn't currently use. For
# any file that matches, reports (per sheet) which columns matched and how
# many rows that sheet has.
#
# Two-stage approach:
#   1. Fast pre-filter: an .xlsx is a zip archive; all its text strings
#      (including column headers) live in xl/sharedStrings.xml. This reads
#      just that one internal XML file per workbook (via `unzip -p`, no
#      extraction to disk) and greps it - cheap enough to run across many
#      files, but workbook-wide, so it can't say which sheet or how many
#      rows.
#   2. For files that pass stage 1 only: hand off to a small embedded R
#      snippet (readxl) that opens just that file, checks each sheet's
#      actual column names, and counts rows for any sheet with a match -
#      precise, but only run on the few files worth it.
#
# Output policy: filenames, column names, sheet names, and row counts only
# - never actual cell data/values.
#
# Usage: ./scan_xlsx_headers.sh <dir1> [<dir2> ...]

set -uo pipefail

if [ "$#" -eq 0 ]; then
  echo "Usage: $0 <dir1> [<dir2> ...]" >&2
  exit 1
fi

pattern='DateReceivedAtLab|datelab|ReceiveDate|ReceivedDate|DateReceived|LabDate|Calprotectin|FecalCalprotectin'

inspect_file() {
  local f="$1"
  Rscript --vanilla -e '
    args <- commandArgs(trailingOnly = TRUE)
    f <- args[1]
    pat <- args[2]
    suppressPackageStartupMessages(library(readxl))
    sheets <- tryCatch(excel_sheets(f), error = function(e) character(0))
    for (s in sheets) {
      cols <- tryCatch(names(read_xlsx(f, sheet = s, n_max = 0)), error = function(e) character(0))
      matched <- cols[grepl(pat, cols, ignore.case = TRUE, perl = TRUE)]
      if (length(matched) > 0) {
        n <- tryCatch(nrow(read_xlsx(f, sheet = s, col_types = "text")), error = function(e) NA)
        cat("    Sheet:", s, "| matching columns:", paste(matched, collapse = ", "), "| rows:", n, "\n")
      }
    }
  ' "$f" "$pattern" 2>/dev/null
}

for dir in "$@"; do
  echo "==== Scanning $dir ===="
  if [ ! -d "$dir" ]; then
    echo "  (directory does not exist or is not accessible)"
    continue
  fi
  find "$dir" -iname "*.xlsx" -type f 2>/dev/null | while IFS= read -r f; do
    matches=$(unzip -p "$f" xl/sharedStrings.xml 2>/dev/null | grep -oiE "$pattern")
    if [ -n "$matches" ]; then
      echo "MATCH: $f"
      inspect_file "$f"
    fi
  done
done
