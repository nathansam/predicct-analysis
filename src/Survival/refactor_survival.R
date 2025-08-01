# Refactoring script for survival analysis files
# This script systematically refactors repetitive survival analysis code
# to use the utility functions defined in utils.R

# Load required libraries
library(stringr)

# Function to refactor a single QMD file
refactor_qmd_file <- function(file_path) {
  cat("Refactoring:", file_path, "\n")
  
  # Read file content
  content <- readLines(file_path)
  content <- paste(content, collapse = "\n")
  
  # Pattern 1: Replace manual cut() categorization with categorize_by_quantiles()
  # This matches patterns like: flare.cd.df$Variable_cat <- cut(flare.cd.df$Variable, quantile(...), ...)
  pattern1 <- "([a-z.]+)\\$([A-Za-z_]+)_cat\\s*<-\\s*cut\\(\\1\\$\\2,\\s*quantile\\(flare\\.df\\$\\2,\\s*na\\.rm\\s*=\\s*TRUE\\),\\s*include\\.lowest\\s*=\\s*TRUE,\\s*right\\s*=\\s*FALSE\\)"
  replacement1 <- "\\1 <- categorize_by_quantiles(\\1, \"\\2\", reference_data = flare.df)"
  content <- str_replace_all(content, pattern1, replacement1)
  
  # Pattern 2: Update paths setup to use analysis_setup
  old_setup <- "source\\(\"utils\\.R\"\\)\\s*library\\(forestplot\\)\\s*(demo\\s*<-[^}]+}[^}]+}\\s*)?"
  new_setup <- "source(\"utils.R\")
library(forestplot)

# Setup analysis environment
analysis_setup <- setup_analysis()
paths <- analysis_setup$paths
demo <- analysis_setup$demo"
  content <- str_replace_all(content, old_setup, new_setup)
  
  # Pattern 3: Fix save paths to use paths$outdir instead of outdir
  content <- str_replace_all(content, "paste0\\(outdir,", "paste0(paths$outdir,")
  
  # Write back to file
  writeLines(strsplit(content, "\n")[[1]], file_path)
  cat("Completed refactoring:", file_path, "\n")
}

# Get all QMD files in the Survival directory
qmd_files <- list.files("/Users/nconsta2/GitHub/predicct-analysis/src/Survival", 
                        pattern = "\\.qmd$", full.names = TRUE)

# Exclude utils.R and this refactoring script
qmd_files <- qmd_files[!grepl("utils\\.R|refactor_survival\\.R", qmd_files)]

# Refactor each file
for (file in qmd_files) {
  tryCatch({
    refactor_qmd_file(file)
  }, error = function(e) {
    cat("Error refactoring", file, ":", e$message, "\n")
  })
}

cat("Refactoring complete!\n")
