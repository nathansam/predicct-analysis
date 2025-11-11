#!/usr/bin/env Rscript

# Move plots from src/plots to docs/plots and update HTML references

cat("Moving plots to docs/plots...\n")

# Copy plots directory to docs
src_plots <- "plots"


 if (file.exists("/docker")) {
  dest_plots <- "temp/Survival/plots"
 } else {
  dest_plots <- "../docs/plots"
 }

if (dir.exists(src_plots)) {
  # Create destination if it doesn't exist
  if (!dir.exists(dest_plots)) {
    dir.create(dest_plots, recursive = TRUE)
  }

  # Copy all files recursively
  files <- list.files(src_plots, recursive = TRUE, full.names = TRUE)
  for (file in files) {
    finfo <- file.info(file)
    if (is.null(finfo) || isTRUE(finfo$isdir)) next

    # Get relative path
    rel_path <- sub(paste0("^", src_plots, "/"), "", file)
    dest_file <- file.path(dest_plots, rel_path)

    # Create directory if needed
    dest_dir <- dirname(dest_file)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
    }

    # Copy file
    file.copy(file, dest_file, overwrite = TRUE)
  }

  cat(sprintf("Copied %d files from %s to %s\n", length(files), src_plots, dest_plots))
}

# Update HTML files to use correct relative paths
html_files <- list.files("../docs", pattern = "\\.html$", recursive = TRUE, full.names = TRUE)

for (html_file in html_files) {
  content <- readLines(html_file, warn = FALSE)

  # Replace src="plots/ with src="../plots/ for files in subdirectories
  if (grepl("/", sub("^\\.\\.\\/docs\\/", "", html_file))) {
    content <- gsub('src="plots/', 'src="../plots/', content)
  }

  writeLines(content, html_file)
}

cat(sprintf("Updated %d HTML files with correct plot paths\n", length(html_files)))
cat("Done!\n")
