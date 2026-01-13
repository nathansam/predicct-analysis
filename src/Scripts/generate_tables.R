source("Scripts/Subscripts/Diet.tmp.R")
source("Scripts/Subscripts/Diet_sensitivity.R")


# Code to generate Supplementary Tables S6 and S7
final.table <- cbind(
  hrs |> select(diagnosis, flare, term, estimate, std.error),
  hrs_sensitivity |> select(estimate, std.error)
)
colnames(final.table) <- c(
  colnames(final.table)[1:5],
  "estimate.sensitivity",
  "std.error.sensitivity"
)

library(dplyr)
library(purrr)
library(tidyr)
library(officer)
library(flextable)
library(googledrive)

# Assume final.table exists with columns:
# diagnosis, flare, term, HR, std.error, HR.sensitivity, std.error.sensitivity

final.table <- final.table %>%
  mutate(
    `Main (SE)` = sprintf("%.3f (%.3f)", estimate, std.error),
    `Sensitivity (SE)` = sprintf(
      "%.3f (%.3f)",
      estimate.sensitivity,
      std.error.sensitivity
    )
  )

final.table$term <- c(
  rep(paste("Meat protein", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Overall meat intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Overall fish intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Dietary fibre", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("PUFA", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("NOVA Score", c("2", "3", "4")), 6),
  rep(paste("%UPF", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Bread intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Sweet intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Drink intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Processed meat intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Processed plant intake", "above median"), 6),
  rep(paste("Fruit intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Vegetable intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Read meat intake", c("2nd", "3rd"), "tertile"), 6),
  rep(paste("White meat intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("White fish intake", c("2nd", "3rd", "4th"), "quartile"), 6)
)


# Six combinations
combos <- tidyr::expand_grid(
  diagnosis = c("CD", "UC", "IBD"),
  flare = c("Soft", "Hard")
)

# Builder for a compact flextable with your 3 columns
make_ft <- function(df) {
  ft <- flextable(df %>% select(term, `Main (SE)`, `Sensitivity (SE)`))
  ft <- set_header_labels(
    ft,
    term = "Term" # <-- pass the flextable as the first arg (x)
  )
  ft <- autofit(ft)
  ft <- theme_box(ft)
  ft <- fontsize(ft, part = "all", size = 9)
  ft <- bold(ft, part = "header")
  ft <- align(
    ft,
    j = c("Main (SE)", "Sensitivity (SE)"),
    align = "right",
    part = "body"
  )
  ft
}

doc <- read_docx()

for (i in seq_len(nrow(combos))) {
  dx <- combos$diagnosis[i]
  fl <- combos$flare[i]
  chunk <- final.table %>% filter(diagnosis == dx, flare == fl)

  doc <- body_add_par(doc, paste(dx, "×", fl), style = "heading 2")
  if (nrow(chunk) > 0) {
    doc <- body_add_flextable(doc, make_ft(chunk))
  } else {
    doc <- body_add_par(doc, "No rows for this group.", style = "Normal")
  }
}

out_path <- file.path(paste0(
  getwd(),
  "/",
  "docx/supplementary-sensitivity.docx"
))
print(doc, target = out_path)

library(dplyr)
library(tidyr)
library(officer)
library(flextable)

# ---- Assumes you already have `pvalues` in memory ----
# pvalues columns: diagnosis, flare, term, p.value, adjusted.p.value

# Pretty p-value formatter
fmt_p <- function(x) {
  ifelse(
    is.na(x),
    "—",
    ifelse(
      x < 0.001,
      "<0.001",
      ifelse(x >= 0.1, sprintf("%.2f", x), sprintf("%.3f", x))
    )
  )
}

pv_disp <- pvalues %>%
  mutate(
    `p-value` = fmt_p(p.value),
    `Adj. p-value` = fmt_p(adjusted.p.value)
  )

pv_disp$term <- c(
  rep(paste("Meat protein", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Overall meat intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Overall fish intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Dietary fibre", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("PUFA", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("NOVA Score", c("2", "3", "4")), 6),
  rep(paste("%UPF", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Bread intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Sweet intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Drink intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Processed meat intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Processed plant intake", "above median"), 6),
  rep(paste("Fruit intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Vegetable intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("Read meat intake", c("2nd", "3rd"), "tertile"), 6),
  rep(paste("White meat intake", c("2nd", "3rd", "4th"), "quartile"), 6),
  rep(paste("White fish intake", c("2nd", "3rd", "4th"), "quartile"), 6)
)


# Six groups
combos <- tidyr::expand_grid(
  diagnosis = c("CD", "UC", "IBD"),
  flare = c("Soft", "Hard")
)

# Flextable helper: only Term | p-value | Adj. p-value
make_ft <- function(dat) {
  out <- dat %>%
    select(term, `p-value`, `Adj. p-value`)

  ft <- flextable(out)
  ft <- set_header_labels(ft, term = "Term")
  ft <- autofit(ft)
  ft <- theme_box(ft)
  ft <- fontsize(ft, part = "all", size = 9)
  ft <- bold(ft, part = "header")
  ft <- bg(ft, part = "header", bg = "#F3F4F6")
  ft <- align(
    ft,
    j = c("p-value", "Adj. p-value"),
    align = "right",
    part = "body"
  )

  # Optional: bold rows with adjusted p < 0.05
  sig_rows <- which(!is.na(dat$adjusted.p.value) & dat$adjusted.p.value < 0.05)
  if (length(sig_rows)) {
    ft <- bold(ft, i = sig_rows, part = "body")
  }

  ft
}

# Build DOCX
doc <- read_docx()
doc <- body_add_par(doc, "P-values by subgroup", style = "heading 1")

for (i in seq_len(nrow(combos))) {
  dx <- combos$diagnosis[i]
  fl <- combos$flare[i]

  chunk <- pv_disp %>% filter(diagnosis == dx, flare == fl)

  doc <- body_add_par(doc, paste(dx, "×", fl), style = "heading 2")
  if (nrow(chunk) > 0) {
    doc <- body_add_flextable(doc, make_ft(chunk))
  } else {
    doc <- body_add_par(doc, "No rows for this group.", style = "Normal")
  }
}

out_path <- file.path(paste0(getwd(), "/", "docx/supplementary-pvalues.docx"))
print(doc, target = out_path)
message("Wrote: ", normalizePath(out_path))
