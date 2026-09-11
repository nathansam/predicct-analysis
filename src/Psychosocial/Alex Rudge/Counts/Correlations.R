library(tidyverse)
library(magrittr)
library(glue)
library(corrplot)

# Correlations between psychosocial variables

# Load data

data_psychosocial <- readr::read_rds("/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/all_psychosocial_variables.rds")


# Correlations between variables
correlation_data <- data_psychosocial %>%
  dplyr::select(
    anxiety_hads,
    depression_hads,
    somatisation_score,
    OftenLackEnergy,
    MinimumExercise,
    AnyLifeEvents,
    SleepDisturbance
  ) %>%
  dplyr::mutate(
    OftenLackEnergy = as.numeric(OftenLackEnergy == "Yes"),
    MinimumExercise = as.numeric(MinimumExercise == "Yes"),
    AnyLifeEvents = as.numeric(AnyLifeEvents == "Yes"),
    SleepDisturbance = as.numeric(SleepDisturbance == "Yes")
  )

# Binary variables are coded as 1 = Yes and 0 = No
# Use Kendall correlations with pairwise complete observations.
correlation_matrix <- stats::cor(
  correlation_data,
  use = "pairwise.complete.obs",
  method = "kendall"
)

tidy_names <- c(
  "HADS anxiety",
  "HADS depression",
  "Somatisation",
  "Fatigue",
  "Exercise",
  "Life events",
  "Sleep disturbance"
)

dimnames(correlation_matrix) <- list(tidy_names, tidy_names)


# Plot the correlation matrix
corrplot::corrplot(
  correlation_matrix,
  method = "color",
  type = "lower",
  order = "hclust",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 90
)

correlation_plot <- grDevices::recordPlot()

grDevices::pdf(
  "/Users/arudge/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Predicct/Plots/2/Other/psychosocial_correlations.pdf",
  width = 5,
  height = 5
)

grDevices::replayPlot(correlation_plot)

grDevices::dev.off()
