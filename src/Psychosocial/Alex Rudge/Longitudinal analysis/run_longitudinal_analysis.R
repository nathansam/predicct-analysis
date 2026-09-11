library(tictoc)


# Run from the Longitudinal analysis directory in the predicct-analysis project.
# The R chunks from each QMD are executed in the current R session so that the objects they create are available to the corresponding Cox scripts.

setwd("/Users/arudge/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis")

tic()

# Exercise
qmd_code <- tempfile(fileext = ".R")

knitr::purl("Exercise/Exercise.qmd", output = qmd_code, documentation = 0)
source(qmd_code, local = .GlobalEnv)
source("Exercise/Cox results.R", local = .GlobalEnv)

unlink(qmd_code)
rm(list = ls(envir = .GlobalEnv, all.names = TRUE), envir = .GlobalEnv)

# Fatigue
qmd_code <- tempfile(fileext = ".R")

knitr::purl("Fatigue/Fatigue.qmd", output = qmd_code, documentation = 0)
source(qmd_code, local = .GlobalEnv)
source("Fatigue/Cox results.R", local = .GlobalEnv)

unlink(qmd_code)
rm(list = ls(envir = .GlobalEnv, all.names = TRUE), envir = .GlobalEnv)

# HADS
qmd_code <- tempfile(fileext = ".R")

knitr::purl("HADS/HADS.qmd", output = qmd_code, documentation = 0)
source(qmd_code, local = .GlobalEnv)
source("HADS/Cox results.R", local = .GlobalEnv)

unlink(qmd_code)
rm(list = ls(envir = .GlobalEnv, all.names = TRUE), envir = .GlobalEnv)

# Life Events
qmd_code <- tempfile(fileext = ".R")

knitr::purl("Life Events/Life Events.qmd", output = qmd_code, documentation = 0)
source(qmd_code, local = .GlobalEnv)
source("Life Events/Cox results.R", local = .GlobalEnv)

unlink(qmd_code)
rm(list = ls(envir = .GlobalEnv, all.names = TRUE), envir = .GlobalEnv)

# PHQ
qmd_code <- tempfile(fileext = ".R")

knitr::purl("PHQ/PHQ.qmd", output = qmd_code, documentation = 0)
source(qmd_code, local = .GlobalEnv)
source("PHQ/Cox results.R", local = .GlobalEnv)

unlink(qmd_code)
rm(list = ls(envir = .GlobalEnv, all.names = TRUE), envir = .GlobalEnv)

# PSQI
qmd_code <- tempfile(fileext = ".R")

knitr::purl("PSQI/PSQI.qmd", output = qmd_code, documentation = 0)
source(qmd_code, local = .GlobalEnv)
source("PSQI/Cox results.R", local = .GlobalEnv)

unlink(qmd_code)
rm(list = ls(envir = .GlobalEnv, all.names = TRUE), envir = .GlobalEnv)

# Combine the variable-specific Cox results
source("Summary/Cox results for all variables.R", local = .GlobalEnv)

toc()
