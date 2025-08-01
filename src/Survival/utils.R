# Utility functions for the Survival analysis

# Load required libraries
load_libraries <- function() {
  suppressMessages({
    library(readxl)
    library(tidyverse)
    library(datefixR)
    library(survival)
    library(survminer)
    library(pander)
    library(coxme)
    library(finalfit)
    library(DescTools)
    library(gtsummary)
  })
}

# Set paths based on environment
set_paths <- function() {
  paths <- list()
  if (file.exists("/docker")) {
    # If running in docker
    paths$data.path <- "data/final/20221004/"
    paths$redcap.path <- "data/final/20231030/"
    paths$all.flare.path <- "data/final/20240308/Followup/"
    paths$prefix <- "data/end-of-follow-up/"
    paths$followup_dir <- "data/end-of-follow-up/"
    paths$outdir <- "data/processed/"
  } else {
    # Run on OS directly
    paths$data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"
    paths$redcap.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20231030/"
    paths$all.flare.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20240308/Followup/"
    paths$prefix <- "/Volumes/igmm/cvallejo-predicct/predicct/end-of-follow-up/"
    paths$followup_dir <- "/Volumes/igmm/cvallejo-predicct/predicct/end-of-follow-up/"
    paths$outdir <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
  }
  return(paths)
}

# Function to summarize Cox models
cox_summary <- function(fit) {
  cat("Cox model summary:\n")
  fit %>%
    finalfit::fit2df(condense = FALSE) %>%
    knitr::kable(
      col.names = c(
        "Variable",
        "HR",
        "Lower 95%",
        "Upper 95%",
        "P-value"
      ),
      digits = 4
    ) %>%
    print()

  cat("\nDiagnostics: \n\n")
  cat("::: {.panel-tabset}\n\n")
  cat("###### Proportional hazards assumption test \n\n")
  cox.zph(fit)$table %>%
    knitr::kable(
      col.names = c(
        "",
        "Chi-squared statistic",
        "DF",
        "P-value"
      ),
      digits = 4
    ) %>%
    print()

  cat("\n###### DF betas \n")
  print(ggcoxdiagnostics(fit, type = "dfbeta"))

  cat("\n###### Martingale residuals \n")
  print(ggcoxdiagnostics(fit, type = "martingale", linear.predictions = TRUE))

  cat("\n:::")
  return()
}

# Function to produce survival plot
generate_survival_plot <- function(
  data,
  formula,
  legend_title,
  legend_labs,
  palette,
  xlab,
  title,
  break_time_by,
  plot_path
) {
  fit <- survfit(formula, data = data)
  
  # Store the formula in the survfit object to prevent extraction issues
  fit$call$formula <- formula
  
  p <- ggsurvplot(
    fit,
    data = data,
    conf.int = TRUE,
    pval = TRUE,
    pval.method = TRUE,
    ggtheme = theme_minimal(),
    risk.table = TRUE,
    legend.title = legend_title,
    legend.labs = legend_labs,
    palette = palette,
    xlab = xlab,
    title = title,
    break.time.by = break_time_by
  )
  cairo_pdf(paste0(plot_path, ".pdf"))
  print(p, newpage = FALSE)
  dev.off()

  png(paste0(plot_path, ".png"), width = 7, height = 7, units = "in", res = 300)
  print(p, newpage = FALSE)
  dev.off()

  return(p)
}

# Function to categorize data
categorize_variable <- function(df, var_name, breaks, labels) {
  df[[paste0(var_name, "_cat")]] <- cut(
    df[[var_name]],
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = FALSE
  )
  return(df)
}

# Function to categorize by quantiles
categorize_by_quantiles <- function(
  df,
  var_name,
  reference_data = NULL,
  num_quantiles = 4
) {
  if (is.null(reference_data)) {
    reference_data <- df
  }
  quants <- quantile(reference_data[[var_name]], na.rm = TRUE)
  breaks <- c(0, quants[2:(num_quantiles + 1)])
  df[[paste0(var_name, "_cat")]] <- cut(
    df[[var_name]],
    breaks = breaks,
    include.lowest = TRUE,
    right = FALSE
  )
  return(df)
}

# Function to get hazard ratios
get_HR <- function(fit, var) {
  # Get point estimate with 95% CI
  HR.dat <- summary(fit)$conf.int %>%
    as.data.frame() %>%
    filter(rownames(summary(fit)$conf.int) %in% var) %>%
    select(`exp(coef)`, `lower .95`, `upper .95`)
  # Ensure variables are in the same order as var
  HR.dat <- HR.dat[var, ]
  # Add p-value
  HR.dat$p <- summary(fit)$coefficients[var, "p"]
  names(HR.dat) <- c("HR", "Lower95", "Upper95", "p")
  HR.dat
}

# Function to run full survival analysis for a variable
run_survival_analysis <- function(
  data,
  var_name,
  outcome_time,
  outcome_event,
  covariates = c("Sex", "IMD", "cat"),
  frailty_var = "SiteNo",
  legend_title = var_name,
  plot_base_path,
  break_time_by = 200,
  palette = c("#1A8FE3", "#FED766", "orange", "#E76D83", "#9B5DE5")
) {
  # Check if variable is already categorized
  cat_var_name <- paste0(var_name, "_cat")
  
  # Determine which variable to use for survival analysis and Cox model
  if (cat_var_name %in% names(data) && !var_name %in% names(data)) {
    # Variable is already categorized, use the categorical version
    surv_var <- cat_var_name
    cox_var <- cat_var_name
  } else if (var_name %in% names(data)) {
    # Variable exists as continuous, use categorical version for survival plot
    surv_var <- cat_var_name
    cox_var <- var_name
  } else {
    stop(paste("Neither", var_name, "nor", cat_var_name, "found in data"))
  }
  
  # Create formula for survival fit
  surv_formula <- as.formula(paste0(
    "Surv(",
    outcome_time,
    ", ",
    outcome_event,
    ") ~ ",
    surv_var
  ))

  # Create formula for Cox model
  cox_formula <- as.formula(paste0(
    "Surv(",
    outcome_time,
    ", ",
    outcome_event,
    ") ~ ",
    paste(
      c(covariates, cox_var, paste0("frailty(", frailty_var, ")")),
      collapse = " + "
    )
  ))

  # Generate survival plot
  fit <- survfit(surv_formula, data = data)
  
  # Store the formula in the survfit object to prevent extraction issues
  fit$call$formula <- surv_formula
  
  p <- ggsurvplot(
    fit,
    data = data,
    conf.int = TRUE,
    pval = TRUE,
    pval.method = TRUE,
    ggtheme = theme_minimal(),
    risk.table = TRUE,
    legend.title = legend_title,
    legend.labs = levels(data[[paste0(var_name, "_cat")]]),
    palette = palette,
    xlab = "Time from study recruitment (days)",
    title = paste("Time to", outcome_event),
    break.time.by = break_time_by
  )

  # Save plots
  cairo_pdf(paste0(plot_base_path, ".pdf"))
  print(p)
  invisible(dev.off())

  png(
    paste0(plot_base_path, ".png"),
    width = 7,
    height = 7,
    units = "in",
    res = 300
  )
  print(p)
  invisible(dev.off())

  # Run Cox model
  fit.me <- coxph(
    cox_formula,
    control = coxph.control(outer.max = 20),
    data = data
  )

  # Return results
  # Try to get HR data, but handle case where var_name doesn't match coefficient names
  hr_data <- tryCatch({
    get_HR(fit.me, var_name)
  }, error = function(e) {
    NULL
  })
  
  list(
    plot = p,
    cox_model = fit.me,
    hr_data = hr_data
  )
}

# Function to create monthly response plots
create_monthly_response_plot <- function(
  data,
  fill_color,
  border_color,
  y_label,
  x_label = "",
  y_max = 1200
) {
  data %>%
    mutate(Q_month = paste0("M", Q_month)) %>%
    ggplot(aes(x = factor(Q_month, levels = paste0("M", seq(1, 24))))) +
    geom_bar(fill = fill_color, color = border_color) +
    theme_minimal() +
    ylab(y_label) +
    xlab(x_label) +
    scale_x_discrete(
      breaks = paste0("M", seq(1, 24, by = 2)),
      labels = paste0("M", seq(1, 24, by = 2))
    ) +
    ylim(0, y_max)
}

# Function to create standard Kaplan-Meier plots for flare analysis
create_km_flare_plot <- function(
  data,
  formula,
  legend_title = NULL,
  legend_labs = NULL,
  palette = c("#4F6D7A", "#02C3BD"),
  xlab = "Time from study recruitment (days)",
  break_time_by = 200,
  show_pval = TRUE,
  xlim = NULL,
  save_path = NULL
) {
  fit <- survfit(formula, data = data)
  
  # Store the formula in the survfit object to prevent extraction issues
  fit$call$formula <- formula

  p <- ggsurvplot(
    fit,
    data = data,
    conf.int = TRUE,
    pval = show_pval,
    pval.method = show_pval,
    ggtheme = theme_minimal(),
    risk.table = TRUE,
    legend.title = legend_title,
    legend.labs = legend_labs,
    tables.y.text = FALSE,
    palette = palette,
    xlab = xlab,
    tables.col = "strata",
    break.time.by = break_time_by
  )

  # Apply xlim if specified
  if (!is.null(xlim)) {
    p$plot <- p$plot + xlim(xlim)
  }

  # Save plot if path provided
  if (!is.null(save_path)) {
    saveRDS(p, paste0(save_path, ".RDS"))
  }

  return(p)
}

# Function to setup standard analysis environment
setup_analysis <- function() {
  load_libraries()
  paths <- set_paths()

  # Load demographics data
  demo <- readRDS(paste0(paths$outdir, "demo-full.RDS"))
  demo$FC <- log(demo$FC)

  return(list(paths = paths, demo = demo))
}
