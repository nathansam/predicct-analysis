## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| message: false
setwd("src")
source("Survival/utils.R")

# Setup analysis environment
analysis_setup <- setup_analysis()
paths <- analysis_setup$paths
demo <- analysis_setup$demo

demo$FC <- log(demo$FC)

flare.df <- readRDS(paste0(paths$outdir, "flares-biochem.RDS"))
flare.cd.df <- readRDS(paste0(paths$outdir, "flares-biochem-cd.RDS"))
flare.uc.df <- readRDS(paste0(paths$outdir, "flares-biochem-uc.RDS"))

paths$outdir <- paste0(paths$outdir, "sensitivity/")

## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize meat protein by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "Meat_sum", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "Meat_sum",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Meat protein quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/meat",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "meat-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + Meat_sum_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value)

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "Meat_sum",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Meat protein quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/meat",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "meat-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + Meat_sum_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Hard") |>
  relocate(diagnosis, flare) |>
  select(-p.value))


# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize meat protein by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "Meat_sum", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "Meat_sum",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Meat protein quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/meat",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "meat-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + Meat_sum_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)
hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "UC", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "Meat_sum",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Meat protein quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/meat",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "meat-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + Meat_sum_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)
hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value))

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize meat protein by quantiles
flare.df <- categorize_by_quantiles(flare.df, "Meat_sum", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "Meat_sum",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Meat protein quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/meat",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + Meat_sum_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)
hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value))

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "Meat_sum",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Meat protein quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/meat",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + Meat_sum_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value))

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize overall meat intake by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "meat_overall", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)


# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "meat_overall",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Meat intake quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/meat_overall",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "meat-overall-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + meat_overall_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "meat_overall",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Meat intake quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/meat_overall",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "meat-overall-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + meat_overall_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value))

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize overall meat intake by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "meat_overall", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "meat_overall",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Meat intake quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/meat_overall",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "meat-overall-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + meat_overall_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value))

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "meat_overall",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Meat intake quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/meat_overall",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "meat-overall-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + meat_overall_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value))

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize overall meat intake by quantiles
flare.df <- categorize_by_quantiles(flare.df, "meat_overall", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "meat_overall",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Meat intake quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/meat_overall",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + meat_overall_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value))

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "meat_overall",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Meat intake quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/meat_overall",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + meat_overall_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value))

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize overall fish intake by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "fish_overall", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "fish_overall",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Fish intake quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/fish_overall",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "fish-overall-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + fish_overall_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value))

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "fish_overall",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Fish intake quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/fish_overall",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "fish-overall-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fish_overall_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value))

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize overall fish intake by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "fish_overall", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "fish_overall",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Fish intake quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/fish_overall",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "fish-overall-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fish_overall_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)
hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value))

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6
# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "fish_overall",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Fish intake quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/fish_overall",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "fish-overall-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fish_overall_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)
hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value))


# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize overall fish intake by quantiles
flare.df <- categorize_by_quantiles(flare.df, "fish_overall", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)


# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "fish_overall",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Fish intake quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/fish_overall",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fish_overall_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "fish_overall",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Fish intake quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/fish_overall",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fish_overall_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
  select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6


# Categorize dietary fibre by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "fibre", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)


# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "fibre",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Fibre quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/fibre",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "fibre-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fibre_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "fibre",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Fibre quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/fibre",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "fibre-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fibre_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize fibre by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "fibre", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)


# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "fibre",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Fibre quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/fibre",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "fibre-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fibre_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "fibre",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Fibre quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/fibre",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "fibre-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fibre_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize dietary fibre by quantiles
flare.df <- categorize_by_quantiles(flare.df, "fibre", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "fibre",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Fibre quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/fibre",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fibre_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "fibre",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Fibre quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/fibre",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fibre_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize PUFA by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "PUFA_percEng", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)


# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "PUFA_percEng",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "PUFA intake (by g) quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/pufa",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "pufa-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + PUFA_percEng_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)
hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "PUFA_percEng",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "PUFA intake (by g) quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/pufa",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "pufa-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + PUFA_percEng_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize PUFA by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "PUFA_percEng", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)


# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "PUFA_percEng",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "PUFA intake (by g) quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/pufa",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "pufa-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + PUFA_percEng_cat +
    frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "PUFA_percEng",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "PUFA intake (by g) quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/pufa",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "pufa-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + PUFA_percEng_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize PUFA by quantiles
flare.df <- categorize_by_quantiles(flare.df, "PUFA_percEng", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)


# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "PUFA_percEng",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "PUFA intake (by g) quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/pufa",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + PUFA_percEng_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "PUFA_percEng",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "PUFA intake (by g) quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivitypufa",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + PUFA_percEng_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "NOVAScore",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Nova score",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/nova",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "nova-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + NOVAScore_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "NOVAScore",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Nova Score",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/nova",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "nova-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + NOVAScore_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "NOVAScore",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Nova score",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/nova",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "nova-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + NOVAScore_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "NOVAScore",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Nova score",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/nova",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "nova-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + NOVAScore_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "NOVAScore",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Nova score",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/nova",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + NOVAScore_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "NOVAScore",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Nova score",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/nova",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + NOVAScore_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize UPF percentage by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "UPF_perc", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)


# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "UPF_perc",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "UPF as % of energy",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/UPF",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "upf-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + UPF_perc_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "UPF_perc",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "UPF as % of energy",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/UPF",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "upf-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + UPF_perc_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize UPF percentage by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "UPF_perc", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "UPF_perc",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "UPF as % of energy",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/UPF",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "upf-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + UPF_perc_cat +
    frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "UPF_perc",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "UPF as % of energy",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/UPF",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "upf-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + UPF_perc_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize UPF percentage by quantiles
flare.df <- categorize_by_quantiles(flare.df, "UPF_perc", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "UPF_perc",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "UPF as % of energy",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/UPF",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + UPF_perc_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "UPF_perc",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "UPF as % of energy",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/UPF",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + UPF_perc_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize bread intake by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "breadIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "breadIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Bread/cereal intake quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/breadIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "breadIntake-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + breadIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "breadIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Bread/cereal intake quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/breadIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "breadIntake-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + breadIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )
# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize bread intake by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "breadIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "breadIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Bread/cereal intake quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/breadIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "breadIntake-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + breadIntake_cat +
    frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "breadIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Bread/cereal intake quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/breadIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "breadIntake-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + breadIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize bread intake by quantiles
flare.df <- categorize_by_quantiles(flare.df, "breadIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "breadIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Bread/cereal intake quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/breadIntake",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + breadIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "breadIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Bread/cereal intake quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/breadIntake",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + breadIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize sweet intake by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "sweetIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "sweetIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Sweet/dessert/snack intake quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/sweetIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "sweetIntake-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + sweetIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "sweetIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Sweet/dessert/snack intake quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/sweetIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "sweetIntake-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + sweetIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize sweet intake by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "sweetIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "sweetIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Sweet/dessert/snack intake quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/sweetIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "sweetIntake-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + sweetIntake_cat +
    frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "sweetIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Sweet/dessert/snack intake quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/sweetIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "sweetIntake-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + sweetIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize sweet intake by quantiles
flare.df <- categorize_by_quantiles(flare.df, "sweetIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "sweetIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Sweet/dessert/snack intake quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/sweetIntake",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + sweetIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "sweetIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Sweet/dessert/snack intake quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/sweetIntake",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + sweetIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize drink intake by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "drinkIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "drinkIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Artificially and sugar-sweetened drink intake quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/drinkIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "drinkIntake-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + drinkIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "drinkIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Artificially and sugar-sweetened drink intake quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/drinkIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "drinkIntake-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + drinkIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize drink intake by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "drinkIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "drinkIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Artificially and sugar-sweetened drink intake quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/drinkIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "drinkIntake-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + drinkIntake_cat +
    frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "drinkIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Artificially and sugar-sweetened drink intake quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/drinkIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "drinkIntake-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + drinkIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize drink intake by quantiles
flare.df <- categorize_by_quantiles(flare.df, "drinkIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "drinkIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Artificially and sugar-sweetened drink intake quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/drinkIntake",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + drinkIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "drinkIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Artificially and sugar-sweetened drink intake quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/drinkIntake",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + drinkIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize processed meat intake by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "processedMeatIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "processedMeatIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Processed meat intake quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/processedMeatIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "processedMeatIntake-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + processedMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "processedMeatIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Processed meat intake quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/processedMeatIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "processedMeatIntake-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + processedMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize processed meat intake by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "processedMeatIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "processedMeatIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Processed meat intake quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/processedMeatIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "processedMeatIntake-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + processedMeatIntake_cat +
    frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "processedMeatIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Processed meat intake quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/processedMeatIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "processedMeatIntake-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + processedMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize processed meat intake by quantiles
flare.df <- categorize_by_quantiles(flare.df, "processedMeatIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "processedMeatIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Processed meat intake quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/processedMeatIntake",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + processedMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "processedMeatIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Processed meat intake quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/processedMeatIntake",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + processedMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize processed plant intake by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "processedPlantIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "processedPlantIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Processed plant-based alternative intake quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/processedPlantIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "processedPlantIntake-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + processedPlantIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "processedPlantIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Processed plant-based alternative intake quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/processedPlantIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "processedPlantIntake-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + processedPlantIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize processed plant intake by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "processedPlantIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "processedPlantIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Processed plant-based alternative intake quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/processedPlantIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "processedPlantIntake-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + processedPlantIntake_cat +
    frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "processedPlantIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Processed plant-based alternative intake quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/processedPlantIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "processedPlantIntake-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + processedPlantIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize processed plant intake by quantiles
flare.df <- categorize_by_quantiles(flare.df, "processedPlantIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "processedPlantIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Processed plant-based alternative intake quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/processedPlantIntake",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + processedPlantIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "processedPlantIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Processed plant-based alternative intake quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/processedPlantIntake",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + processedPlantIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize fruit intake by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "fruitIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "fruitIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Fruit intake quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/fruitIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "fruitIntake-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fruitIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "fruitIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Fruit intake quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/fruitIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "fruitIntake-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fruitIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize fruit intake by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "fruitIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "fruitIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Fruit intake quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/fruitIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "fruitIntake-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fruitIntake_cat +
    frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "fruitIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Fruit intake quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/fruitIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "fruitIntake-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + fruitIntake_cat + dqi_tot + BMI + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize fruit intake by quantiles
flare.df <- categorize_by_quantiles(flare.df, "fruitIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "fruitIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Fruit intake quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/fruitIntake",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fruitIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "fruitIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Fruit intake quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/fruitIntake",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + fruitIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize vegetable intake by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "vegIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "vegIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Vegetable/legume intake quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/vegIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "vegIntake-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + vegIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "vegIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Vegetable/legume intake quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/vegIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "vegIntake-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + vegIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize vegetable intake by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "vegIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "vegIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Vegetable/legume intake quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/vegIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "vegIntake-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + vegIntake_cat +
    frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "vegIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Vegetable/legume intake quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/vegIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "vegIntake-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + vegIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize vegetable intake by quantiles
flare.df <- categorize_by_quantiles(flare.df, "vegIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "vegIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Vegetable/legume intake quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/vegIntake",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + vegIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "vegIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Vegetable/legume intake quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/vegIntake",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + vegIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize red meat intake by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "redMeatIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "redMeatIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Red meat intake quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/redMeatIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "redMeatIntake-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + redMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "redMeatIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Red meat intake quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/redMeatIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "redMeatIntake-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + redMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize red meat intake by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "redMeatIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "redMeatIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Red meat intake quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/redMeatIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "redMeatIntake-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + redMeatIntake_cat +
    frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "redMeatIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Red meat intake quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/redMeatIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "redMeatIntake-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + redMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize red meat intake by quantiles
flare.df <- categorize_by_quantiles(flare.df, "redMeatIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "redMeatIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "Red meat intake quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/redMeatIntake",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + redMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "redMeatIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "Red meat intake quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/redMeatIntake",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + redMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize white meat intake by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "whiteMeatIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "whiteMeatIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "White meat intake quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/whiteMeatIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "whiteMeatIntake-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + whiteMeatIntake_cat + dqi_tot + BMI + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "whiteMeatIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "White meat intake quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/whiteMeatIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "whiteMeatIntake-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + whiteMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize white meat intake by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "whiteMeatIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "whiteMeatIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "White meat intake quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/whiteMeatIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "whiteMeatIntake-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + whiteMeatIntake_cat +
    frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "whiteMeatIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "White meat intake quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/whiteMeatIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "whiteMeatIntake-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + whiteMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize white meat intake by quantiles
flare.df <- categorize_by_quantiles(flare.df, "whiteMeatIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "whiteMeatIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "White meat intake quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/whiteMeatIntake",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + whiteMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "whiteMeatIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "White meat intake quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/whiteMeatIntake",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + whiteMeatIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize white fish intake by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "whiteFishIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.cd.df <- flare.cd.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "whiteFishIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "White fish intake quantiles",
  plot_base_path = "plots/cd/soft-flare/diet/sensitivity/whiteFishIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "whiteFishIntake-cd-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + whiteFishIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.cd.df,
  var_name = "whiteFishIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "White fish intake quantiles",
  plot_base_path = "plots/cd/hard-flare/diet/sensitivity/whiteFishIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "whiteFishIntake-cd-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + whiteFishIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize white fish intake by quantiles
flare.uc.df <- categorize_by_quantiles(flare.uc.df, "whiteFishIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.uc.df <- flare.uc.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "whiteFishIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "White fish intake quantiles",
  plot_base_path = "plots/uc/soft-flare/diet/sensitivity/whiteFishIntake",
  break_time_by = 200
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "whiteFishIntake-uc-soft.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + whiteFishIntake_cat +
    frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.uc.df,
  var_name = "whiteFishIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "White fish intake quantiles",
  plot_base_path = "plots/uc/hard-flare/diet/sensitivity/whiteFishIntake",
  break_time_by = 500
)

# Save plot as RDS
saveRDS(analysis_result$plot, paste0(paths$outdir, "whiteFishIntake-uc-hard.RDS"))

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + whiteFishIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Categorize white fish intake by quantiles
flare.df <- categorize_by_quantiles(flare.df, "whiteFishIntake", reference_data = flare.df)

# Sensitivity analysis using FC < 250
flare.df <- flare.df |> filter(exp(exp(FC)) < 250)

# Run survival analysis using utility function
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "whiteFishIntake",
  outcome_time = "softflare_time",
  outcome_event = "softflare",
  legend_title = "White fish intake quantiles",
  plot_base_path = "plots/ibd/soft-flare/diet/sensitivity/whiteFishIntake",
  break_time_by = 200
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + BMI + whiteFishIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "hold"
#| output: "asis"
#| fig-height: 6

# Run survival analysis using utility function for objective flare
analysis_result <- run_survival_analysis(
  data = flare.df,
  var_name = "whiteFishIntake",
  outcome_time = "hardflare_time",
  outcome_event = "hardflare",
  legend_title = "White fish intake quantiles",
  plot_base_path = "plots/ibd/hard-flare/diet/sensitivity/whiteFishIntake",
  break_time_by = 500
)

# Run Cox model with categorical variable
fit.me <- coxph(
  Surv(hardflare_time, hardflare) ~
    Sex + cat + IMD + dqi_tot + BMI + whiteFishIntake_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.df
)

hrs_sensitivity <- rbind(hrs_sensitivity, broom::tidy(fit.me) |> 
  filter(!grepl("^Sex|^cat|^IMD|^dqi_tot|^BMI|^frailty", term)) |>
  mutate(diagnosis = "CD", flare ="Soft") |>
  relocate(diagnosis, flare) |>
    select(-p.value) )

# Display plot and model summary
analysis_result$plot
invisible(cox_summary(fit.me))


## ----Session info--------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
pander::pander(sessionInfo())
