
# Template for the survival analysis

# Need to replace 'variable_name' with the exposure variable
# Need to change the splines to the correct df for the data.


#### Full Cohort

##### Patient reported flare

```{r}
# Patient reported flare
# Kaplan Meier
data_survival_soft %>%
  survfit(Surv(time, DiseaseFlareYN) ~ variable_name, data = .) %>%
  ggsurvplot(
    ., 
    data = data_survival_soft, 
    conf.int = TRUE, 
    risk.table = TRUE,
    pval = TRUE,
    pval.method = TRUE,
    legend.title = 'HADS Anxiety Score',
    xlab = "Time from study recruitment (days)",
    ggtheme = theme_minimal())

# Cox model
cox_anxiety_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    variable_name +
    IMD +
    Sex +
    age_decade +
    FC +
    flare_group +
    ns(OverallControl, df = 3) +
    Smoke +
    frailty(SiteNo),
  data = data_survival_soft
)

cox_anxiety_soft %>%
  print_survival()

```

```{r}
# Cox model with MICE
# Run the mice
cox_anxiety_soft_mice <- coxph_mice(
  Surv(time, DiseaseFlareYN) ~
    variable_name +
    IMD +
    Sex +
    age_decade +
    FC +
    flare_group +
    OverallControl +
    Smoke +
    frailty(SiteNo),
  data = data_survival_soft,
  m = 20,
  printFlag = FALSE
)

# Fit the Cox
cox_anxiety_soft <- with(
  cox_anxiety_soft_mice,
  coxph(
    Surv(time, DiseaseFlareYN) ~
      variable_name +
      IMD +
      Sex +
      age_decade +
      FC +
      flare_group +
      ns(OverallControl, df = 3) +
      Smoke +
      frailty(SiteNo)
  )
) %>%
  mice::pool()

cox_anxiety_soft %>% 
  summary(conf.int = TRUE,
          conf.level = 0.95,
          exponentiate = TRUE)

```

##### Objective flare

```{r}
# Patient reported flare
# Kaplan Meier
data_survival_hard %>%
  survfit(Surv(time, DiseaseFlareYN) ~ variable_name, data = .) %>%
  ggsurvplot(
    ., 
    data = data_survival_hard, 
    conf.int = TRUE, 
    risk.table = TRUE,
    pval = TRUE,
    pval.method = TRUE,
    legend.title = 'HADS Anxiety Score',
    xlab = "Time from study recruitment (days)",
    ggtheme = theme_minimal())

# Cox model
cox_anxiety_hard <- coxph(
  Surv(time, DiseaseFlareYN) ~
    variable_name +
    IMD +
    Sex +
    age_decade +
    FC +
    flare_group +
    OverallControl +
    Smoke +
    frailty(SiteNo),
  data = data_survival_hard
)

cox_anxiety_hard %>%
  print_survival()
```

```{r}
# Cox model with MICE
# Run the mice
cox_anxiety_hard_mice <- coxph_mice(
  Surv(time, DiseaseFlareYN) ~
    variable_name +
    IMD +
    Sex +
    age_decade +
    FC +
    flare_group +
    OverallControl +
    Smoke +
    frailty(SiteNo),
  data = data_survival_hard,
  m = 20,
  printFlag = FALSE
)

# Fit the Cox
cox_anxiety_hard <- with(
  cox_anxiety_hard_mice,
  coxph(
    Surv(time, DiseaseFlareYN) ~
      variable_name +
      IMD +
      Sex +
      age_decade +
      FC +
      flare_group +
      OverallControl +
      Smoke +
      frailty(SiteNo)
  )
) %>%
  mice::pool()

cox_anxiety_hard %>% 
  summary(conf.int = TRUE,
          conf.level = 0.95,
          exponentiate = TRUE)
```

#### Ulcerative Colitis

##### Patient reported flare

```{r}
# Patient reported flare
# Kaplan Meier
data_survival_soft_uc %>%
  survfit(Surv(time, DiseaseFlareYN) ~ variable_name, data = .) %>%
  ggsurvplot(
    ., 
    data = data_survival_soft_uc, 
    conf.int = TRUE, 
    risk.table = TRUE,
    pval = TRUE,
    pval.method = TRUE,
    legend.title = 'HADS Anxiety Score',
    xlab = "Time from study recruitment (days)",
    ggtheme = theme_minimal())

# Cox model
cox_anxiety_soft_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    variable_name +
    IMD +
    Sex +
    age_decade +
    FC +
    flare_group +
    ns(OverallControl, df = 2) +
    Smoke +
    frailty(SiteNo),
  data = data_survival_soft_uc
)

cox_anxiety_soft_uc %>%
  print_survival()

```

```{r}
# Cox model with MICE
# Run the mice
cox_anxiety_soft_uc_mice <- coxph_mice(
  Surv(time, DiseaseFlareYN) ~
    variable_name +
    IMD +
    Sex +
    age_decade +
    FC +
    flare_group +
    OverallControl +
    Smoke +
    frailty(SiteNo),
  data = data_survival_soft_uc,
  m = 20,
  printFlag = FALSE
)

# Fit the Cox
cox_anxiety_soft_uc <- with(cox_anxiety_soft_uc_mice,
                            coxph(
                              Surv(time, DiseaseFlareYN) ~
                                variable_name +
                                IMD +
                                Sex +
                                age_decade +
                                FC +
                                flare_group +
                                ns(OverallControl, df = 2) +
                                Smoke +
                                frailty(SiteNo)
                            )) %>%
  mice::pool()

cox_anxiety_soft_uc %>% 
  summary(conf.int = TRUE,
          conf.level = 0.95,
          exponentiate = TRUE)

```

##### Objective flare

```{r}
# Patient reported flare
# Kaplan Meier
data_survival_hard_uc %>%
  survfit(Surv(time, DiseaseFlareYN) ~ variable_name, data = .) %>%
  ggsurvplot(
    ., 
    data = data_survival_hard_uc, 
    conf.int = TRUE, 
    risk.table = TRUE,
    pval = TRUE,
    pval.method = TRUE,
    legend.title = 'HADS Anxiety Score',
    xlab = "Time from study recruitment (days)",
    ggtheme = theme_minimal())

# Cox model
cox_anxiety_hard_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    variable_name +
    IMD +
    Sex +
    ns(age_decade, df = 4) +
    ns(FC, df = 2) +
    flare_group +
    OverallControl +
    Smoke +
    frailty(SiteNo),
  data = data_survival_hard_uc
)

cox_anxiety_hard_uc %>%
  print_survival()
```

```{r}
# Cox model with MICE
# Run the mice
# Note: mice does not support splines so I cannot impute using a spline.
cox_anxiety_hard_uc_mice <- coxph_mice(
  Surv(time, DiseaseFlareYN) ~
    variable_name +
    IMD +
    Sex +
    age_decade +
    FC +
    flare_group +
    OverallControl +
    Smoke +
    frailty(SiteNo),
  data = data_survival_hard_uc,
  m = 20,
  printFlag = FALSE
)

# Fit the Cox
cox_anxiety_hard_uc <- with(cox_anxiety_hard_uc_mice,
                            coxph(
                              Surv(time, DiseaseFlareYN) ~
                                variable_name +
                                IMD +
                                Sex +
                                ns(age_decade, df = 4) +
                                ns(FC, df = 2) +
                                flare_group +
                                OverallControl +
                                Smoke +
                                frailty(SiteNo)
                            )) %>%
  mice::pool()

cox_anxiety_hard_uc %>% 
  summary(conf.int = TRUE,
          conf.level = 0.95,
          exponentiate = TRUE)
```

#### Crohn's Disease

##### Patient reported flare.

```{r}
# Patient reported flare
# Kaplan Meier
data_survival_soft_cd %>%
  survfit(Surv(time, DiseaseFlareYN) ~ variable_name, data = .) %>%
  ggsurvplot(
    ., 
    data = data_survival_soft_cd, 
    conf.int = TRUE, 
    risk.table = TRUE,
    pval = TRUE,
    pval.method = TRUE,
    legend.title = 'HADS Anxiety Score',
    xlab = "Time from study recruitment (days)",
    ggtheme = theme_minimal())

# Cox model
cox_anxiety_soft_cd <- coxph(
  Surv(time, DiseaseFlareYN) ~
    variable_name +
    IMD +
    Sex +
    age_decade +
    FC +
    flare_group +
    ns(OverallControl, df = 2) +
    Smoke +
    frailty(SiteNo),
  data = data_survival_soft_cd
)

cox_anxiety_soft_cd %>%
  print_survival()

```

```{r}
# Cox model with MICE
# Run the mice
cox_anxiety_soft_cd_mice <- coxph_mice(
  Surv(time, DiseaseFlareYN) ~
    variable_name +
    IMD +
    Sex +
    age_decade +
    FC +
    flare_group +
    OverallControl +
    Smoke +
    frailty(SiteNo),
  data = data_survival_soft_cd,
  m = 20,
  printFlag = FALSE
)

# Fit the Cox
cox_anxiety_soft_cd <- with(cox_anxiety_soft_cd_mice,
                            coxph(
                              Surv(time, DiseaseFlareYN) ~
                                variable_name +
                                IMD +
                                Sex +
                                age_decade +
                                FC +
                                flare_group +
                                ns(OverallControl, df = 2) +
                                Smoke +
                                frailty(SiteNo)
                            )) %>%
  mice::pool()

cox_anxiety_soft_cd %>% 
  summary(conf.int = TRUE,
          conf.level = 0.95,
          exponentiate = TRUE)

```

##### Objective flare

```{r}
# Patient reported flare
# Kaplan Meier
data_survival_hard_cd %>%
  survfit(Surv(time, DiseaseFlareYN) ~ variable_name, data = .) %>%
  ggsurvplot(
    ., 
    data = data_survival_hard_cd, 
    conf.int = TRUE, 
    risk.table = TRUE,
    pval = TRUE,
    pval.method = TRUE,
    legend.title = 'HADS Anxiety Score',
    xlab = "Time from study recruitment (days)",
    ggtheme = theme_minimal())

# Cox model
cox_anxiety_hard_cd <- coxph(
  Surv(time, DiseaseFlareYN) ~
    variable_name +
    IMD +
    Sex +
    age_decade +
    FC +
    flare_group +
    OverallControl +
    Smoke +
    frailty(SiteNo),
  data = data_survival_hard_cd
)

cox_anxiety_hard_cd %>%
  print_survival()
```

```{r}
# Cox model with MICE
# Run the mice
cox_anxiety_hard_cd_mice <- coxph_mice(
  Surv(time, DiseaseFlareYN) ~
    variable_name +
    IMD +
    Sex +
    age_decade +
    FC +
    flare_group +
    OverallControl +
    Smoke +
    frailty(SiteNo),
  data = data_survival_hard_cd,
  m = 20,
  printFlag = FALSE
)

# Fit the Cox
cox_anxiety_hard_cd <- with(
  cox_anxiety_hard_cd_mice,
  coxph(
    Surv(time, DiseaseFlareYN) ~
      variable_name +
      IMD +
      Sex +
      age_decade +
      FC +
      flare_group +
      OverallControl +
      Smoke +
      frailty(SiteNo)
  )
) %>%
  mice::pool()

cox_anxiety_hard_cd %>% 
  summary(conf.int = TRUE,
          conf.level = 0.95,
          exponentiate = TRUE)
```
