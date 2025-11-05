
# Template for determining shapes of continuous variables

# Replace hads_for_analysis with the data name

## Survival Analysis

### Data Cleaning

```{r}
# Survival data
# Join HADS with flare data
# Patient reported
data_survival_soft <- hads_for_analysis %>% dplyr::inner_join(
  flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
  by = 'ParticipantNo'
) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)

# UC
data_survival_soft_uc <- data_survival_soft %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU')

# CD
data_survival_soft_cd <- data_survival_soft %>%
  dplyr::filter(diagnosis2 == 'CD')


# Hard flares
data_survival_hard <- hads_for_analysis %>% dplyr::inner_join(
  flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
  by = 'ParticipantNo'
) %>%
  dplyr::mutate(DiseaseFlareYN = hardflare, time = hardflare_time)

# UC
data_survival_hard_uc <- data_survival_hard %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU')

# CD
data_survival_hard_cd <- data_survival_hard %>%
  dplyr::filter(diagnosis2 == 'CD')

```


#### Full Cohort

##### Patient reported flare

```{r}

# Cox with continuous variables
# Age
cox_age_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    age_decade +
    frailty(SiteNo),
  data = data_survival_soft
)

# Spline for age
summon_lrt(cox_age_soft, remove = 'age_decade', add = 'ns(age_decade, df = 2)')
# No

# FC
cox_fc_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    FC +
    frailty(SiteNo),
  data = data_survival_soft
)

# Spline for FC
summon_lrt(cox_fc_soft, remove = 'FC', add = 'ns(FC, df = 2)')
# No

# OverallControl
cox_control_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    OverallControl +
    frailty(SiteNo),
  data = data_survival_soft
)

# Spline for control
summon_lrt(cox_control_soft, remove = 'OverallControl', add = 'ns(OverallControl, df = 2)')
# Yes

# OverallControl
cox_control_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    ns(OverallControl, df = 2) +
    frailty(SiteNo),
  data = data_survival_soft
)

# Spline for control
summon_lrt(cox_control_soft, remove = 'ns(OverallControl, df = 2)', add = 'ns(OverallControl, df = 3)')
# Yes

# OverallControl
cox_control_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    ns(OverallControl, df = 3) +
    frailty(SiteNo),
  data = data_survival_soft
)

# Spline for control
summon_lrt(cox_control_soft, remove = 'ns(OverallControl, df = 3)', add = 'ns(OverallControl, df = 4)')
# No, df = 3 is sufficient

```

```{r}
# Plot shapes
# Age
plot_continuous_hr(
  data = data_survival_soft, 
  model = cox_age_soft, 
  variable = 'age_decade')

# FC
plot_continuous_hr(
  data = data_survival_soft, 
  model = cox_fc_soft, 
  variable = 'FC')

# Control
plot_continuous_hr(
  data = data_survival_soft, 
  model = cox_control_soft, 
  variable = 'OverallControl',
  splineterm = 'ns(OverallControl, df = 3)')
```

##### Objective flare

```{r}

# Age
cox_age_hard <- coxph(
  Surv(time, DiseaseFlareYN) ~
    age_decade +
    frailty(SiteNo),
  data = data_survival_hard
)

# Spline for age
summon_lrt(cox_age_hard, remove = 'age_decade', add = 'ns(age_decade, df = 2)')
# No

# FC
cox_fc_hard <- coxph(
  Surv(time, DiseaseFlareYN) ~
    FC +
    frailty(SiteNo),
  data = data_survival_hard
)

# Spline for FC
summon_lrt(cox_fc_hard, remove = 'FC', add = 'ns(FC, df = 2)')
# No

# OverallControl
cox_control_hard <- coxph(
  Surv(time, DiseaseFlareYN) ~
    OverallControl +
    frailty(SiteNo),
  data = data_survival_hard
)

# Spline for control
summon_lrt(cox_control_hard, remove = 'OverallControl', add = 'ns(OverallControl, df = 2)')
# No
```

```{r}
# Plot shapes
# Age
plot_continuous_hr(
  data = data_survival_hard, 
  model = cox_age_hard, 
  variable = 'age_decade')

# FC
plot_continuous_hr(
  data = data_survival_hard, 
  model = cox_fc_hard, 
  variable = 'FC')

# Control
plot_continuous_hr(
  data = data_survival_soft, 
  model = cox_control_hard, 
  variable = 'OverallControl')
```

#### Ulcerative Colitis

##### Patient reported flare

```{r}
# Age
cox_age_soft_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    age_decade +
    frailty(SiteNo),
  data = data_survival_soft_uc
)

# Spline for age
summon_lrt(cox_age_soft_uc, remove = 'age_decade', add = 'ns(age_decade, df = 2)')
# No

# FC
cox_fc_soft_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    FC +
    frailty(SiteNo),
  data = data_survival_soft_uc
)

# Spline for FC
summon_lrt(cox_fc_soft_uc, remove = 'FC', add = 'ns(FC, df = 2)')
# No

# OverallControl
cox_control_soft_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    OverallControl +
    frailty(SiteNo),
  data = data_survival_soft_uc
)

# Spline for control
summon_lrt(cox_control_soft_uc, remove = 'OverallControl', add = 'ns(OverallControl, df = 2)')
# Yes

# OverallControl
cox_control_soft_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    ns(OverallControl, df = 2) +
    frailty(SiteNo),
  data = data_survival_soft_uc
)

summon_lrt(cox_control_soft_uc, remove = 'ns(OverallControl, df = 2)', add = 'ns(OverallControl, df = 3)')
# No, df = 2 is sufficient

```

```{r}
# Plot shapes
# Age
plot_continuous_hr(
  data = data_survival_soft_uc, 
  model = cox_age_soft_uc, 
  variable = 'age_decade')

# FC
plot_continuous_hr(
  data = data_survival_soft_uc, 
  model = cox_fc_soft_uc, 
  variable = 'FC')

# Control
plot_continuous_hr(
  data = data_survival_soft_uc, 
  model = cox_control_soft_uc, 
  variable = 'OverallControl',
  splineterm = 'ns(OverallControl, df = 2)')
```

##### Objective flare

```{r}

# Age
cox_age_hard_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    age_decade +
    frailty(SiteNo),
  data = data_survival_hard_uc
)

# Spline for age
summon_lrt(cox_age_hard_uc, remove = 'age_decade', add = 'ns(age_decade, df = 2)')
# Yes

# Refit
cox_age_hard_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    ns(age_decade, df = 2) +
    frailty(SiteNo),
  data = data_survival_hard_uc
)

# Test for df = 3
summon_lrt(cox_age_hard_uc, remove = 'ns(age_decade, df = 2)', add = 'ns(age_decade, df = 3)')
# Yes - very close to 0.05

# Refit
cox_age_hard_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    ns(age_decade, df = 3) +
    frailty(SiteNo),
  data = data_survival_hard_uc
)

# Test for df = 4
summon_lrt(cox_age_hard_uc, remove = 'ns(age_decade, df = 3)', add = 'ns(age_decade, df = 4)')
# Yes - again very close to 0.05

# Refit
cox_age_hard_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    ns(age_decade, df = 4) +
    frailty(SiteNo),
  data = data_survival_hard_uc
)

# Test for df = 5
summon_lrt(cox_age_hard_uc, remove = 'ns(age_decade, df = 4)', add = 'ns(age_decade, df = 5)')
# 5 df not needed, stay with 4

# FC
cox_fc_hard_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    FC +
    frailty(SiteNo),
  data = data_survival_hard_uc
)

# Spline for FC
summon_lrt(cox_fc_hard_uc, remove = 'FC', add = 'ns(FC, df = 2)')
# Yes, very close to 0.05

# Refit
cox_fc_hard_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    ns(FC, df = 2) +
    frailty(SiteNo),
  data = data_survival_hard_uc
)

# Test for df = 3
summon_lrt(cox_fc_hard_uc, remove = 'ns(FC, df = 2)', add = 'ns(FC, df = 3)')
# No, stay with df = 2


# OverallControl
cox_control_hard_uc <- coxph(
  Surv(time, DiseaseFlareYN) ~
    OverallControl +
    frailty(SiteNo),
  data = data_survival_hard_uc
)

# Spline for control
summon_lrt(cox_control_hard_uc, remove = 'OverallControl', add = 'ns(OverallControl, df = 2)')
# No

```

```{r}
# Plot shapes
# Age
plot_continuous_hr(
  data = data_survival_hard_uc, 
  model = cox_age_hard_uc, 
  variable = 'age_decade',
  splineterm = 'ns(age_decade, df = 4)')

# FC
plot_continuous_hr(
  data = data_survival_hard_uc, 
  model = cox_fc_hard_uc, 
  variable = 'FC',
  splineterm = 'ns(FC, df = 2)')

# Control
plot_continuous_hr(
  data = data_survival_hard_uc, 
  model = cox_control_hard_uc, 
  variable = 'OverallControl')
```

#### Crohn's Disease

##### Patient reported flare

```{r}

# Age
cox_age_soft_cd <- coxph(
  Surv(time, DiseaseFlareYN) ~
    age_decade +
    frailty(SiteNo),
  data = data_survival_soft_cd
)

# Spline for age
summon_lrt(cox_age_soft_cd, remove = 'age_decade', add = 'ns(age_decade, df = 2)')
# No

# FC
cox_fc_soft_cd <- coxph(
  Surv(time, DiseaseFlareYN) ~
    FC +
    frailty(SiteNo),
  data = data_survival_soft_cd
)

# Spline for FC
summon_lrt(cox_fc_soft_cd, remove = 'FC', add = 'ns(FC, df = 2)')
# No

# OverallControl
cox_control_soft_cd <- coxph(
  Surv(time, DiseaseFlareYN) ~
    OverallControl +
    frailty(SiteNo),
  data = data_survival_soft_cd
)

# Spline for control
summon_lrt(cox_control_soft_cd, remove = 'OverallControl', add = 'ns(OverallControl, df = 2)')
# Yes

# OverallControl
cox_control_soft_cd <- coxph(
  Surv(time, DiseaseFlareYN) ~
    ns(OverallControl, df = 2) +
    frailty(SiteNo),
  data = data_survival_soft_cd
)

summon_lrt(cox_control_soft_cd, remove = 'ns(OverallControl, df = 2)', add = 'ns(OverallControl, df = 3)')
# No, df = 2 is sufficient

```

```{r}
# Plot shapes
# Age
plot_continuous_hr(
  data = data_survival_soft_cd, 
  model = cox_age_soft_cd, 
  variable = 'age_decade')

# FC
plot_continuous_hr(
  data = data_survival_soft_cd, 
  model = cox_fc_soft_cd, 
  variable = 'FC')

# Control
plot_continuous_hr(
  data = data_survival_soft_cd, 
  model = cox_control_soft_cd, 
  variable = 'OverallControl',
  splineterm = 'ns(OverallControl, df = 2)')
```

##### Objective flare

```{r}

# Age
cox_age_hard_cd <- coxph(
  Surv(time, DiseaseFlareYN) ~
    age_decade +
    frailty(SiteNo),
  data = data_survival_hard_cd
)

# Spline for age
summon_lrt(cox_age_hard_cd, remove = 'age_decade', add = 'ns(age_decade, df = 2)')
# No

# FC
cox_fc_hard_cd <- coxph(
  Surv(time, DiseaseFlareYN) ~
    FC +
    frailty(SiteNo),
  data = data_survival_hard_cd
)

# Spline for FC
summon_lrt(cox_fc_hard_cd, remove = 'FC', add = 'ns(FC, df = 2)')
# No

# OverallControl
cox_control_hard_cd <- coxph(
  Surv(time, DiseaseFlareYN) ~
    OverallControl +
    frailty(SiteNo),
  data = data_survival_hard_cd
)

# Spline for control
summon_lrt(cox_control_hard_cd, remove = 'OverallControl', add = 'ns(OverallControl, df = 2)')
# No
```

```{r}
# Plot shapes
# Age
plot_continuous_hr(
  data = data_survival_hard_cd, 
  model = cox_age_hard_cd, 
  variable = 'age_decade')

# FC
plot_continuous_hr(
  data = data_survival_hard_cd, 
  model = cox_fc_hard_cd, 
  variable = 'FC')

# Control
plot_continuous_hr(
  data = data_survival_hard_cd, 
  model = cox_control_hard_cd, 
  variable = 'OverallControl')
```