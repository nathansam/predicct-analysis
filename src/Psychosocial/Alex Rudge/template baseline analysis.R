
# Termplate for baseline analysis

# Need to replace 'variable_name' with the exposure of interest
# Replace hads_for_analysis with the data name


# Add extra data cleaning for continuous variables

# For load data chunk
IBD_C <- readRDS(paste0(chiara, "IBD_C.RDS"))

# For data cleaning chunk

# IBD control scores
hads_for_analysis %<>%
  dplyr::left_join(
    IBD_C %>% dplyr::select(
      ParticipantNo,
      control_score,
      OverallControl,
      control_8,
      control_grouped,
      vas_control
    ),
    by = "ParticipantNo"
  )

#change control_grouped and vas_control from character into factors
hads_for_analysis %<>% 
  dplyr::mutate(control_grouped = factor(control_grouped))

hads_for_analysis %<>% 
  dplyr::mutate(vas_control = factor(vas_control))


# Age in decades
hads_for_analysis %<>%
  dplyr::mutate(
    age_decade = age/10
  )


# FC maximum detectable value is 1250
hads_for_analysis %<>%
  dplyr::mutate(
    FC = dplyr::case_when(
      FC > 1250 ~ 1250,
      .default = FC
    )
  )

# Log transform FC due to extreme positive skew
hads_for_analysis %<>%
  dplyr::mutate(
    FC = log(FC)
  )







#### Full Cohort

```{r}

data_baseline <- hads_for_analysis

# Independent variables
independent = c('diagnosis2',
                'age',
                'Sex',
                'flare_group',
                'FC',
                'Smoke',
                'OverallControl')

# Dependent variable is variable_name

# Statistical tests for each independent variable with variable_name
# With p-values adjusted for multiple testing using holm correction.
statistical_tests <- summon_statistical_test(
  data = data_baseline, 
  dependent = 'variable_name', 
  independent = independent)

statistical_tests

# Plots
summon_baseline_plot_discrete(
  data = data_baseline,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'diagnosis2'
)

summon_baseline_plot_discrete(
  data = data_baseline,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'Sex'
)

summon_baseline_plot_discrete(
  data = data_baseline,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'flare_group'
)

summon_baseline_plot_discrete(
  data = data_baseline,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'Smoke'
)

summon_baseline_plot_continuous(
  data = data_baseline,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'age'
)

summon_baseline_plot_continuous(
  data = data_baseline,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'FC'
)

summon_baseline_plot_continuous(
  data = data_baseline,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'OverallControl'
)
```

#### Ulcerative Colitis/IBD Unclassified

```{r}
# Select anxiety data and UC patients
data_baseline_uc <- hads_for_analysis %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU')

# Independent variables
independent = c('age',
                'Sex',
                'flare_group',
                'FC',
                'Smoke',
                'OverallControl')

# Dependent variable is variable_name

# Statistical tests for each independent variable with variable_name
# With p-values adjusted for multiple testing using holm correction.
statistical_tests <- summon_statistical_test(
  data = data_baseline_uc, 
  dependent = 'variable_name', 
  independent = independent)

statistical_tests

# Plots
summon_baseline_plot_discrete(
  data = data_baseline_uc,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'Sex'
)

summon_baseline_plot_discrete(
  data = data_baseline_uc,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'flare_group'
)

summon_baseline_plot_discrete(
  data = data_baseline_uc,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'Smoke'
)

summon_baseline_plot_continuous(
  data = data_baseline_uc,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'age'
)

summon_baseline_plot_continuous(
  data = data_baseline_uc,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'FC'
)

summon_baseline_plot_continuous(
  data = data_baseline_uc,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'OverallControl'
)
```

#### Crohn's Disease

```{r}
# Select anxiety data and CD patients
data_baseline_cd <- hads_for_analysis %>%
  dplyr::filter(diagnosis2 == 'CD')

# Dependent variable is variable_name

# Statistical tests for each independent variable with variable_name
# With p-values adjusted for multiple testing using holm correction.
statistical_tests <- summon_statistical_test(
  data = data_baseline_cd, 
  dependent = 'variable_name', 
  independent = independent)

statistical_tests

# Plots
summon_baseline_plot_discrete(
  data = data_baseline_cd,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'Sex'
)

summon_baseline_plot_discrete(
  data = data_baseline_cd,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'flare_group'
)

summon_baseline_plot_discrete(
  data = data_baseline_cd,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'Smoke'
)

summon_baseline_plot_continuous(
  data = data_baseline_cd,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'age'
)

summon_baseline_plot_continuous(
  data = data_baseline_cd,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'FC'
)

summon_baseline_plot_continuous(
  data = data_baseline_cd,
  stat_tests = statistical_tests,
  dependent = 'variable_name',
  independent = 'OverallControl'
)
```
