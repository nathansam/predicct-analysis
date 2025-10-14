library(tidyver)
library(magrittr)

# Testing missingness in FC tests.

# HADS
# Run HADS.qmd but skip the removal of missing FC tests
data <- hads_for_analysis

# Missing FC flag
data %<>%
  dplyr::mutate(
    cat_missing_flag = dplyr::case_when(
      is.na(cat) ~ TRUE,
      .default = FALSE
    )
  )

# Anxiety - only need to do one as it just means patients don't count as 2 people
data_anxiety <- data %>%
  dplyr::filter(hads_type == 'anxiety_hads')

# chi square with missing flag as the dependent
independent = c('diagnosis2',
                'AgeGroup',
                'Sex',
                'flare_group')

summon_chisq_test(
  data = data_anxiety, 
  dependent = 'cat_missing_flag', 
  independent = independent) 

# age as a continuous variable
data_anxiety %>%
  ggplot(aes(x = age, colour = cat_missing_flag)) +
  geom_density()

t_test(data = data_anxiety, age ~ cat_missing_flag)

# Don't see any associations so assume MCAR?


# Other variables
# Run the data cleaning without removing FC
# Rename the variable of choice

#data <- ETOH
#data <- exercise
#data <- lifeevents
#data <- phq_clean
#data <- psqi2
data <- QOL


# Missing FC flag
data %<>%
  dplyr::mutate(
    cat_missing_flag = dplyr::case_when(
      is.na(cat) ~ TRUE,
      .default = FALSE
    )
  )

# chi square with missing flag as the dependent
independent = c('diagnosis2',
                'AgeGroup',
                'Sex',
                'flare_group')

summon_chisq_test(
  data = data, 
  dependent = 'cat_missing_flag', 
  independent = independent) 

# age as a continuous variable
data %>%
  ggplot(aes(x = age, colour = cat_missing_flag)) +
  geom_density()

t_test(data = data, age ~ cat_missing_flag)




