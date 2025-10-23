library(tidyverse)
library(survival)


# Smoking included in the model using multiple imputation

# Run Diet to get data
# Using meat_sum as the example


cox <- coxph(Surv(softflare_time, softflare) ~
                            Sex + 
                            IMD +
                            age_decade + 
                            BMI +
                            FC +
                            Meat_sum + 
                            dqi_tot +
                            frailty(SiteNo),
                          data = flare.cd.df
)