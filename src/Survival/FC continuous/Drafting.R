

model <- coxph(
  Surv(softflare_time, softflare) ~
    Sex +
    IMD +
    age_decade +
    BMI +
    ns(FC, df = 2) +
    Meat_sum +
    frailty(SiteNo),
  data = data
) 


# Complete case
data = flare.uc.df %>%
  dplyr::select(tidyselect::all_of(all.vars(terms(model)))) %>%
  dplyr::filter(!dplyr::if_any(.cols = everything(), .fns = is.na)) 

variable = "Meat_sum"

# Extract means from the model itself

data_cox <- model$means %>% 
  tibble::enframe(
    name = 'variable',
    value = 'value'
  ) %>%
  dplyr::mutate(
    beta = model$coefficients
  )

# Calculate linear predictor at reference
data_cox %>%
  dplyr::mutate(lp = value * beta) %>%
  dplyr::pull(lp) %>%
  sum()

reference_variables <-  all.vars(delete.response(terms(model))) %>%
  tibble(x = .) %>%
  dplyr::filter(
    x != variable,
    x != "SiteNo"
  ) %>%
  dplyr::pull(x)

# I can recover this value, stored in the constant attr 
summon_reference_values(
  data, variables = reference_variables
) %>%
  dplyr::mutate(Meat_sum = 40.7) %>%
  predict(model, newdata = . , type = 'terms')



# Calculate linear predictor
# At reference

values_ref <- summon_reference_values(
  data, variables = reference_variables
) %>%
  dplyr::mutate(Meat_sum = 40.1)


# At wanted value
values <- summon_reference_values(
  data, variables = reference_variables
) %>%
  dplyr::mutate(Meat_sum = 100)

# Variance
values_mm <- model.matrix(~ Sex +
                            IMD +
                            age_decade +
                            BMI +
                            ns(FC, df = 2) +
                            Meat_sum, data = values) %>%
  # Remove intercept
  {.[, -1]}

values_ref_mm <- model.matrix(~ Sex +
                                IMD +
                                age_decade +
                                BMI +
                                ns(FC, df = 2) +
                                Meat_sum, data = values_ref) %>%
  # Remove intercept
  {.[, -1]}


# Fit
(values_mm - values_ref_mm)%*%coef(model)

# SE
(t(values_mm - values_ref_mm) %*% vcov(model)) %*% (values_mm - values_ref_mm) %>% as.numeric() %>% sqrt()

# Compare to built in
values %>%
  predict(model, newdata = . , type = 'lp', se = TRUE)


# When there is splines the reference value is the mean of ns(data$FC, df = 2)
ns <- ns(data$FC, df = 2)
ns %>% tibble() %>% colMeans(na.rm = TRUE)
model$means
# So, If i want to plot the HR for FC i need to know the new value of the spline
predict(ns, newx = 6)
# Interesting

values_ref_mm <- model$means

values_mm <- model$means

values_mm[stringr::str_detect(names(values_mm), 'FC')] = predict(ns, newx = 6) %>% as.numeric()

# Fit
(values_mm - values_ref_mm)%*%coef(model)

# SE
(t(values_mm - values_ref_mm) %*% vcov(model)) %*% (values_mm - values_ref_mm) %>% as.numeric() %>% sqrt()


# Compare with built in
summon_reference_values(
  data, variables = all.vars(delete.response(terms(model))) %>%
    tibble(x = .) %>%
    dplyr::filter(
      x != 'FC',
      x != "SiteNo"
    ) %>%
    dplyr::pull(x)
) %>%
  dplyr::mutate(FC = 6) %>%
  predict(model, newdata = . , type = 'lp', se = TRUE)


# So now I can calculate the fit and se by hand

model = model
data = data

variable = 'FC'

# Recreate the spline

spline <- data %$% ns(FC, df = 2)
# New FC value, e.g., 6
spline_predict <- predict(spline, 6) %>% as.numeric()
# Need names
names(spline_predict) <- c('ns(FC, df = 2)1', 'ns(FC, df = 2)2')
spline_predict

# Reference values from the model
reference <- model$means

newdata <- reference
# Change the FC value
newdata[c('ns(FC, df = 2)1', 'ns(FC, df = 2)2')] = predict(spline, 6) %>% as.numeric()

# Calculate fit and se
# Fit
(newdata - reference)%*%coef(model)

# SE
(t(newdata - reference) %*% vcov(model)) %*% (newdata - reference) %>% as.numeric() %>% sqrt()


# I have recovered the built in behavior- huzzah

data = data
model = model
variable = 'FC'
term = 'ns(FC, df = 2)'

# I could let term be null by default and only if its a spline do i need to input it
# Data must be the same data used to fit the model - NAs do matter

# New FC value to calculate HR at
value = 6

# Recreate the spline
spline <- with(data, eval(parse(text = term)))
  
# Predict from the spline at new FC value
new_spline_values <- predict(spline, newx = value) %>% as.numeric()

# Reference values from cox model
ref_values <- model$means

# New values at which we want to evaluate the HR
new_values <- ref_values

# Apply the new spline values
# Positions of the spline terms
pos_spline <- stringr::str_detect(names(new_values), variable)

new_values[pos_spline] = new_spline_values
# Ordering of the spline terms should be the same

# Calculate fit and se
# Fit
pred <- (new_values - ref_values)%*%coef(model)

# SE
se <- (new_values - ref_values) %*% vcov(model) %*% (new_values - ref_values) %>% 
  as.numeric() %>% 
  sqrt()

# Exponentiate to get hazard ratios
pred <- exp(pred)

se <- se*sqrt(pred)  # Taylor series approximation used in survival package


plot_continuous_hr <- function(data, model, variable, splineterm = NULL){
  
  # Plotting continuous Hazard Ratio curves
  # Data is the same data used to fit the Cox model
  # model is a Cox model
  # variable is the variable we are plotting
  # splineterm is the spline used in the model, if there is one
  
  
  # Range of the variable to plot
  variable_range <- data %>%
    dplyr::pull(variable) %>%
    quantile(., probs = seq(0, 0.9, 0.01), na.rm = TRUE) %>%
    unname() %>%
    unique()
  
  # If there is a spline
  if (!is.null(splineterm)){
    
    # Recreate the spline
    spline <- with(data, eval(parse(text = term)))
    
    # Get the spline terms for the variable values we are plotting
    spline_values <- predict(spline, newx = variable_range)
    
    # Get the reference values of the variables that were used to fit the Cox model
    X_ref <- model$means
    
    # New values to calculate the HR at 
    # Take the ref values and vary our variable of interest
    
    # Number of values
    n_values <- variable_range %>% length()
    
    X_ref <- matrix(rep(X_ref, n_values), nrow = n_values, byrow = TRUE)
    # Fix the column names
    colnames(X_ref) <- names(model$means)
    
    # Now add our new variable values
    X_new <- X_ref
    # Identify the correct columns
    column_pos <- stringr::str_detect(colnames(X_new), variable)
    # Add the values - ordering of columns and model term names should be preserved
    X_new[,column_pos] <- spline_values
    
    # Calculate the linear predictor and se
    # Method directly from predict.coxph from the survival package
    lp <- (X_new - X_ref)%*%coef(model) 
    
    lp_se <- rowSums((X_new - X_ref)%*%vcov(model)*(X_new - X_ref))
    
  } else {
    
    # Get the reference values of the variables that were used to fit the Cox model
    X_ref <- model$means
    
    # New values to calculate the HR at 
    # Take the ref values and vary our variable of interest
    
    # Number of values
    n_values <- variable_range %>% length()
    
    X_ref <- matrix(rep(X_ref, n_values), nrow = n_values, byrow = TRUE)
    # Fix the column names
    colnames(X_ref) <- names(model$means)
    
    # Now add our new variable values
    X_new <- X_ref
    
    # Add the values
    X_new[,variable] <- variable_range
    
    # Calculate the linear predictor and se
    # Method directly from predict.coxph from the survival package
    lp <- (X_new - X_ref)%*%coef(model) %>% as.numeric() 
    
    lp_se <- rowSums((X_new - X_ref)%*%vcov(model)*(X_new - X_ref)) %>% as.numeric() 
    
  }
  
  # Store results in a tibble
  data_pred <- tibble(
    !!sym(variable) := variable_range,
    p = lp,
    se = lp_se
  )
  
  # Plot
  data_pred %>%
    ggplot(aes(
      x = !!rlang::sym(variable),
      y = exp(p),
      ymin = exp(p - 1.96 * se),
      ymax = exp(p + 1.96 * se)
    )) +
    geom_point() +
    geom_line() +
    geom_ribbon(alpha = 0.2) +
    ylab("HR")
  
  
    
}
  
  
  
  
}