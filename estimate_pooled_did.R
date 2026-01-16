estimate_pooled_did <- function(
    data,
    id_var,
    outcome_var,
    cum_out_var,
    time_var,
    treated_var,
    date_treated_var,
    control_vars ,
    estimand = "ATT",
    balancing_method = "ebal",
    lead=450, 
    followup=450
) {
  
  
  
  # Validate required variables exist
  required_vars <- c(id_var, outcome_var, cum_out_var, time_var, 
                     treated_var, date_treated_var, control_vars)
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  # Create data.table copy for manipulation
  dt <- as.data.table(data)
  
  # Pre-filter: remove rows with NA or infinite outcomes
  dt <- dt[!is.na(get(outcome_var)) & is.finite(get(outcome_var))]
  
  # Get unique treatment dates (excluding NA)
  treatment_dates <- dt[!is.na(get(date_treated_var)), 
                        unique(get(date_treated_var))]
  
  # Helper function for processing each treatment date
  process_cohort <- function(treat_date, dt_full, 
                             id_var, outcome_var, cum_out_var, 
                             time_var, treated_var, date_treated_var,
                             control_vars, estimand, balancing_method) {
    
    # Subset data for current cohort: treated at this date OR never treated
    cohort_dt <- dt_full[get(date_treated_var) == treat_date | get(treated_var) == 0]
    
    # subset for lead and lags
    cohort_dt <-  cohort_dt[get(time_var)>treat_date-lead & get(time_var)<treat_date + followup]
    
    # Select relevant columns
    keep_vars <- c(id_var, control_vars, time_var, cum_out_var, 
                   outcome_var, date_treated_var, treated_var)
    cohort_dt <- cohort_dt[, ..keep_vars]
    
    # Filter to pre-treatment period for balancing
    pre_treat_dt <- cohort_dt[get(time_var) <= treat_date]
    
    # Skip if no pre-treatment data
    if (nrow(pre_treat_dt) == 0) {
      warning(sprintf("No pre-treatment data for cohort treated on %s", 
                      as.character(treat_date)))
      return(NULL)
    }
    
    # Reshape to wide format for cumulative outcomes
    id_formula <- paste(id_var, "+", 
                        paste(control_vars, collapse = " + "), "+", 
                        treated_var)
    
    wide_dt <- dcast(
      pre_treat_dt,
      as.formula(paste(id_formula, "~", time_var)),
      value.var = cum_out_var
    )
    
    # Clean column names
    wide_dt<-janitor::clean_names(wide_dt)
    # Remove NA rows efficiently
    wide_dt <- na.omit(wide_dt)
    
    # Skip if insufficient data
    if (nrow(wide_dt) < 2) {
      warning(sprintf("Insufficient data for balancing for cohort treated on %s", 
                      as.character(treat_date)))
      return(NULL)
    }
    
    # Create formula for balancing
    covar_names <- setdiff(names(wide_dt), c(id_var, treated_var))
    
    if (length(covar_names) == 0) {
      warning(sprintf("No covariates for balancing for cohort treated on %s", 
                      as.character(treat_date)))
      return(NULL)
    }
    
    form <- as.formula(paste(treated_var, "~", paste(covar_names, collapse = " + ")))
    
    # Calculate balancing weights
    tryCatch({
      suppressWarnings(
      weights <- weightit(form, data = wide_dt, 
                          estimand = estimand, method = balancing_method)$weights
      )
    }, error = function(e) {
      warning(sprintf("Weighting failed for cohort treated on %s: %s", 
                      as.character(treat_date), e$message))
      return(NULL)
    })
    
    wide_dt[, wt := weights]
    
    # # Merge weights back to full cohort data
    weights_dt<- wide_dt[, .SD, .SDcols = c(id_var, "wt")]
    # cohort_dt <- merge(cohort_dt, weights_dt, by = id_var, all.x = TRUE)
    # 
    # Set keys for binary search
    setkeyv(cohort_dt, id_var)
    setkeyv(weights_dt, id_var)
    
    # Binary join - much faster than merge()
    cohort_dt[weights_dt, wt := i.wt, on = id_var]
    
    # Assign weight = 1 for individuals without weights
    cohort_dt[is.na(wt), wt := 1]
    
    # Create DID indicators
    cohort_dt[, `:=`(
      did = as.numeric(get(time_var) >= treat_date & get(treated_var) == 1),
      after = as.numeric(get(time_var) >= treat_date),
      date_treated_dt = as.IDate(treat_date)
    )]
    
    # Count treated observations post-treatment
    tau_wt <- cohort_dt[get(treated_var) == 1 & get(time_var) >= treat_date, .N]
    
    # Calculate weighted outcome means by group
    group_vars <- c(time_var, treated_var, "after", "did", "date_treated_dt")
    
    ag_data <- cohort_dt[, .(
      outcome= weighted.mean(get(outcome_var), w = wt, na.rm = TRUE),
      num = .N
    ), by = group_vars]
    
    setnames(ag_data, old="outcome", new=paste0(outcome_var))
    ag_data[, tau_wt := tau_wt]
    
    # Estimate DID coefficient
    
    tryCatch({
      
      did_formula <- as.formula(paste(outcome_var, "~ after *", treated_var))
      
      did_coef <- coef(lm(did_formula, data = ag_data))[4]
      ag_data[, estimate := did_coef]
    }, error = function(e) {
      warning(sprintf("Regression failed for cohort treated on %s: %s", 
                      as.character(treat_date), e$message))
      ag_data[, estimate := NA_real_]
    })
    
    return(ag_data)
  }
  
  # Process all treatment dates using lapply
  results_list <- lapply(
    treatment_dates,
    function(t_date) {
      process_cohort(
        treat_date = t_date,
        dt_full = dt,
        id_var = id_var,
        outcome_var = outcome_var,
        cum_out_var = cum_out_var,
        time_var = time_var,
        treated_var = treated_var,
        date_treated_var = date_treated_var,
        control_vars = control_vars,
        estimand = estimand,
        balancing_method = balancing_method
      )
    }
  )
  
  # Filter out NULL results
  valid_results <- !sapply(results_list, is.null)
  if (sum(valid_results) == 0) {
    stop("All cohorts failed during processing. Check warnings for details.")
  }
  
  results_list <- results_list[valid_results]
  
  # Combine all results
  pooled_data <- rbindlist(results_list, use.names = TRUE, fill = TRUE)
  
  if (nrow(pooled_data) == 0) {
    warning("No valid data after processing all cohorts")
    return(list(
      aggregated_data = data.table(),
      estimates = data.table()
    ))
  }
  
  # Extract unique estimates by treatment date
  est_table <- pooled_data[
    get(treated_var) == 1 & !is.na(estimate),
    .(date_treated_dt = unique(date_treated_dt),
      estimate = unique(estimate),
      tau_wt = unique(tau_wt)),
    by = date_treated_dt
  ]
  
  # Calculate overall weighted estimate
  if (nrow(est_table) > 0) {
    est_table[, overall := weighted.mean(estimate, w = tau_wt, na.rm = TRUE)]
    setorder(est_table, date_treated_dt)
  } else {
    est_table[, overall := NA_real_]
  }
  
  # Return results
  return(list(
    aggregated_data = pooled_data,
    estimates = est_table,
    n_cohorts_processed = sum(valid_results),
    n_cohorts_total = length(treatment_dates)
  ))
}

