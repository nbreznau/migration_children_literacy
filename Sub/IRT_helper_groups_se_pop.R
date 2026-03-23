# To calculate NFE12 (non IRT variables)
svy_pct_yes <- function(varname, des, yes_value = 1) {
  # Check variable exists
  if (!(varname %in% names(des$variables))) return(NA_real_)

  vals <- des$variables[[varname]]
  
  # Make sure NAs stay in the data as not 1's
  target_vec <- ifelse(!is.na(vals) & vals == yes_value, 1, 0)
  
  # Add this recoded vector back to a temporary design object
  des_tmp <- des
  des_tmp$variables$.target_ale <- target_vec
  
  # Calculate weighted mean (no na.rm needed now as we removed NAs)
  res <- tryCatch(
    svymean(~.target_ale, des_tmp), 
    error = function(e) return(NULL)
  )
  
  if (is.null(res)) return(NA_real_)
  
  # Return as percentage
  return(as.numeric(res)[1] * 100) 
}

calc_groups_pv <- function(df1_rep, df2_rep, pvlit, pvnum, group_vars,
                           assign_to_env = TRUE,
                           target_env = .GlobalEnv) {
  library(survey)
  
  # safe svymean that returns both mean and within-imputation variance (SE^2)
  svymean_mean_var <- function(var, des) {
    if (is.na(var) || is.null(var) || !nzchar(var)) return(c(NA_real_, NA_real_))
    f <- as.formula(paste0("~", var))
    fit <- tryCatch(svymean(f, des, na.rm = TRUE), error = function(e) NULL)
    if (is.null(fit)) return(c(NA_real_, NA_real_))
    c(as.numeric(coef(fit))[1], as.numeric(SE(fit))[1]^2)
  }
  
  # helper: proportion below a cutoff for a given PV (binary mean)
  svyprop_lt_mean_var <- function(var, des, cutoff = 226) {
    if (is.na(var) || is.null(var) || !nzchar(var)) return(c(NA_real_, NA_real_))
    if (!var %in% names(des$variables)) return(c(NA_real_, NA_real_))
    vals <- des$variables[[var]]
    tmp  <- ifelse(is.na(vals), NA_real_, as.numeric(vals < cutoff))
    tmpname <- paste0(".tmp_", var)
    des2 <- des
    des2$variables[[tmpname]] <- tmp
    fit <- tryCatch(svymean(reformulate(tmpname), des2, na.rm = TRUE), error = function(e) NULL)
    if (is.null(fit)) return(c(NA_real_, NA_real_))
    c(as.numeric(coef(fit))[1], as.numeric(SE(fit))[1]^2)
  }
  
  # Rubin's rules combiner for (mean, within-var) pairs across PVs
  rubin_combine <- function(mv_mat) {
    m1 <- mv_mat[1, ]
    v1 <- mv_mat[2, ]
    ok <- !(is.na(m1) | is.na(v1))
    if (!any(ok)) return(list(mean = NA_real_, se = NA_real_))
    M <- sum(ok)
    mean_val <- mean(m1[ok])
    W <- mean(v1[ok])
    B <- if (M > 1) stats::var(m1[ok]) else 0
    se_rubin <- sqrt(W + (1 + 1/M) * B)
    list(mean = mean_val, se = se_rubin)
  }
  
  pv_stats_one_domain <- function(des, pv_names) {
    if (is.null(pv_names) || length(pv_names) == 0 || all(is.na(pv_names))) {
      return(list(mean = NA_real_, se = NA_real_))
    }
    mv <- vapply(pv_names, function(v) svymean_mean_var(v, des), numeric(2))
    rubin_combine(mv)
  }
  
  # % in Levels 0/1 (score < 226); return percent only (no SE)
  pv_pct_lit01 <- function(des, pv_names, cutoff = 226) {
    if (is.null(pv_names) || length(pv_names) == 0 || all(is.na(pv_names))) {
      return(NA_real_)
    }
    mv <- vapply(pv_names, function(v) svyprop_lt_mean_var(v, des, cutoff), numeric(2))
    res <- rubin_combine(mv)
    100 * res$mean
  }
  
  pv_stats_all <- function(des) {
    lit   <- pv_stats_one_domain(des, pvlit)
    num   <- pv_stats_one_domain(des, pvnum)
    pct01 <- pv_pct_lit01(des, pvlit, cutoff = 226)
    list(
      mean_lit = lit$mean,   se_lit = lit$se,
      mean_num = num$mean,   se_num = num$se,
      pct_lit01 = pct01
    )
  }
  
  out <- vector("list", length(group_vars))
  names(out) <- paste0("g", seq_along(group_vars))
  
  for (i in seq_along(group_vars)) {
    gname <- names(out)[i]
    v     <- group_vars[[i]]
    
    # subset to v == 1 (and non-missing)
    take1 <- function(des) {
      if (!v %in% names(des$variables)) return(subset(des, FALSE))
      idx <- des$variables[[v]] == 1 & !is.na(des$variables[[v]])
      tryCatch(subset(des, idx), error = function(e) subset(des, FALSE))
    }
    
    des1_g <- take1(df1_rep)
    des2_g <- take1(df2_rep)
    
    s1_ale <- svy_pct_yes("NFE12", des1_g)
    s2_ale <- svy_pct_yes("NFE12", des2_g)
    
    s1 <- pv_stats_all(des1_g)
    s2 <- pv_stats_all(des2_g)
    
    out[[gname]] <- list(
      varname          = v,
      mean_lit_df1     = s1$mean_lit,  se_lit_df1   = s1$se_lit,
      mean_num_df1     = s1$mean_num,  se_num_df1   = s1$se_num,
      pct_lit01_df1    = s1$pct_lit01,
      mean_lit_df2     = s2$mean_lit,  se_lit_df2   = s2$se_lit,
      mean_num_df2     = s2$mean_num,  se_num_df2   = s2$se_num,
      pct_lit01_df2    = s2$pct_lit01,
      pct_ale_df1      = s1_ale,
      pct_ale_df2      = s2_ale
    )
    
    if (assign_to_env) {
      # literacy PV means/SEs
      if (!is.null(pvlit) && length(pvlit) > 0 && !all(is.na(pvlit))) {
        assign(paste0(gname, "_mean_lit_df1"),  s1$mean_lit,  envir = target_env)
        assign(paste0(gname, "_se_lit_df1"),    s1$se_lit,    envir = target_env)
        assign(paste0(gname, "_mean_lit_df2"),  s2$mean_lit,  envir = target_env)
        assign(paste0(gname, "_se_lit_df2"),    s2$se_lit,    envir = target_env)
        # NEW.. % in Levels 0/1 (no SE)
        assign(paste0(gname, "_pct_lit01_df1"), s1$pct_lit01, envir = target_env)
        assign(paste0(gname, "_pct_lit01_df2"), s2$pct_lit01, envir = target_env)
        # NEW - add NFE12
        assign(paste0(gname, "_pct_ale_df1"), s1_ale, envir = target_env)
        assign(paste0(gname, "_pct_ale_df2"), s2_ale, envir = target_env)
      }
      # numeracy PV means/SEs
      if (!is.null(pvnum) && length(pvnum) > 0 && !all(is.na(pvnum))) {
        assign(paste0(gname, "_mean_num_df1"), s1$mean_num, envir = target_env)
        assign(paste0(gname, "_se_num_df1"),   s1$se_num,   envir = target_env)
        assign(paste0(gname, "_mean_num_df2"), s2$mean_num, envir = target_env)
        assign(paste0(gname, "_se_num_df2"),   s2$se_num,   envir = target_env)
      }
    }
  }
  
  invisible(out)
}
