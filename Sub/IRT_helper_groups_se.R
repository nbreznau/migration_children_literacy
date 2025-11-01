calc_groups_pv <- function(df1_rep, df2_rep, pvlit, pvnum, group_vars, assign_to_env = TRUE) {
  library(survey)
  
  # safe svymean that returns both mean and within-imputation variance (SE^2)
  svymean_mean_var <- function(var, des) {
    if (is.na(var) || is.null(var) || !nzchar(var)) return(c(NA_real_, NA_real_))
    f <- as.formula(paste0("~", var))
    fit <- tryCatch(svymean(f, des, na.rm = TRUE), error = function(e) NULL)
    if (is.null(fit)) return(c(NA_real_, NA_real_))
    c(as.numeric(coef(fit))[1], as.numeric(SE(fit))[1]^2)
  }
  
  # Rubin: combine across PVs -> mean and SE
  pv_stats_one_domain <- function(des, pv_names) {
    if (is.null(pv_names) || length(pv_names) == 0 || all(is.na(pv_names))) {
      return(list(mean = NA_real_, se = NA_real_))
    }
    mv <- vapply(pv_names, function(v) svymean_mean_var(v, des), numeric(2))
    m1 <- mv[1, ]
    v1 <- mv[2, ]
    ok <- !(is.na(m1) | is.na(v1))
    if (!any(ok)) return(list(mean = NA_real_, se = NA_real_))
    
    M <- sum(ok)
    mean_val <- mean(m1[ok])
    W <- mean(v1[ok])                      # within-imputation variance
    B <- if (M > 1) stats::var(m1[ok]) else 0  # between-imputation variance
    se_rubin <- sqrt(W + (1 + 1/M) * B)
    
    list(mean = mean_val, se = se_rubin)
  }
  
  pv_stats_both <- function(des) {
    lit <- pv_stats_one_domain(des, pvlit)
    num <- pv_stats_one_domain(des, pvnum)
    list(
      mean_lit = lit$mean, se_lit = lit$se,
      mean_num = num$mean, se_num = num$se
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
    
    s1 <- pv_stats_both(des1_g)
    s2 <- pv_stats_both(des2_g)
    
    out[[gname]] <- list(
      varname       = v,
      mean_lit_df1  = s1$mean_lit, se_lit_df1  = s1$se_lit,
      mean_num_df1  = s1$mean_num, se_num_df1  = s1$se_num,
      mean_lit_df2  = s2$mean_lit, se_lit_df2  = s2$se_lit,
      mean_num_df2  = s2$mean_num, se_num_df2  = s2$se_num
    )
    
    if (assign_to_env) {
      if (!is.null(pvlit) && length(pvlit) > 0 && !all(is.na(pvlit))) {
        assign(paste0(gname, "_mean_lit_df1"), s1$mean_lit, parent.frame())
        assign(paste0(gname, "_se_lit_df1"),   s1$se_lit,   parent.frame())
        assign(paste0(gname, "_mean_lit_df2"), s2$mean_lit, parent.frame())
        assign(paste0(gname, "_se_lit_df2"),   s2$se_lit,   parent.frame())
      }
      if (!is.null(pvnum) && length(pvnum) > 0 && !all(is.na(pvnum))) {
        assign(paste0(gname, "_mean_num_df1"), s1$mean_num, parent.frame())
        assign(paste0(gname, "_se_num_df1"),   s1$se_num,   parent.frame())
        assign(paste0(gname, "_mean_num_df2"), s2$mean_num, parent.frame())
        assign(paste0(gname, "_se_num_df2"),   s2$se_num,   parent.frame())
      }
    }
  }
  
  invisible(out)
}
