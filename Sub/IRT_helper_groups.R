calc_groups_pv <- function(df1_rep, df2_rep, pvlit, pvnum, group_vars, assign_to_env = TRUE) {
  library(survey)
  
  svymean_coef <- function(var, des) {
    if (is.na(var) || is.null(var) || !nzchar(var)) return(NA_real_)
    f <- as.formula(paste0("~", var))
    out <- tryCatch(coef(svymean(f, des, na.rm = TRUE)), error = function(e) NA_real_)
    as.numeric(out)[1]
  }
  
  pv_stats_one_domain <- function(des, pv_names){
    if (is.null(pv_names) || length(pv_names) == 0 || all(is.na(pv_names))) {
      return(list(mean = NA_real_, sd = NA_real_))
    }
    m1 <- vapply(pv_names, function(v) svymean_coef(v, des), numeric(1))
    m2 <- vapply(pv_names, function(v) svymean_coef(paste0("I(", v, "^2)"), des), numeric(1))
    if (all(is.na(m1)) || all(is.na(m2))) return(list(mean = NA_real_, sd = NA_real_))
    mean_val <- mean(m1, na.rm = TRUE)
    var_val  <- mean(m2, na.rm = TRUE) - mean_val^2
    list(mean = mean_val, sd = sqrt(max(var_val, 0)))
  }
  
  pv_stats_both <- function(des){
    lit <- pv_stats_one_domain(des, pvlit)
    num <- pv_stats_one_domain(des, pvnum)
    list(mean_lit = lit$mean, sd_lit = lit$sd, mean_num = num$mean, sd_num = num$sd)
  }
  
  out <- vector("list", length(group_vars))
  names(out) <- paste0("g", seq_along(group_vars))
  
  for (i in seq_along(group_vars)) {
    gname <- names(out)[i]
    v     <- group_vars[[i]]
    
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
      mean_lit_df1  = s1$mean_lit, sd_lit_df1  = s1$sd_lit,
      mean_num_df1  = s1$mean_num, sd_num_df1  = s1$sd_num,
      mean_lit_df2  = s2$mean_lit, sd_lit_df2  = s2$sd_lit,
      mean_num_df2  = s2$mean_num, sd_num_df2  = s2$sd_num
    )
    
    if (assign_to_env) {
      if (!is.null(pvlit) && length(pvlit) > 0 && !all(is.na(pvlit))) {
        assign(paste0(gname, "_mean_lit_df1"), s1$mean_lit, parent.frame())
        assign(paste0(gname, "_sd_lit_df1"),   s1$sd_lit,   parent.frame())
        assign(paste0(gname, "_mean_lit_df2"), s2$mean_lit, parent.frame())
        assign(paste0(gname, "_sd_lit_df2"),   s2$sd_lit,   parent.frame())
      }
      if (!is.null(pvnum) && length(pvnum) > 0 && !all(is.na(pvnum))) {
        assign(paste0(gname, "_mean_num_df1"), s1$mean_num, parent.frame())
        assign(paste0(gname, "_sd_num_df1"),   s1$sd_num,   parent.frame())
        assign(paste0(gname, "_mean_num_df2"), s2$mean_num, parent.frame())
        assign(paste0(gname, "_sd_num_df2"),   s2$sd_num,   parent.frame())
      }
    }
  }
  
  invisible(out)
}
