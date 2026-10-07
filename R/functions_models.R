# functions_models.R
#
# Shared code for the regression scripts (03, 04, 05): preparing the model
# data, fitting the multilevel models, estimating the effect of ALE for each
# intersectional group with its standard error, and drawing the effect
# figures.
#
# The approach in brief
#
#   * Every variable in the models is split into a between-country part (the
#     country mean) and a within-country part (the person's value minus that
#     mean). The between parts are z-scored across countries.
#   * Every country has the same influence on the estimates: each respondent
#     gets the weight  (average sample size per country) / (own country's
#     sample size). Within a country all respondents count equally.
#   * The effect of ALE for a group (e.g. foreign-born, non-native-speaking
#     mothers) is the difference in predicted literacy between ALE = 1 and
#     ALE = 0 with the group characteristics fixed. Because ALE enters the
#     model linearly (alone and in interactions), this difference is a fixed
#     weighted sum of model coefficients, the same for every person and
#     every country. Its standard error therefore follows exactly from the
#     coefficients' covariance matrix (no simulation or t-test needed).

# -----------------------------------------------------------------------------
# Countries used in the robustness checks
# -----------------------------------------------------------------------------

#' High-income immigrant destination countries (05 and Supplementary Fig S3).
high_income_destinations <- c("AUT", "BEL", "CHE", "DEU", "DNK", "ESP", "FIN",
                              "FRA", "GBR", "IRL", "ITA", "NOR", "PRT", "SWE",
                              "JPN", "CAN", "USA", "NZL")

# -----------------------------------------------------------------------------
# Model data
# -----------------------------------------------------------------------------

#' The characteristics split into within- and between-country parts. Names
#' on the left are the variables in df2, names on the right the stems used
#' in the models (e.g. forborn_within, forborn_between_z).
wb_vars <- c(NFE12 = "NFE12", female = "female", children = "children",
             FORLANG = "forlang", FORBORN = "forborn", tertiary = "ed3_tertiary")

#' Analysis sample with within/between variables and country weights.
#'
#' `countries`: optional vector of ISO codes to restrict the sample.
#' Country means use the final weight SPFWT0, so the between part is the
#' population share in that country. Between parts are z-scored across the
#' countries in the sample (one value per country).
prep_model_data <- function(df2, countries = NULL) {
  d <- dplyr::filter(df2, in_sample == 1)
  if (!is.null(countries)) d <- dplyr::filter(d, iso3c %in% countries)

  for (v in names(wb_vars)) {
    stem <- wb_vars[[v]]
    between <- ave(d$SPFWT0 * d[[v]], d$iso3c, FUN = sum) /
      ave(d$SPFWT0, d$iso3c, FUN = sum)
    d[[paste0(stem, "_between")]] <- between
    d[[paste0(stem, "_within")]]  <- d[[v]] - between
  }

  # z-score the between parts across countries
  country_means <- d %>%
    dplyr::distinct(iso3c, dplyr::across(dplyr::ends_with("_between")))
  z <- country_means %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_between"),
                                ~ as.numeric(scale(.x)), .names = "{.col}_z")) %>%
    dplyr::select(iso3c, dplyr::ends_with("_z"))
  d <- dplyr::left_join(d, z, by = "iso3c")

  d %>%
    dplyr::mutate(
      # age group missing (a few respondents in SWE and NOR) is its own
      # category, so these respondents stay in the analysis
      age_group = droplevels(factor(dplyr::coalesce(age10, 6), levels = 1:6,
                                    labels = c("16-24", "25-34", "35-44",
                                               "45-54", "55-65", "Missing"))),
      # equal country weights, mean 1
      cweight = (dplyr::n() / dplyr::n_distinct(iso3c)) /
        ave(rep(1, dplyr::n()), iso3c, FUN = length))
}

#' Country means of the between parts (unweighted across countries). A
#' group's within value is its 0/1 value minus this mean, i.e. the group as
#' it would appear in a country with average composition.
grand_means <- function(d) {
  d %>%
    dplyr::distinct(iso3c, dplyr::across(dplyr::ends_with("_between"))) %>%
    dplyr::summarise(dplyr::across(dplyr::ends_with("_between"), mean)) %>%
    unlist()
}

# -----------------------------------------------------------------------------
# Model formulas
# -----------------------------------------------------------------------------

#' Main-effect part: within and between terms for each characteristic, the
#' age control, plus optional extra terms.
main_terms <- function(stems = c("NFE12", "female", "children", "forlang",
                                 "forborn", "ed3_tertiary"),
                       extra = NULL, age = TRUE) {
  c(paste0(stems, "_within"), paste0(stems, "_between_z"),
    if (age) "age_group", extra)
}

#' Full formula. `interaction`: stems whose within parts are fully
#' interacted (all sub-interactions included), e.g. c("female", "forborn",
#' "children", "forlang", "NFE12"). `random`: random-effects part.
make_formula <- function(stems = c("NFE12", "female", "children", "forlang",
                                   "forborn", "ed3_tertiary"),
                         interaction = NULL, extra = NULL,
                         random = "(1 | iso3c)") {
  rhs <- main_terms(stems, extra)
  if (length(interaction) > 0)
    rhs <- c(rhs, paste(paste0(interaction, "_within"), collapse = " * "))
  stats::as.formula(paste("skill_reading ~", paste(c(rhs, random), collapse = " + ")))
}

#' Fit a linear mixed model by maximum likelihood (so that AIC and BIC can
#' be compared across models with different fixed effects), with equal
#' country weights.
fit_lmm <- function(formula, data, weights = TRUE) {
  data$.w <- if (weights) data$cweight else 1
  m <- lme4::lmer(formula, data = data, weights = .w, REML = FALSE,
                  control = lme4::lmerControl(calc.derivs = FALSE))
  m
}

# -----------------------------------------------------------------------------
# Effect of ALE for a group: estimate and standard error
# -----------------------------------------------------------------------------

#' The 12 groups shown in the effect figures (native-born non-native
#' speakers are left out: too few cases).
ate_profiles <- tibble::tribble(
  ~type_id, ~type_label,                                        ~female, ~forborn, ~children, ~forlang,
  "1f", "1f. Foreign-born mother\nnon-native speaker",                1, 1, 1, 1,
  "2f", "2f. Foreign-born no children female\nnon-native speaker",    1, 1, 0, 1,
  "3f", "3f. Foreign-born mother\nnative speaker",                    1, 1, 1, 0,
  "4f", "4f. Foreign-born no children female\nnative speaker",        1, 1, 0, 0,
  "5f", "5f. Native-born mother\nnative speaker",                     1, 0, 1, 0,
  "6f", "6f. Native-born no children female\nnative speaker",         1, 0, 0, 0,
  "1m", "1m. Foreign-born father\nnon-native speaker",                0, 1, 1, 1,
  "2m", "2m. Foreign-born no children male\nnon-native speaker",      0, 1, 0, 1,
  "3m", "3m. Foreign-born father\nnative speaker",                    0, 1, 1, 0,
  "4m", "4m. Foreign-born no children male\nnative speaker",          0, 1, 0, 0,
  "5m", "5m. Native-born father\nnative speaker",                     0, 0, 1, 0,
  "6m", "6m. Native-born no children male\nnative speaker",           0, 0, 0, 0
)

#' Fixed-effects design matrix of `model` for new data.
fixed_matrix <- function(model, newdata) {
  tt <- stats::delete.response(stats::terms(model, fixed.only = TRUE))
  X  <- stats::model.matrix(tt, newdata)
  X[, names(lme4::fixef(model)), drop = FALSE]
}

#' Estimate, standard error and 95% CI of a linear combination of the fixed
#' effects, given the contrast vector `cvec`.
lincomb <- function(model, cvec) {
  b <- lme4::fixef(model)
  V <- as.matrix(stats::vcov(model))
  est <- sum(cvec * b)
  se  <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  tibble::tibble(ATE = est, se = se, lower = est - 1.96 * se, upper = est + 1.96 * se)
}

#' Set a group's characteristics in `nd` (within part = value - grand mean)
#' and ALE to `ale`. `group` is a named list of 0/1 values using the stems
#' of `wb_vars` (e.g. list(female = 1, forborn = 1)); characteristics not in
#' `group` keep their observed values.
set_group <- function(nd, group, ale, gm) {
  for (stem in names(group)) {
    if (is.na(group[[stem]])) next
    nd[[paste0(stem, "_within")]] <- group[[stem]] - gm[[paste0(stem, "_between")]]
  }
  nd$NFE12_within <- ale - gm[["NFE12_between"]]
  nd
}

#' Effect of ALE for one group. With every characteristic fixed, one row of
#' data is enough (the other variables cancel out of the difference).
group_ate <- function(model, data, group, gm = grand_means(data)) {
  nd <- data[1, ]
  x0 <- fixed_matrix(model, set_group(nd, group, 0, gm))
  x1 <- fixed_matrix(model, set_group(nd, group, 1, gm))
  lincomb(model, as.numeric(x1 - x0))
}

#' Effects for the 12 groups, optionally crossed with tertiary education.
profile_ates <- function(model, data, by_education = FALSE) {
  gm <- grand_means(data)
  prof <- ate_profiles
  if (by_education) prof <- tidyr::crossing(prof, ed3_tertiary = c(0, 1))
  groups <- prof %>% dplyr::select(dplyr::any_of(c("female", "forborn", "children",
                                                   "forlang", "ed3_tertiary")))
  res <- purrr::map_dfr(seq_len(nrow(prof)),
                        ~ group_ate(model, data, as.list(groups[.x, ]), gm))
  dplyr::bind_cols(prof, res)
}

#' Average effect of ALE over everyone in `data` who belongs to a partly
#' specified group (characteristics set to NA are left as observed), using
#' weights `w`. Used for the simulator, where e.g. "all women" is a group.
average_ate <- function(model, data, group, w = data$cweight, gm = grand_means(data)) {
  in_grp <- rep(TRUE, nrow(data))
  for (stem in names(group)) {
    if (is.na(group[[stem]])) next
    var <- names(wb_vars)[wb_vars == stem]
    in_grp <- in_grp & data[[var]] == group[[stem]]
  }
  # the effect only depends on the group characteristics, so it is enough to
  # average the contrast over the distinct combinations present in the group
  keys <- c("female", "forborn", "children", "forlang", "ed3_tertiary")
  vars <- names(wb_vars)[match(keys, wb_vars)]
  combos <- data[in_grp, ] %>%
    dplyr::mutate(.w = w[in_grp]) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars))) %>%
    dplyr::summarise(.w = sum(.w), .groups = "drop")
  cvec <- 0
  for (i in seq_len(nrow(combos))) {
    g <- stats::setNames(as.list(unlist(combos[i, vars])), keys)
    nd <- data[1, ]
    cvec <- cvec + combos$.w[i] / sum(combos$.w) *
      as.numeric(fixed_matrix(model, set_group(nd, g, 1, gm)) -
                 fixed_matrix(model, set_group(nd, g, 0, gm)))
  }
  lincomb(model, cvec)
}

# -----------------------------------------------------------------------------
# Tables
# -----------------------------------------------------------------------------

#' Coefficients (estimate and SE, with stars) of several models side by side,
#' plus N, number of countries, R2 (marginal and conditional for mixed
#' models), log-likelihood, AIC and BIC.
model_table <- function(models, digits = 2) {
  stars <- function(p) dplyr::case_when(p < 0.001 ~ "***", p < 0.01 ~ "**",
                                        p < 0.05 ~ "*", TRUE ~ "")
  coefs <- purrr::imap_dfr(models, function(m, name) {
    s <- summary(m)$coefficients
    p <- if ("Pr(>|t|)" %in% colnames(s)) s[, "Pr(>|t|)"] else
      2 * stats::pnorm(-abs(s[, "Estimate"] / s[, "Std. Error"]))
    tibble::tibble(model = name, term = rownames(s),
                   value = sprintf(paste0("%.", digits, "f%s (%.", digits, "f)"),
                                   s[, "Estimate"], stars(p), s[, "Std. Error"]))
  }) %>%
    dplyr::filter(!grepl("^factor\\(iso3c\\)", term)) %>%
    tidyr::pivot_wider(names_from = model, values_from = value, values_fill = "")

  fit <- purrr::imap_dfr(models, function(m, name) {
    if (inherits(m, "merMod")) {
      r2 <- suppressWarnings(performance::r2_nakagawa(m))
      tibble::tibble(model = name, N = stats::nobs(m),
                     Countries = lme4::ngrps(m)[["iso3c"]],
                     `R2 marginal` = r2$R2_marginal, `R2 conditional` = r2$R2_conditional,
                     logLik = as.numeric(stats::logLik(m)),
                     AIC = stats::AIC(m), BIC = stats::BIC(m))
    } else {
      tibble::tibble(model = name, N = stats::nobs(m),
                     Countries = length(unique(m$model[["factor(iso3c)"]])),
                     `R2 marginal` = summary(m)$r.squared, `R2 conditional` = NA_real_,
                     logLik = as.numeric(stats::logLik(m)),
                     AIC = stats::AIC(m), BIC = stats::BIC(m))
    }
  }) %>%
    dplyr::mutate(dplyr::across(-model, ~ vapply(.x, function(v) {
      if (is.na(v)) "" else formatC(v, format = "f", big.mark = ",",
                                    digits = if (v == round(v)) 0 else if (abs(v) < 10) 3 else 1)
    }, character(1)))) %>%
    tidyr::pivot_longer(-model, names_to = "term") %>%
    tidyr::pivot_wider(names_from = model, values_from = value)

  dplyr::bind_rows(coefs, fit)
}

#' Write a table to Word (.docx) and CSV with the same base name.
save_table <- function(tab, name, caption = NULL) {
  readr::write_csv(tab, here::here("Results", paste0(name, ".csv")))
  ft <- flextable::flextable(tab) %>%
    flextable::fontsize(size = 8, part = "all") %>%
    flextable::autofit()
  if (!is.null(caption)) ft <- flextable::set_caption(ft, caption)
  flextable::save_as_docx(ft, path = here::here("Results", paste0(name, ".docx")))
  invisible(tab)
}

# -----------------------------------------------------------------------------
# Effect figures: dot matrix of group characteristics + bars with 95% CIs
# -----------------------------------------------------------------------------

effect_type_order <- function(prof = ate_profiles) {
  prof %>%
    dplyr::mutate(idx = as.integer(substr(type_id, 1, 1)),
                  sex = substr(type_id, 2, 2)) %>%
    dplyr::arrange(idx, sex) %>%
    dplyr::pull(type_label)
}

#' `effects`: output of profile_ates(). `panels`: column that splits the
#' bars into panels (e.g. "ed3_tertiary") or NULL. `compare`: optional
#' second set of effects (same groups) drawn as hollow bars for comparison.
plot_effects <- function(effects, panels = NULL, panel_labels = NULL,
                         compare = NULL, compare_label = NULL,
                         ylab = "Effect of ALE participation on literacy") {
  lv <- rev(effect_type_order())
  pal <- viridis::viridis(12, option = "D", begin = 0.1, end = 0.9)
  names(pal) <- lv
  effects <- dplyr::mutate(effects, type_label = factor(type_label, levels = lv))

  dots <- ate_profiles %>%
    dplyr::transmute(type_label = factor(type_label, levels = lv),
                     Female = female, `Foreign-born` = forborn,
                     `Non-native speaker` = forlang, Parent = children) %>%
    tidyr::pivot_longer(-type_label, names_to = "variable") %>%
    dplyr::mutate(variable = factor(variable, levels = c("Female", "Foreign-born",
                                                         "Non-native speaker", "Parent")))
  p_dots <- ggplot2::ggplot(dots, ggplot2::aes(variable, type_label)) +
    ggplot2::geom_point(ggplot2::aes(shape = factor(value)), size = 3.2, fill = "white") +
    ggplot2::scale_shape_manual(values = c(`0` = 21, `1` = 16), guide = "none") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 10, angle = 45, hjust = 1))

  rng <- range(c(effects$lower, effects$upper, 0,
                 if (!is.null(compare)) c(compare$lower, compare$upper)), na.rm = TRUE)

  bar_panel <- function(e, cmp, title) {
    p <- ggplot2::ggplot(e, ggplot2::aes(type_label, ATE, fill = type_label)) +
      ggplot2::geom_col(width = 0.55, colour = "black") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.15)
    if (!is.null(cmp)) {
      cmp <- dplyr::mutate(cmp, type_label = factor(type_label, levels = lv))
      p <- p + ggplot2::geom_point(data = cmp, ggplot2::aes(type_label, ATE),
                                   inherit.aes = FALSE, shape = 23, size = 2.5,
                                   fill = "white")
    }
    p + ggplot2::scale_fill_manual(values = pal, guide = "none") +
      ggplot2::coord_flip(ylim = rng) +
      ggplot2::labs(x = NULL, y = title) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank())
  }

  if (is.null(panels)) {
    bars <- list(bar_panel(effects, compare, ylab))
  } else {
    vals <- sort(unique(effects[[panels]]))
    bars <- purrr::map(seq_along(vals), function(i) {
      sel <- effects[[panels]] == vals[i]
      cmp <- if (!is.null(compare)) compare[compare[[panels]] == vals[i], ] else NULL
      bar_panel(effects[sel, ], cmp, paste0(panel_labels[i], "\n", ylab))
    })
  }
  p <- patchwork::wrap_plots(c(list(p_dots), bars), nrow = 1,
                             widths = c(1.1, rep(2.4, length(bars))))
  if (!is.null(compare_label))
    p <- p + patchwork::plot_annotation(caption = compare_label)
  p
}

#' Save a figure as PNG in Results/ and return the path.
save_png <- function(p, file, width = 1100, height = 750) {
  path <- here::here("Results", file)
  ragg::agg_png(path, res = 144, width = width, height = height)
  print(p)
  grDevices::dev.off()
  path
}
