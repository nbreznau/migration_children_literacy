# functions_descriptives.R
#
# Weighted descriptive statistics for PIAAC with correct standard errors, and
# the "flower tree" figure (Figure 1 and Supplementary Figure S3).
#
# Standard errors follow the PIAAC technical standards:
#   * sampling variance from the 80 replicate weights, using each country's
#     own method: Fay's method (factor 0.3) everywhere except France
#     (paired jackknife, JK2); Chile, Portugal and the US use only their first
#     28, 52 and 44 replicates;
#   * imputation variance across the ten plausible values, combined with
#     Rubin's rules.
# Pooled estimates for several countries treat each country as an independent
# sample: the variance is the sum of every country's replicate contributions.

# -----------------------------------------------------------------------------
# Replicate-weight variance for a weighted mean, pooled over countries
# -----------------------------------------------------------------------------

#' Variance factor per replicate for each country.
#' Fay: 1 / (R (1 - k)^2); JK2: 1; JK1: (R - 1) / R.
rep_factor <- function(method, fayfac, nreps) {
  dplyr::case_when(method == "FAY" ~ 1 / (nreps * (1 - fayfac)^2),
                   method == "JK2" ~ 1,
                   method == "JK1" ~ (nreps - 1) / nreps)
}

#' Prepare a data set once for repeated estimation: replicate-weight matrix,
#' country index and per-country variance settings.
make_design <- function(data, weight = "SPFWT0") {
  countries <- data %>%
    dplyr::distinct(iso3c, VEMETHOD, VEFAYFAC, VENREPS) %>%
    dplyr::mutate(factor = rep_factor(VEMETHOD, VEFAYFAC, VENREPS))
  stopifnot(!anyDuplicated(countries$iso3c))
  list(data    = data,
       w0      = data[[weight]],
       W       = as.matrix(data[, paste0("SPFWT", 1:80)]),
       country = data$iso3c,
       factor  = setNames(countries$factor, countries$iso3c),
       nreps   = setNames(countries$VENREPS, countries$iso3c))
}

#' Keep only some rows of a design (a domain such as one group).
subset_design <- function(design, rows) {
  design$data    <- design$data[rows, , drop = FALSE]
  design$w0      <- design$w0[rows]
  design$W       <- design$W[rows, , drop = FALSE]
  design$country <- design$country[rows]
  design
}

#' Weighted mean of y (one value per row of the design) with its replicate
#' sampling variance. Rows with missing y are left out.
rep_mean <- function(design, y) {
  if (anyNA(y)) {
    design <- subset_design(design, !is.na(y))
    y <- y[!is.na(y)]
  }
  if (length(y) == 0) return(c(est = NA_real_, var = NA_real_))
  cn <- design$country

  # weighted sums of y and of the weights, by country (full and replicate)
  sy0 <- rowsum(design$w0 * y, cn)[, 1]
  sw0 <- rowsum(design$w0, cn)[, 1]
  syr <- rowsum(design$W * y, cn)
  swr <- rowsum(design$W, cn)

  est <- sum(sy0) / sum(sw0)
  # estimate when only country c's weights are replaced by replicate r
  theta <- (sum(sy0) - sy0 + syr) / (sum(sw0) - sw0 + swr)
  used  <- outer(design$nreps[rownames(syr)], 1:80, ">=")
  v     <- sum(design$factor[rownames(syr)] * rowSums((theta - est)^2 * used))
  c(est = est, var = v)
}

#' Combine estimates over plausible values with Rubin's rules.
rubin <- function(est_var) {
  m <- est_var["est", ]
  if (anyNA(m)) return(c(est = NA_real_, se = NA_real_))
  M <- length(m)
  W <- mean(est_var["var", ])
  B <- if (M > 1) stats::var(m) else 0
  c(est = mean(m), se = sqrt(W + (1 + 1 / M) * B))
}

#' Mean of a set of plausible values (or of one ordinary variable) with SE.
pv_mean <- function(design, vars, transform = identity) {
  est_var <- vapply(vars, function(v) rep_mean(design, transform(design$data[[v]])),
                    numeric(2))
  rubin(est_var)
}

# -----------------------------------------------------------------------------
# Statistics for one group of respondents
# -----------------------------------------------------------------------------

#' Literacy (or numeracy) mean, % below Level 2, % in ALE and % with tertiary
#' education, each with its standard error, plus the unweighted n.
group_stats <- function(design, rows, domain = "LIT") {
  design <- subset_design(design, rows)
  pvs   <- paste0("PV", domain, 1:10)
  skill <- pv_mean(design, pvs)
  low   <- pv_mean(design, pvs, transform = function(x) 100 * (x < 226))
  ale   <- pv_mean(design, "NFE12", transform = function(x) 100 * x)
  ter   <- pv_mean(design, "tertiary", transform = function(x) 100 * x)
  tibble::tibble(
    n = sum(rows),
    mean = skill[["est"]], se = skill[["se"]],
    pct_below_l2 = low[["est"]], se_below_l2 = low[["se"]],
    pct_ale = ale[["est"]], se_ale = ale[["se"]],
    pct_tertiary = ter[["est"]], se_tertiary = ter[["se"]]
  )
}

# -----------------------------------------------------------------------------
# The intersectional tree: 1 + 2 + 4 + 8 + 16 = 31 nodes
# -----------------------------------------------------------------------------

#' The four splits, in the order they appear going down the tree. Each node
#' label joins the codes of its branches, e.g. "female_fb_notnatlang_child".
tree_splits <- list(
  list(var = "female",   codes = c(female = 1, male = 0)),
  list(var = "FORBORN",  codes = c(fb = 1, not = 0)),
  list(var = "FORLANG",  codes = c(natlang = 0, notnatlang = 1)),
  list(var = "children", codes = c(child = 1, nochild = 0))
)

#' All nodes of the tree with their depth, parent and position.
#' Leaves (depth 4) sit at x = -7.5, -6.5, ..., 7.5; every other node is
#' centred over its two children.
tree_nodes <- function(splits = tree_splits) {
  nodes <- tibble::tibble(label = "ALL", depth = 0L, parent = NA_character_,
                          index = 0L)
  current <- nodes
  for (d in seq_along(splits)) {
    codes <- names(splits[[d]]$codes)
    current <- tidyr::expand_grid(parent_row = seq_len(nrow(current)),
                                  branch = seq_along(codes)) %>%
      dplyr::mutate(
        parent = current$label[parent_row],
        label  = ifelse(parent == "ALL", codes[branch],
                        paste(parent, codes[branch], sep = "_")),
        depth  = d,
        index  = (current$index[parent_row]) * length(codes) + branch - 1L) %>%
      dplyr::select(label, depth, parent, index)
    nodes <- dplyr::bind_rows(nodes, current)
  }
  D <- length(splits)
  nodes %>% dplyr::mutate(x = (index + 0.5) * 2^(D - depth) - 2^D / 2)
}

#' Rows of the data that belong to a node label.
node_rows <- function(data, label, splits = tree_splits) {
  rows <- rep(TRUE, nrow(data))
  if (label == "ALL") return(rows)
  parts <- strsplit(label, "_")[[1]]
  for (d in seq_along(parts)) {
    s <- splits[[d]]
    rows <- rows & data[[s$var]] %in% s$codes[[parts[d]]]
  }
  rows
}

#' Group statistics for every node of the tree.
tree_stats <- function(data, domain = "LIT") {
  design <- make_design(data)
  nodes  <- tree_nodes()
  stats  <- purrr::map(nodes$label,
                       ~ group_stats(design, node_rows(data, .x), domain))
  dplyr::bind_cols(nodes, dplyr::bind_rows(stats))
}

# -----------------------------------------------------------------------------
# Flower-tree figure
# -----------------------------------------------------------------------------

tree_labels <- list(
  en = c(ALL = "Entire Population", male = "Male", female = "Female",
         not = "Native-born", fb = "Foreign-born", natlang = "Native",
         notnatlang = "Non-native", child = "Parent", nochild = "Childless",
         levels = c("All", "Gender", "Migration", "Language", "Parenthood"),
         skill_lit = "Literacy\n(Avg.)", skill_num = "Numeracy\n(Avg.)",
         ale = "ALE partici-\npation (%)", ter = "Tertiary\ncompletion (%)",
         low = "Lowest values", high = "Highest values"),
  de = c(ALL = "Gesamtbev\u00f6lkerung", male = "M\u00e4nner", female = "Frauen",
         not = "Im Inland geboren", fb = "Im Ausland geboren",
         natlang = "Muttersprache", notnatlang = "Andere Sprache",
         child = "Eltern", nochild = "Kinderlos",
         levels = c("Alle", "Geschlecht", "Migration", "Sprache", "Elternschaft"),
         skill_lit = "Lesekompetenz\n(Mittel)", skill_num = "Alltagsmathematik\n(Mittel)",
         ale = "Weiterbildung\n(%)", ter = "Hochschul-\nabschluss (%)",
         low = "Niedrigste Werte", high = "H\u00f6chste Werte")
)

#' Colour for each value on a fixed scale, so the same colour means the same
#' value in every panel that shares `limits`.
viridis_fixed <- function(x, limits) {
  pal <- scales::viridis_pal()(100)
  pos <- pmin(pmax((x - limits[1]) / diff(limits), 0), 1)
  ifelse(is.na(x), NA_character_, pal[1 + round(pos * 99)])
}

#' Draw one flower tree. Each node shows three petals: skill mean (top),
#' ALE participation (right) and tertiary completion (left).
#' `limits` is a list with elements mean, pct_ale and pct_tertiary.
plot_tree <- function(stats, title, limits, lang = "en", domain = "LIT",
                      x_scale = 6) {
  lab <- tree_labels[[lang]]
  rx <- 0.8; ry <- 0.15   # petal size

  d <- stats %>%
    dplyr::mutate(
      x = x * x_scale, y = depth,
      branch = ifelse(label == "ALL", "ALL", sub(".*_", "", label)),
      plot_label = lab[branch],
      col_skill = viridis_fixed(mean, limits$mean),
      col_ale   = viridis_fixed(pct_ale, limits$pct_ale),
      col_ter   = viridis_fixed(pct_tertiary, limits$pct_tertiary),
      first = label == "ALL")

  edges <- d %>%
    dplyr::filter(!is.na(parent)) %>%
    dplyr::select(xend = x, yend = y, parent) %>%
    dplyr::left_join(d %>% dplyr::select(parent = label, x, y), by = "parent")

  leg_x <- min(d$x) + 9
  bar_w <- diff(range(d$x)) * 0.15
  bar_x <- max(d$x) - 8 - bar_w

  ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_segment(data = edges, ggplot2::aes(xend = xend, yend = yend),
                          colour = "grey75", linetype = "dotted") +
    # petals
    ggplot2::geom_segment(ggplot2::aes(xend = x + rx * 0.866, yend = y + ry * 0.7,
                                       colour = col_ale), linewidth = 3.5, lineend = "round") +
    ggplot2::geom_segment(ggplot2::aes(xend = x - rx * 0.866, yend = y + ry * 0.7,
                                       colour = col_ter), linewidth = 3.5, lineend = "round") +
    ggplot2::geom_segment(ggplot2::aes(xend = x, yend = y - ry, colour = col_skill),
                          linewidth = 3.5, lineend = "round") +
    # petal values
    ggplot2::geom_text(ggplot2::aes(y = y - ry - 0.1, label = round(mean)),
                       vjust = 0, size = 2.2) +
    ggplot2::geom_text(ggplot2::aes(x = x + rx * 0.866 + 0.3, y = y + ry * 0.5 + 0.05,
                                    label = paste0("  ", round(pct_ale), ifelse(first, "%", ""))),
                       hjust = 0, size = 2.2) +
    ggplot2::geom_text(ggplot2::aes(x = x - rx * 0.866 - 0.3, y = y + ry * 0.5 + 0.05,
                                    label = paste0(round(pct_tertiary), ifelse(first, "% ", "  "))),
                       hjust = 1, size = 2.2) +
    ggplot2::geom_text(ggplot2::aes(label = plot_label), vjust = 5, size = 1.9) +
    # petal key (top left)
    ggplot2::annotate("segment", x = leg_x, xend = leg_x, y = 0, yend = -ry,
                      linewidth = 3.5, colour = "grey60", lineend = "round") +
    ggplot2::annotate("segment", x = leg_x, xend = leg_x + rx * 0.866, y = 0, yend = ry * 0.7,
                      linewidth = 3.5, colour = "grey60", lineend = "round") +
    ggplot2::annotate("segment", x = leg_x, xend = leg_x - rx * 0.866, y = 0, yend = ry * 0.7,
                      linewidth = 3.5, colour = "grey60", lineend = "round") +
    ggplot2::annotate("text", x = leg_x, y = -ry - 0.15, vjust = 0, size = 2.2,
                      label = lab[[paste0("skill_", tolower(domain))]]) +
    ggplot2::annotate("text", x = leg_x + rx * 0.866 + 4.5, y = ry * 0.5 + 0.05,
                      size = 2.2, label = lab[["ale"]]) +
    ggplot2::annotate("text", x = leg_x - rx * 0.866 - 4.5, y = ry * 0.5 + 0.05,
                      size = 2.2, label = lab[["ter"]]) +
    # colour key (top right)
    ggplot2::annotate("rect", xmin = bar_x + (0:8) * bar_w / 9,
                      xmax = bar_x + (1:9) * bar_w / 9,
                      ymin = 0.09, ymax = 0.15, fill = scales::viridis_pal()(9)) +
    ggplot2::annotate("text", x = bar_x - 0.3, y = 0.12, hjust = 1, size = 2.2,
                      label = lab[["low"]]) +
    ggplot2::annotate("text", x = bar_x + bar_w + 0.3, y = 0.12, hjust = 0, size = 2.2,
                      label = lab[["high"]]) +
    ggplot2::annotate("text", x = max(d$x) + 2, y = -0.3, label = title, size = 4,
                      hjust = 1) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_y_reverse(breaks = 0:4, labels = lab[paste0("levels", 1:5)],
                             limits = c(4.5, -0.6)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())
}

#' Shared colour limits for a list of tree_stats tables.
tree_limits <- function(stats_list) {
  all <- dplyr::bind_rows(stats_list)
  list(mean = range(all$mean, na.rm = TRUE),
       pct_ale = range(all$pct_ale, na.rm = TRUE),
       pct_tertiary = range(all$pct_tertiary, na.rm = TRUE))
}

# -----------------------------------------------------------------------------
# Simple descriptive table
# -----------------------------------------------------------------------------

#' Unweighted mean, SD, min, max and n, plus the weighted mean (final
#' weight SPFWT0) for each variable.
describe_vars <- function(data, vars, labels = vars, weight = "SPFWT0") {
  purrr::map2_dfr(vars, labels, function(v, l) {
    x <- data[[v]]; w <- data[[weight]]; ok <- !is.na(x)
    tibble::tibble(variable = l,
                   mean = mean(x[ok]), wmean = stats::weighted.mean(x[ok], w[ok]),
                   sd = stats::sd(x[ok]), min = min(x[ok]), max = max(x[ok]),
                   n = sum(ok))
  })
}
