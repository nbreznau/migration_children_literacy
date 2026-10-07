# combine_g_objects.R
library(dplyr)
library(purrr)
library(tibble)

# labels in order g1 ... g31
group_labels <- c(
  "ALL", "male", "female", "male_not", "male_fb", "female_not", "female_fb",
  "male_not_natlang", "male_not_notnatlang", "male_fb_natlang", "male_fb_notnatlang",
  "female_not_natlang", "female_not_notnatlang", "female_fb_natlang", "female_fb_notnatlang",
  "male_not_natlang_child", "male_not_notnatlang_child", "male_fb_natlang_child", "male_fb_notnatlang_child",
  "male_not_natlang_nochild", "male_not_notnatlang_nochild", "male_fb_natlang_nochild", "male_fb_notnatlang_nochild",
  "female_not_natlang_child", "female_not_notnatlang_child", "female_fb_natlang_child", "female_fb_notnatlang_child",
  "female_not_natlang_nochild", "female_not_notnatlang_nochild", "female_fb_natlang_nochild", "female_fb_notnatlang_nochild"
)

groups <- paste0("g", seq_along(group_labels))   # "g1" ... "g31"
cycles <- c("df1", "df2")                        # cycle 1 and 2

#  lit and num
fetch_stats_domain <- function(g, cyc, domain = c("lit", "num")) {
  domain <- match.arg(domain)
  
  mean_name <- paste0(g, "_mean_", domain, "_", cyc)
  se_name   <- paste0(g, "_se_",   domain, "_", cyc)
  ale_name  <- paste0(g, "_pct_ale_", cyc) # ALE
  sec_name  <- paste0(g, "_pct_sec_", cyc) # % secondary
  
  tibble(
    group   = g,
    cycle   = cyc,
    mean    = as.numeric(get0(mean_name, ifnotfound = NA_real_)),
    se      = as.numeric(get0(se_name,   ifnotfound = NA_real_)),
    pct_ale = as.numeric(get0(ale_name,  ifnotfound = NA_real_)),
    pct_sec = as.numeric(get0(sec_name,  ifnotfound = NA_real_))
  )
}

build_df_domain <- function(domain = c("lit", "num"),
                            cycle_labels = c("Cycle 1 (2012)", "Cycle 2 (2022)")) {
  domain <- match.arg(domain)
  
  map_dfr(groups, \(g) map_dfr(cycles, \(cyc) fetch_stats_domain(g, cyc, domain))) |>
    mutate(
      group_label = group_labels[as.integer(sub("^g", "", group))],
      mean = round(mean, 1),
      se   = round(se, 2),
      group = factor(group, levels = groups),
      cycle = factor(cycle,
                     levels = c("df1", "df2"),
                     labels = cycle_labels)
    )
}

df_lit <- build_df_domain("lit")
df_num <- build_df_domain("num")