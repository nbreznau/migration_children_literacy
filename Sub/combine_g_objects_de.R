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

groups <- paste0("g", seq_along(group_labels))     # "g1" ... "g31"
cycles <- c("df1", "df2")                           # cycle 1 and 2

# helper to fetch mean/se for a group-cycle pair, safely
fetch_stats <- function(g, cyc) {
  mean_name <- paste0(g, "_mean_lit_", cyc)
  se_name   <- paste0(g, "_se_lit_",   cyc)
  
  mean_val <- get0(mean_name, ifnotfound = NA_real_)
  se_val   <- get0(se_name,   ifnotfound = NA_real_)
  
  tibble(
    group = g,
    cycle = cyc,
    mean  = as.numeric(mean_val),
    se    = as.numeric(se_val)
  )
}

# build the long table
df_lit_de <- map_dfr(groups, \(g) map_dfr(cycles, \(cyc) fetch_stats(g, cyc))) |>
  mutate(
    # attach pretty label in same order as groups
    group_label = group_labels[as.integer(sub("^g", "", group))],
    # round as requested
    mean = round(mean, 1),
    se   = round(se, 2)
  ) |>
  # optional: order factors nicely
  mutate(
    group = factor(group, levels = groups),
    cycle = factor(cycle, levels = c("df1", "df2"),
                   labels = c("Cycle 1 (2012)", "Cycle 2 (2022)"))
  )

# peek
df_lit_de
