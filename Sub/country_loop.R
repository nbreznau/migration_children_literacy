# collect ISO codes from both designs
iso_vec <- sort(unique(c(
  as.character(df1_rep$variables$iso3c),
  as.character(df2_rep$variables$iso3c)
)))
iso_vec <- iso_vec[!is.na(iso_vec) & nzchar(iso_vec)]

# loop over countries
res_country <- do.call(rbind, lapply(iso_vec, function(cc) {
  
  des1_c <- tryCatch(subset(df1_rep, df1_rep$variables$iso3c == cc),
                     error = function(e) subset(df1_rep, FALSE))
  des2_c <- tryCatch(subset(df2_rep, df2_rep$variables$iso3c == cc),
                     error = function(e) subset(df2_rep, FALSE))
  
  # helper defined in IRT_helper_groups.R
  out <- calc_groups_pv(
    df1_rep = des1_c,
    df2_rep = des2_c,
    pvlit   = pvlit,
    pvnum   = NA,
    group_vars   = "FORBORN"
  )
  
  #extract single group
  g <- out[[1]]
  
  #assemble
  data.frame(
    iso3c         = cc,
    mean_lit_df1  = g$mean_lit_df1,
    sd_lit_df1    = g$sd_lit_df1,
    mean_lit_df2  = g$mean_lit_df2,
    sd_lit_df2    = g$sd_lit_df2,
    stringsAsFactors = FALSE
  )
}))

# do again for full population estimates
res_country_all <- do.call(rbind, lapply(iso_vec, function(cc) {
  
  des1_c <- tryCatch(subset(df1_rep, df1_rep$variables$iso3c == cc),
                     error = function(e) subset(df1_rep, FALSE))
  des2_c <- tryCatch(subset(df2_rep, df2_rep$variables$iso3c == cc),
                     error = function(e) subset(df2_rep, FALSE))
  
  # script defined in IRT_helper_groups.R
  out <- calc_groups_pv_basic(
    df1_rep = des1_c,
    df2_rep = des2_c,
    pvlit   = pvlit,
    pvnum   = NA,
    group_vars   = "ALL"
  )
  
  #extract single group
  g <- out[[1]]
  
  #assemble
  data.frame(
    iso3c         = cc,
    mean_lit_df1  = g$mean_lit_df1,
    sd_lit_df1    = g$sd_lit_df1,
    mean_lit_df2  = g$mean_lit_df2,
    sd_lit_df2    = g$sd_lit_df2,
    stringsAsFactors = FALSE
  )
}))


# setup plotting df

res_country <- res_country %>%
  mutate(change_lit = mean_lit_df2 - mean_lit_df1,
         group = "Immigrants")

res_country_all <- res_country_all %>%
  mutate(change_lit = mean_lit_df2 - mean_lit_df1,
         group = "All adults")

plot_df <- bind_rows(res_country, res_country_all) %>%
  filter(!is.na(change_lit)) %>%
  arrange(group, change_lit)

order_df <- plot_df %>%
  group_by(iso3c) %>%
  summarize(mean_change = mean(change_lit, na.rm = TRUE)) %>%
  arrange(mean_change)

plot_df$iso3c <- factor(plot_df$iso3c, levels = order_df$iso3c)

save(plot_df, file = here("Data", "plot_df.RData"))

