if (!file.exists(here("Data", "pop_2024.RDS"))) {

pop_raw <- wb_data(
  indicator = "SP.POP.TOTL",
  start_date = 2024,
  end_date = 2024
) %>%
  left_join(wb_countries(), by = "iso2c") %>%
  filter(!is.na(region), !region %in% "Aggregates")

pop_2024 <- pop_raw %>%
  mutate(
    Entity = case_when(
      region == "East Asia & Pacific" ~ "Asia",
      region == "Europe & Central Asia" ~ "Europe",
      region == "Latin America & Caribbean" ~ "South America",
      region == "Middle East & North Africa" ~ "Africa",
      region == "Sub-Saharan Africa" ~ "Africa",
      region == "North America" ~ "North America",
      region == "South Asia" ~ "Asia",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Entity)) %>%
  group_by(Entity) %>%
  summarise(population_2024 = sum(SP.POP.TOTL, na.rm = TRUE), .groups="drop") %>%
  bind_rows(
    summarise(., Entity = "World",
              population_2024 = sum(population_2024))
  )

write_rds(pop_2024, here("Data", "pop_2024.RDS"))

rm(pop_raw, pop_2024)

}

df_pop_2024 <- readRDS(here("Data", "pop_2024.RDS"))