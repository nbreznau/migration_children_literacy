library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

# saved objects from 03_Analyses.Rmd
# no longer used this way but left for legacy
#df2  <- readRDS("shiny_df2.rds")
#df2r <- readRDS("shiny_df2r.rds")
#m4   <- readRDS("shiny_m4.rds")

# new, precomputed values (solves RAM problem on shinyapps.io)
precomp <- readRDS("precomp_spec_country.rds")
spec_grid <- readRDS("precomp_spec_grid.rds")

# helpers

map_within <- c(female="female_within", FORBORN="forborn_within", FORLANG="forlang_within", children="children_within")
map_between <- c(female="female_between", FORBORN="forborn_between", FORLANG="forlang_between", children="children_between")
vars_raw <- c("female", "FORBORN", "FORLANG", "children")

make_spec_id <- function(spec){
  paste0(
    "f=", ifelse(is.na(spec$female), "all", spec$female), ";",
    "b=", ifelse(is.na(spec$FORBORN), "all", spec$FORBORN), ";",
    "l=", ifelse(is.na(spec$FORLANG), "all", spec$FORLANG), ";",
    "c=", ifelse(is.na(spec$children), "all", spec$children)
  )
}

# Build counterfactual newdata for predict(m4) consistent with within/between coding
make_newdata_for_m4 <- function(df_base, spec, nfe_value) {
  stopifnot(all(names(spec) %in% vars_raw))
  nd <- df_base
  
  # Set NFE12 within directly to 0/1  (this is "within", so this is a strong intervention).
  nd$NFE12_within <- nfe_value - nd$NFE12_between
  
  # For each targeting var: if spec is 0/1, set within = target - between; else leave as observed (population mix).
  for (v in vars_raw) {
    target <- spec[[v]]
    if (!is.na(target)) {
      w <- map_within[[v]]
      b <- map_between[[v]]
      nd[[w]] <- target - nd[[b]]
    }
  }
  
  nd
}


# Compute AME for a spec: E[pred(NFE=1) - pred(NFE=0)] (weighted)
calc_ame_spec <- function(model_m4, df_base, spec, weight_var="aweight") {
  nd0 <- make_newdata_for_m4(df_base, spec, nfe_value = 0)
  nd1 <- make_newdata_for_m4(df_base, spec, nfe_value = 1)
  
  p0 <- predict(model_m4, newdata = nd0, re.form = NA)
  p1 <- predict(model_m4, newdata = nd1, re.form = NA)
  
  d  <- p1 - p0  # gain from ALE
  
  w <- df_base[[weight_var]]
  ok <- is.finite(d) & is.finite(w) & !is.na(w) & w > 0
  d <- d[ok]; w <- w[ok]
  
  ame <- weighted.mean(d, w)
  
  # Simple weighted SE (design-based would be better; this is a pragmatic approximation)
  se  <- sqrt( sum(w^2 * (d - ame)^2) / (sum(w)^2) )
  ci  <- ame + c(-1, 1) * 1.96 * se
  
  list(AME = ame, SE = se, lower = ci[1], upper = ci[2])
}

# Identify who is "in the targeted group" given a spec (raw vars; NA means ignore)
in_group <- function(df, spec) {
  idx <- rep(TRUE, nrow(df))
  for (v in vars_raw) {
    target <- spec[[v]]
    if (!is.na(target)) idx <- idx & (df[[v]] == target)
  }
  idx
}

# Population gain per country from targeting spec:
#    Boost skill_reading by AME for those in group AND NFE12==0, then take weighted mean difference.
calc_country_gains <- function(df2_full, spec, AME, weight_var="aweight") {
  df <- df2_full
  
  g  <- in_group(df, spec)
  treatable <- g & df$NFE12 == 0 & !is.na(df$skill_reading)
  
  df <- df %>%
    mutate(
      skill_boost = ifelse(treatable, skill_reading + AME, skill_reading),
      g_total     = ifelse(g, 1, 0),
      g_noale     = ifelse(g & NFE12 == 0, 1, 0)
    )
  
  out <- df %>%
    filter(!is.na(iso3c)) %>%
    group_by(iso3c) %>%
    summarise(
      skill_base  = weighted.mean(skill_reading, .data[[weight_var]], na.rm = TRUE),
      skill_gain  = weighted.mean(skill_boost,   .data[[weight_var]], na.rm = TRUE) - skill_base,
      pct_group   = weighted.mean(g_total, .data[[weight_var]], na.rm = TRUE),
      pct_group_noale = weighted.mean(g_noale, .data[[weight_var]], na.rm = TRUE),
      pop18to65 = sum(SPFWT0, na.rm = TRUE),
      .groups = "drop"
    )
  
  out
}

# Apply capacity cap: only treat as many in Y (among Y & no ALE) as there are in X (as a pop share)
cap_gains_to_X <- function(df_xy,
                           cap_var = "pct_group_X",
                           y_noale_var = "pct_group_noale_Y",
                           y_gain_var = "skill_gain_Y") {
  
  df_xy %>%
    mutate(
      cap_share = .data[[cap_var]],
      y_noale_share = .data[[y_noale_var]],
      cap_ratio = dplyr::if_else(
        is.na(y_noale_share) | y_noale_share <= 0,
        0,
        pmin(1, cap_share / y_noale_share)
      ),
      skill_gain_Y_capped = .data[[y_gain_var]] * cap_ratio
    )
}


# User Interface

choices_01all <- c("All" = "all", "0" = "0", "1" = "1")

ui <- fluidPage(

  titlePanel(
    tagList(
      "Targeting Adult Learning: Policy Intervention Simulator",
      tags$div(
        style = "font-size: 14px; margin-top: 4px;",
        "Average literacy gain at the country-level when targeting group X"
      ),
      tags$div(
        style = "font-size: 12px; color: #555;",
        "Estimated from PIAAC Cycle 2 data, 2022"
      )

    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "position: relative; min-height: 650px;",
      h4("X group definition"),
      tags$div(
        style = "font-size: 12px; color: #444; margin-bottom: 8px;",
        HTML('<b>Guide:</b> 1 = yes, 0 = no, All = both')
      ),
      selectInput("x_female",   "Female",            choices = choices_01all, selected = 0),
      selectInput("x_FORBORN",  "Foreign-born",      choices = choices_01all, selected = 1),
      selectInput("x_FORLANG",  "Non-native speaker",choices = choices_01all, selected = "all"),
      selectInput("x_children", "Children",          choices = choices_01all, selected = "all"),
      
      hr(),
      
      h4("Y group definition"),
      selectInput("y_female",   "Female",            choices = choices_01all, selected = "all"),
      selectInput("y_FORBORN",  "Foreign-born",      choices = choices_01all, selected = 0),
      selectInput("y_FORLANG",  "Non-native speaker",choices = choices_01all, selected = "all"),
      selectInput("y_children", "Children",          choices = choices_01all, selected = "all"),
      
      hr(),
      
      tags$div(
        style = "font-size: 12px; line-height: 1.3;",
        tags$b("Nate Breznau"), tags$br(),
        HTML('contact: <a href="mailto:breznau.nate@gmail.com">email</a>'), tags$br(),
        "CC BY 4.0", tags$br(),
        HTML('citation: <a href="https://github.com/nbreznau/migration_children_literacy" target="_blank">GitHub</a>')
      ),
      
      hr(),
      
      tags$div(
        style = "position: absolute; bottom: 10px; left: 10px;",
        tags$img(src = "die.png", style = "width: 90px; height: auto;")
      )
    ),
    
    mainPanel(
      plotOutput("scatter", height = 650),
      tableOutput("summary")
    )
  )
)

server <- function(input, output, session) {
  
  to_int_or_na <- function(x) {
    if (is.null(x) || identical(x, "all")) return(NA_integer_)
    as.integer(x)
  }
  
  specX <- reactive({
    list(
      female   = to_int_or_na(input$x_female),
      FORBORN  = to_int_or_na(input$x_FORBORN),
      FORLANG  = to_int_or_na(input$x_FORLANG),
      children = to_int_or_na(input$x_children)
    )
  })
  
  specY <- reactive({
    list(
      female   = to_int_or_na(input$y_female),
      FORBORN  = to_int_or_na(input$y_FORBORN),
      FORLANG  = to_int_or_na(input$y_FORLANG),
      children = to_int_or_na(input$y_children)
    )
  })
  
  results <- reactive({
    idX <- make_spec_id(specX())
    idY <- make_spec_id(specY())
    
    gainX <- precomp %>% filter(spec_id == idX) %>%
      rename(skill_gain_X = skill_gain,
             pct_group_X = pct_group,
             pct_group_noale_X = pct_group_noale,
             AME_X = AME, lower_X = lower, upper_X = upper)
    
    gainY <- precomp %>% filter(spec_id == idY) %>%
      rename(skill_gain_Y = skill_gain,
             pct_group_Y = pct_group,
             pct_group_noale_Y = pct_group_noale,
             AME_Y = AME, lower_Y = lower, upper_Y = upper)
    
    df_xy <- gainX %>% left_join(gainY, by="iso3c")
    
    # cap Y to X capacity (same function you already have)
    df_xy <- df_xy %>% mutate(
      cap_ratio = ifelse(pct_group_noale_Y <= 0 | is.na(pct_group_noale_Y), 0,
                         pmin(1, pct_group_X / pct_group_noale_Y)),
      skill_gain_Y_capped = skill_gain_Y * cap_ratio
    )
    
    # ---- FIX AME EXTRACTION (ALWAYS SCALARS) ----
    
    ameX_row <- gainX %>% summarise(
      AME   = dplyr::first(AME_X),
      lower = dplyr::first(lower_X),
      upper = dplyr::first(upper_X)
    )
    
    ameY_row <- gainY %>% summarise(
      AME   = dplyr::first(AME_Y),
      lower = dplyr::first(lower_Y),
      upper = dplyr::first(upper_Y)
    )
    
    fix_scalar <- function(x) if (length(x) == 0 || is.null(x)) NA_real_ else x
    
    ameX_out <- list(
      AME   = fix_scalar(ameX_row$AME),
      lower = fix_scalar(ameX_row$lower),
      upper = fix_scalar(ameX_row$upper)
    )
    
    ameY_out <- list(
      AME   = fix_scalar(ameY_row$AME),
      lower = fix_scalar(ameY_row$lower),
      upper = fix_scalar(ameY_row$upper)
    )
    
    list(
      df   = df_xy,
      ameX = ameX_out,
      ameY = ameY_out
    )
  })
    
  
  output$scatter <- renderPlot({
    r <- results()
    df_xy <- r$df
    
    p <- ggplot(df_xy, aes(
      x = skill_gain_X,
      y = skill_gain_Y_capped,
      label = iso3c,
      size = pct_group_X
    )) +
      geom_abline(slope = 1, intercept = 0, linetype="dotted", color="grey40") +
      geom_point(aes(color = pct_group_noale_Y), alpha = 0.9) +
      scale_size_continuous(range=c(1,6), labels=percent_format(accuracy=1)) +
      scale_color_gradient(low="lightblue", high="darkblue", labels=percent_format(accuracy=1)) +
      labs(
        x = "Gain from X targeting",
        y = "Gain from Y targeting\n(capped at size of X for each country)",
        size = "% X group",
        color = "% Y group\nno ALE",
        title = "Country gains: X vs Y"
      ) +
      geom_text_repel(size=3, max.overlaps=Inf) +
      theme_classic()
    
    p
})
  
  output$summary <- renderTable({
    r <- results()
    data.frame(
      target = c("X", "Y"),
      AME = c(r$ameX$AME, r$ameY$AME),
      lower = c(r$ameX$lower, r$ameY$lower),
      upper = c(r$ameX$upper, r$ameY$upper)
    )
  }, digits = 3)
  
}

shinyApp(ui, server)
