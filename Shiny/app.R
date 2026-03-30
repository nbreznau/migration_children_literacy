
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

# User Interface
choices_01all <- c("All" = "all", "0" = "0", "1" = "1")

ui <- fluidPage(
  
  titlePanel(
    tagList(
      "Targeting Adult Learning: Policy Intervention Simulator",
      tags$div(
        style = "font-size: 14px; margin-top: 4px;",
        "Average literacy gain at the country-level when targeting group X vs Group Y"
      ),
      tags$div(
        style = "font-size: 12px; color: #555;",
        "Estimated from PIAAC Cycle 2 data, 2022"
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "position: relative; min-height: 900px;", # Increased slightly for new input
      
      h4("Education Subpopulation"),
      radioButtons("education_level", label = NULL,
                   choices = c("All", "Non-tertiary", "Tertiary"),
                   selected = "All"
      ),
      tags$div(
        style = "font-size: 11px; color: #666; font-style: italic; margin-top: -10px; margin-bottom: 10px;",
        "Note: Population % metrics will reflect the share within this specific educational subset."
      ),
      
      hr(),
      
      h4("Treatment Intervention Scenarios"),
      radioButtons("intervention", label = NULL,
                   choiceNames = list(
                     HTML("<b>STRICT PARITY:</b> 50% of untreated in both groups, capped at absolute number of treated in X."),
                     HTML("<b>LIMITED REACH:</b> Treat 25% of untreated in X, and max 1.5x the absolute X treatment in Y."),
                     HTML("<b>BLANK CHECK:</b> Treat 75% of untreated in both groups.")
                   ),
                   choiceValues = list("equal", "limited", "universal"),
                   selected = "equal"
      ),
      
      hr(),
      
      h4("X group definition"),
      tags$div(
        style = "font-size: 12px; color: #444; margin-bottom: 8px;",
        HTML('<b>Guide:</b> 1 = yes, 0 = no, All = both')
      ),
      selectInput("x_female",   "Female",            choices = choices_01all, selected = "1"),
      selectInput("x_FORBORN",  "Foreign-born",      choices = choices_01all, selected = "1"),
      selectInput("x_FORLANG",  "Non-native speaker",choices = choices_01all, selected = "1"),
      selectInput("x_children", "Children",          choices = choices_01all, selected = "1"),
      
      hr(),
      
      h4("Y group definition"),
      selectInput("y_female",   "Female",            choices = choices_01all, selected = "1"),
      selectInput("y_FORBORN",  "Foreign-born",      choices = choices_01all, selected = "0"),
      selectInput("y_FORLANG",  "Non-native speaker",choices = choices_01all, selected = "0"),
      selectInput("y_children", "Children",          choices = choices_01all, selected = "1"),
      
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
        tags$img(src = "die.png", style = "width: 200px; height: auto;")
      )
    ),
    
    mainPanel(
      h4("Country Details", style = "margin-top: 0px;"),
      p(HTML("<b>Instruction:</b> Click on a country's dot in the scatterplot below to see detailed group statistics. (Defaults to Germany - DEU).")),
      plotOutput("scatter", height = 650, click = "plot_click"),
      downloadButton("download_plot", "Download PNG"),
      
      hr(),
      
      h4(textOutput("selected_country_title")),
      tableOutput("country_detail_table"),
      
      uiOutput("dnk_footnote")
    )
  )
)

server <- function(input, output, session) {
  
  selected_country <- reactiveVal("DEU")
  
  observeEvent(input$plot_click, {
    r <- results()
    res <- nearPoints(r$df, input$plot_click, xvar = "plot_gain_X", yvar = "plot_gain_Y", maxpoints = 1)
    if (nrow(res) > 0) {
      selected_country(res$iso3c[1]) 
    }
  })
  
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
    
    # Filter by BOTH the spec_id and the selected ed_level
    gainX <- precomp %>% 
      filter(spec_id == idX & ed_level == input$education_level) %>%
      rename(skill_gain_X = skill_gain,
             pct_group_X = pct_group,
             pct_group_noale_X = pct_group_noale,
             group_skill_base_X = group_skill_base,
             context_effect_X = context_effect, 
             AME_X = AME, lower_X = lower, upper_X = upper)
    
    gainY <- precomp %>% 
      filter(spec_id == idY & ed_level == input$education_level) %>%
      rename(skill_gain_Y = skill_gain,
             pct_group_Y = pct_group,
             pct_group_noale_Y = pct_group_noale,
             group_skill_base_Y = group_skill_base,
             context_effect_Y = context_effect, 
             AME_Y = AME, lower_Y = lower, upper_Y = upper)
    
    df_xy <- gainX %>% left_join(gainY, by="iso3c")
    
    if (input$x_FORLANG %in% c("0", "1") || input$y_FORLANG %in% c("0", "1")) {
      df_xy <- df_xy %>% filter(iso3c != "DNK")
    }
    
    df_xy <- df_xy %>% mutate(
      treated_fraction_X = case_when(
        input$intervention == "universal" ~ 0.75,
        input$intervention == "equal" ~ 0.50,
        TRUE ~ 0.25 
      ),
      treated_share_X = pct_group_noale_X * treated_fraction_X,
      
      treated_fraction_Y = case_when(
        input$intervention == "universal" ~ 0.75,
        input$intervention == "equal" ~ ifelse(pct_group_noale_Y > 0, pmin(1, treated_share_X / pct_group_noale_Y), 0),
        TRUE ~ ifelse(pct_group_noale_Y > 0, pmin(1, (1.5 * treated_share_X) / pct_group_noale_Y), 0) 
      ),
      treated_share_Y = pct_group_noale_Y * treated_fraction_Y,
      
      plot_gain_X = skill_gain_X * treated_fraction_X,
      plot_gain_Y = skill_gain_Y * treated_fraction_Y
    )
    
    df_xy <- df_xy %>% mutate(
      cap_ratio = case_when(
        input$intervention == "universal" ~ 1,
        pct_group_noale_Y <= 0 | is.na(pct_group_noale_Y) ~ 0,
        input$intervention == "double" ~ pmin(1, (2 * pct_group_noale_X) / pct_group_noale_Y),
        TRUE ~ pmin(1, pct_group_noale_X / pct_group_noale_Y) 
      ),
      skill_gain_Y_capped = skill_gain_Y * cap_ratio
    )
    
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
    
    ameX_out <- list(AME = fix_scalar(ameX_row$AME), lower = fix_scalar(ameX_row$lower), upper = fix_scalar(ameX_row$upper))
    ameY_out <- list(AME = fix_scalar(ameY_row$AME), lower = fix_scalar(ameY_row$lower), upper = fix_scalar(ameY_row$upper))
    
    list(df = df_xy, ameX = ameX_out, ameY = ameY_out)
  })
  
  output$selected_country_title <- renderText({
    paste("Detailed Statistics for:", selected_country(), "(", input$education_level, ")")
  })
  
  output$country_detail_table <- renderTable({
    req(selected_country()) 
    
    r <- results()
    df_xy <- r$df
    c_data <- df_xy %>% filter(iso3c == selected_country())
    
    if(nrow(c_data) == 0) return(NULL) 
    
    frac_X <- c_data$treated_fraction_X
    frac_Y <- c_data$treated_fraction_Y
    
    treated_country_share_X <- c_data$treated_share_X
    treated_country_share_Y <- c_data$treated_share_Y
    
    pct_noale_X <- ifelse(c_data$pct_group_X > 0, c_data$pct_group_noale_X / c_data$pct_group_X, 0)
    pct_noale_Y <- ifelse(c_data$pct_group_Y > 0, c_data$pct_group_noale_Y / c_data$pct_group_Y, 0)
    
    net_effect_X <- max(0, c_data$AME_X + c_data$context_effect_X)
    net_effect_Y <- max(0, c_data$AME_Y + c_data$context_effect_Y)
    
    group_treated_pct_X <- pct_noale_X * frac_X
    group_treated_pct_Y <- pct_noale_Y * frac_Y
    
    post_treat_X <- c_data$group_skill_base_X + (net_effect_X * group_treated_pct_X)
    post_treat_Y <- c_data$group_skill_base_Y + (net_effect_Y * group_treated_pct_Y)
    
    data.frame(
      Metric = c(
        "Average Treatment Effect (AME)",
        "Institutional Context (Deviation from Global Avg)",
        "Net Treatment Effect (Floored at 0)",
        paste0("% of ", input$education_level, " Population in Group"),
        "% of Group without ALE in last 12m",
        paste0("Policy Intervention Size (% of ", input$education_level, " Pop. Treated)"),
        "Average Group Literacy (Pre-intervention)",
        "Expected Group Literacy (Post-intervention)"
      ),
      `Group X` = c(
        sprintf("+%.2f points", c_data$AME_X),
        sprintf("%+.2f points", c_data$context_effect_X),
        sprintf("+%.2f points", net_effect_X),
        sprintf("%.1f%%", c_data$pct_group_X * 100),
        sprintf("%.1f%%", pct_noale_X * 100),
        sprintf("%.2f%%", treated_country_share_X * 100), 
        sprintf("%.1f", c_data$group_skill_base_X), 
        sprintf("%.1f", post_treat_X)
      ),
      `Group Y` = c(
        sprintf("+%.2f points", c_data$AME_Y),
        sprintf("%+.2f points", c_data$context_effect_Y),
        sprintf("+%.2f points", net_effect_Y),
        sprintf("%.1f%%", c_data$pct_group_Y * 100),
        sprintf("%.1f%%", pct_noale_Y * 100),
        sprintf("%.2f%%", treated_country_share_Y * 100), 
        sprintf("%.1f", c_data$group_skill_base_Y), 
        sprintf("%.1f", post_treat_Y)
      ),
      check.names = FALSE 
    )
  }, align = "lcc") 
  
  output$scatter <- renderPlot({
    r <- results()
    df_xy <- r$df
    
    max_gain <- max(c(df_xy$plot_gain_X, df_xy$plot_gain_Y), na.rm = TRUE)
    
    ggplot(df_xy, aes(
      x = plot_gain_X, 
      y = plot_gain_Y,  
      label = iso3c,
      size = pct_group_X
    )) +
      geom_abline(slope = 1, intercept = 0, linetype="dotted", color="grey40", linewidth = 1) +
      geom_point(aes(color = pct_group_noale_Y), alpha = 0.9) +
      scale_size_continuous(range=c(3,10), labels=scales::label_percent(accuracy=1)) +
      scale_color_gradient(low="lightblue", high="darkblue", labels=scales::label_percent(accuracy=1)) +
      coord_fixed(xlim = c(0, max_gain), ylim = c(0, max_gain)) + 
      labs(
        x = "Gain from X targeting",
        y = "Gain from Y targeting",
        size = paste0("% X group\n(", input$education_level, ")"),
        color = "% Y group\nno ALE",
        title = paste("Country gains: X vs Y (", input$education_level, ")")
      ) +
      geom_text_repel(size=5, max.overlaps=Inf) +
      theme_classic(base_size = 16) +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("ale_plot_", input$education_level, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      r <- results()
      df_xy <- r$df
      
      max_gain <- max(c(df_xy$plot_gain_X, df_xy$plot_gain_Y), na.rm = TRUE)
      
      p <- ggplot(df_xy, aes(x = plot_gain_X, y = plot_gain_Y, label = iso3c, size = pct_group_X)) +
        geom_abline(slope = 1, intercept = 0, linetype="dotted", color="grey40", linewidth = 1) +
        geom_point(aes(color = pct_group_noale_Y), alpha = 0.9) +
        scale_size_continuous(range=c(3,10), labels=percent_format(accuracy=1)) +
        scale_color_gradient(low="lightblue", high="darkblue", labels=percent_format(accuracy=1)) +
        coord_fixed(xlim = c(0, max_gain), ylim = c(0, max_gain)) + 
        labs(
          x = "Gain from X targeting", y = "Gain from Y targeting",
          size = paste0("% X group\n(", input$education_level, ")"),
          color = "% Y group\nno ALE",
          title = paste("Country gains: X vs Y (", input$education_level, ")")
        ) +
        geom_text_repel(size=5, max.overlaps=Inf) +
        theme_classic(base_size = 16) +
        theme(plot.title = element_text(size = 20, face = "bold"))
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
    }
  )
  
  output$dnk_footnote <- renderUI({
    if (input$x_FORLANG %in% c("0", "1") || input$y_FORLANG %in% c("0", "1")) {
      tags$div(
        style = "font-size: 11px; color: #666; font-style: italic; margin-top: 8px;",
        "* DNK dropped from plot and analysis due to missing data on the foreign-language question."
      )
    }
  })
}

shinyApp(ui, server)