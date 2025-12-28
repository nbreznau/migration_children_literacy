library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(here)

# saved objects from 03_Analyses.Rmd

df2  <- readRDS(here("Shiny", "shiny_df2.rds"))
df2r <- readRDS(here("Shiny", "shiny_df2r.rds"))
m4   <- readRDS(here("Shiny", "shiny_m4.rds"))

shiny_helpers <- readRDS(here("Shiny", "shiny_helpers.rds"))

vars_raw           <- shiny_helpers$vars_raw
map_within         <- shiny_helpers$map_within
map_between        <- shiny_helpers$map_between
make_newdata_for_m4 <- shiny_helpers$make_newdata_for_m4
in_group           <- shiny_helpers$in_group
calc_ame_spec      <- shiny_helpers$calc_ame_spec
calc_country_gains <- shiny_helpers$calc_country_gains

# assumes m4, df2, df2r, and the helper functions are already defined/loaded

choices_01all <- list("All" = NA, "0" = 0, "1" = 1)

ui <- fluidPage(
  titlePanel("ALE Targeting Simulator (X vs Y)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("X group definition"),
      selectInput("x_female",   "female",   choices = choices_01all, selected = 0),
      selectInput("x_FORBORN",  "FORBORN",  choices = choices_01all, selected = 1),
      selectInput("x_FORLANG",  "FORLANG",  choices = choices_01all, selected = NA),
      selectInput("x_children", "children", choices = choices_01all, selected = NA),
      
      hr(),
      
      h4("Y group definition"),
      selectInput("y_female",   "female",   choices = choices_01all, selected = 1),
      selectInput("y_FORBORN",  "FORBORN",  choices = choices_01all, selected = 1),
      selectInput("y_FORLANG",  "FORLANG",  choices = choices_01all, selected = NA),
      selectInput("y_children", "children", choices = choices_01all, selected = NA),
      
      hr(),
      checkboxInput("label_countries", "Label countries", TRUE)
    ),
    
    mainPanel(
      plotOutput("scatter", height = 650),
      tableOutput("summary")
    )
  )
)

server <- function(input, output, session) {
  
  specX <- reactive({
    list(
      female   = as.integer(input$x_female),
      FORBORN  = as.integer(input$x_FORBORN),
      FORLANG  = as.integer(input$x_FORLANG),
      children = as.integer(input$x_children)
    )
  })
  
  specY <- reactive({
    list(
      female   = as.integer(input$y_female),
      FORBORN  = as.integer(input$y_FORBORN),
      FORLANG  = as.integer(input$y_FORLANG),
      children = as.integer(input$y_children)
    )
  })
  
  results <- reactive({
    ameX <- calc_ame_spec(m4, df2r, specX(), weight_var="aweight")
    ameY <- calc_ame_spec(m4, df2r, specY(), weight_var="aweight")
    
    gainX <- calc_country_gains(df2, specX(), AME=ameX$AME, weight_var="aweight") %>%
      rename(skill_gain_X = skill_gain, pct_group_X = pct_group, pct_group_noale_X = pct_group_noale)
    
    gainY <- calc_country_gains(df2, specY(), AME=ameY$AME, weight_var="aweight") %>%
      rename(skill_gain_Y = skill_gain, pct_group_Y = pct_group, pct_group_noale_Y = pct_group_noale)
    
    df_xy <- gainX %>% left_join(gainY, by="iso3c")
    
    list(df = df_xy, ameX = ameX, ameY = ameY)
  })
  
  output$scatter <- renderPlot({
    r <- results()
    df_xy <- r$df
    
    p <- ggplot(df_xy, aes(
      x = skill_gain_X,
      y = skill_gain_Y,
      label = iso3c,
      size = pct_group_X
    )) +
      geom_abline(slope = 1, intercept = 0, linetype="dotted", color="grey40") +
      geom_point(aes(color = pct_group_noale_Y), alpha = 0.9) +
      scale_size_continuous(range=c(1,6), labels=percent_format(accuracy=1)) +
      scale_color_gradient(low="lightblue", high="darkblue", labels=percent_format(accuracy=1)) +
      labs(
        x = "Gain from X targeting",
        y = "Gain from Y targeting",
        size = "% X group",
        color = "% Y group\nno ALE",
        title = "Country gains: X vs Y"
      ) +
      theme_classic()
    
    if (isTRUE(input$label_countries)) {
      p <- p + geom_text_repel(size=3, max.overlaps=Inf)
    }
    
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
