# app.R

library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)
library(dplyr)
library(ggplot2)
library(corrplot)
library(DT)

# Source the module files
source("R/survey_module.R")
source("R/analytics_module.R")

# Non-interactive auth cache for public apps
options(gargle_oauth_cache = ".secrets")

ui <- page_navbar(
  title = "SURVLYTICS: Dynamic Survey Builder",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Pass the SAME module ID to both survey UIs
  nav_panel("1. Survey Builder", survey_builder_ui("survey_mod")),
  nav_panel("2. Active Survey", survey_active_ui("survey_mod")),
  nav_panel("3. Analytics", analytics_ui("analytics_mod"))
)

server <- function(input, output, session) {
  
  # Centralized State passed to both modules
  # Centralized State passed to both modules
  app_state <- reactiveValues(
    # Added 'Options' column here:
    schema = data.frame(Variable = character(), Prompt = character(), Type = character(), Options = character(), stringsAsFactors = FALSE),
    sheet_id = NULL,
    data = data.frame()
  )
  
  # Initialize module servers
  survey_server("survey_mod", app_state)
  analytics_server("analytics_mod", app_state)
}

shinyApp(ui, server)