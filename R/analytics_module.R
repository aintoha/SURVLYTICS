# R/analytics_module.R

analytics_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      actionButton(ns("refresh_data"), "Refresh Cloud Data", icon = icon("sync")),
      hr(),
      h4("Modeling"),
      uiOutput(ns("modeling_ui")),
      actionButton(ns("run_models"), "Run Analysis", class = "btn-info"),
      hr(),
      h4("Export"),
      textInput(ns("drive_folder"), "Google Drive Folder URL:"),
      actionButton(ns("export_plot"), "Save Plot to Drive", icon = icon("cloud-upload-alt"))
    ),
    navset_card_tab(
      nav_panel("Summary Stats", 
                h5("Raw Data"), DTOutput(ns("data_table")),
                h5("Summaries"), verbatimTextOutput(ns("summary_stats"))),
      nav_panel("Hypothesis Testing", verbatimTextOutput(ns("ht_results"))),
      nav_panel("Correlation", plotOutput(ns("corr_plot"))),
      nav_panel("Regression", verbatimTextOutput(ns("lm_results")))
    )
  )
}

analytics_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Local reactive value to store the plot for Drive export
    local_state <- reactiveValues(last_plot = NULL)
    
    # --- 1. DATA PULLING ---
    observeEvent(input$refresh_data, {
      req(shared_state$sheet_id)
      shared_state$data <- read_sheet(shared_state$sheet_id)
      showNotification("Data synchronized.", type = "message")
    })
    
    output$data_table <- renderDT({ datatable(shared_state$data) })
    
    # --- 2. DESCRIPTIVE STATS ---
    output$summary_stats <- renderPrint({
      req(nrow(shared_state$data) > 0)
      cat("--- Quantitative Summary ---\n")
      print(summary(shared_state$data %>% select(where(is.numeric))))
      
      cat("\n--- Categorical/Character Summary ---\n")
      char_data <- shared_state$data %>% select(where(is.character))
      if(ncol(char_data) > 0) {
        lapply(char_data, table)
      } else { cat("No categorical data found.") }
    })
    
    # --- 3. MODELING & HYPOTHESIS TESTING ---
    output$modeling_ui <- renderUI({
      req(nrow(shared_state$data) > 0)
      cols <- names(shared_state$data)[names(shared_state$data) != "Timestamp"]
      tagList(
        selectInput(ns("target_y"), "Target Variable (Y):", choices = cols),
        selectizeInput(ns("predictors_x"), "Predictors (X):", choices = cols, multiple = TRUE)
      )
    })
    
    observeEvent(input$run_models, {
      req(input$target_y, input$predictors_x, nrow(shared_state$data) > 1)
      
      output$ht_results <- renderPrint({
        if (length(input$predictors_x) == 1) {
          formula_str <- paste(input$target_y, "~", input$predictors_x)
          res <- aov(as.formula(formula_str), data = shared_state$data)
          print(summary(res))
        } else {
          cat("Please select exactly one predictor for simple hypothesis testing (ANOVA).")
        }
      })
      
      output$lm_results <- renderPrint({
        formula_str <- paste(input$target_y, "~", paste(input$predictors_x, collapse = " + "))
        model <- lm(as.formula(formula_str), data = shared_state$data)
        print(summary(model))
      })
    })
    
    # --- 4. VISUALIZATION & EXPORT ---
    output$corr_plot <- renderPlot({
      req(nrow(shared_state$data) > 1)
      num_data <- shared_state$data %>% select(where(is.numeric)) %>% na.omit()
      req(ncol(num_data) > 1)
      
      M <- cor(num_data)
      local_state$last_plot <- recordPlot() 
      corrplot(M, method = "color", type = "upper", addCoef.col = "black", tl.col = "black")
    })
    
    observeEvent(input$export_plot, {
      req(input$drive_folder, local_state$last_plot)
      tryCatch({
        temp_file <- tempfile(fileext = ".png")
        png(temp_file, width = 800, height = 600)
        replayPlot(local_state$last_plot)
        dev.off()
        
        folder_id <- as_id(input$drive_folder)
        drive_upload(media = temp_file, path = folder_id, name = paste0("Survey_Plot_", Sys.Date(), ".png"))
        
        showNotification("Plot uploaded to Google Drive!", type = "message")
      }, error = function(e) {
        showNotification(paste("Export failed:", e$message), type = "error")
      })
    })
  })
}