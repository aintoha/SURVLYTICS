# R/analytics/05_linear_regression_module.R

linear_regression_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      h4("Linear Regression"),
      selectInput(ns("target_y"), "Dependent Variable Y (Continuous only):", choices = NULL),
      selectizeInput(ns("predictors_x"), "Independent Variable(s) X:", choices = NULL, multiple = TRUE),
      hr(),
      actionButton(ns("run_lm"), "Fit Regression Model(s)", class = "btn-primary w-100")
    ),
    card(
      card_header("Model Summary Table"),
      DTOutput(ns("lm_table")),
      hr(),
      verbatimTextOutput(ns("lm_details"))
    )
  )
}

linear_regression_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    
    # Restrict Dependent Variable Y strictly to numeric variables
    observe({
      req(shared_state$data)
      df <- shared_state$data
      
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      all_cols <- names(df)
      
      updateSelectInput(session, "target_y", choices = num_cols)
      updateSelectizeInput(session, "predictors_x", choices = all_cols)
    })
    
    # Store Fit Results
    lm_results <- eventReactive(input$run_lm, {
      req(shared_state$data, input$target_y, input$predictors_x)
      df <- shared_state$data
      
      combined_results <- data.frame()
      summary_notes <- c()
      
      # 1. If multiple predictors selected, run simple regression for EACH individual predictor first
      if (length(input$predictors_x) > 1) {
        summary_notes <- c(summary_notes, "--- INDIVIDUAL SIMPLE LINEAR REGRESSIONS ---")
        
        for (pred in input$predictors_x) {
          f_ind <- as.formula(paste(input$target_y, "~", pred))
          fit_ind <- lm(f_ind, data = df)
          s_ind <- summary(fit_ind)$coefficients
          
          df_ind <- as.data.frame(s_ind)
          df_ind$Term <- rownames(df_ind)           
          df_ind$`Model Scope` <- paste("Individual:", pred)
          combined_results <- rbind(combined_results, df_ind)
        }
      }
      
      # 2. Run Combined Multiple Linear Regression
      f_multi <- as.formula(paste(input$target_y, "~", paste(input$predictors_x, collapse = " + ")))
      fit_multi <- lm(f_multi, data = df)
      s_multi <- summary(fit_multi)
      
      df_multi <- as.data.frame(s_multi$coefficients)
      df_multi$Term <- rownames(df_multi)       
      df_multi$`Model Scope` <- if(length(input$predictors_x) > 1) "Multiple Linear Regression" else "Simple Linear Regression"              
      combined_results <- rbind(combined_results, df_multi)              # Format Data Frame Output       
      colnames(combined_results)[1:4] <- c("Estimate", "Std. Error", "Statistic", "p-value")              
      combined_results$`Significance` <- ifelse(combined_results$`p-value` <= 0.001, "***",
                                         ifelse(combined_results$`p-value` <= 0.01, "**",
                                         ifelse(combined_results$`p-value` <= 0.05, "*", "NS")))
      
      # Format columns and order
      final_df <- combined_results[, c("Model Scope", "Term", "Estimate", "Std. Error", "Statistic", "p-value", "Significance")]
      final_df$Estimate <- round(final_df$Estimate, 4)       
      final_df$`Std. Error` <- round(final_df$`Std. Error`, 4)
      final_df$Statistic <- round(final_df$Statistic, 3)       
      final_df$`p-value` <- round(final_df$`p-value`, 5)
      
      # Log Details & R-Squared Info
      details_str <- paste0("Multiple R-Squared: ", round(s_multi$r.squared, 4), 
                            " | Adjusted R-Squared: ", round(s_multi$adj.r.squared, 4),
                            "\nOverall Model p-value: ", round(pf(s_multi$fstatistic[1], s_multi$fstatistic[2], s_multi$fstatistic[3], lower.tail = FALSE), 5))
      
      # Cache for Final Summary Report Module
      shared_state$final_models[["Linear Regression"]] <- data.frame(
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        Module    = "Linear Regression",
        Action    = if(length(input$predictors_x) > 1) "Individual & Multiple Linear Regression" else "Simple Linear Regression",
        Details   = paste("Y:", input$target_y, "~ X:", paste(input$predictors_x, collapse = ", "), "| Adj R2 =", round(s_multi$adj.r.squared, 3)),
        stringsAsFactors = FALSE
      )
      
      list(table = final_df, details = details_str)
    })
    
    # Render DT Table
    output$lm_table <- renderDT({
      req(lm_results()$table)
      datatable(lm_results()$table, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = 't'))
    })
    
    output$lm_details <- renderText({ lm_results()$details })
  })
}