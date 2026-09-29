# R/analytics/06_logistic_regression_module.R

logistic_regression_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      h4("Generalized Logistic Regression"),
      selectInput(ns("target_y"), "Dependent Variable Y (Categorical):", choices = NULL),
      selectizeInput(ns("predictors_x"), "Independent Variable(s) X:", choices = NULL, multiple = TRUE),
      hr(),
      actionButton(ns("run_glm"), "Fit Model", class = "btn-primary w-100")
    ),
    card(
      card_header("Model Output & Odds Ratios Table"),
      DTOutput(ns("glm_table")),
      hr(),
      verbatimTextOutput(ns("glm_details"))
    )
  )
}

logistic_regression_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    
    # Restrict Dependent Variable Y strictly to Categorical/Factor columns
    observe({
      req(shared_state$data)
      df <- shared_state$data
      
      cat_cols <- names(df)[vapply(df, function(col) is.character(col) || is.factor(col), logical(1))]
      all_cols <- names(df)
      
      updateSelectInput(session, "target_y", choices = cat_cols)
      updateSelectizeInput(session, "predictors_x", choices = all_cols)
    })
    
    # Execution Engine: Supports both Binary & Multinomial Logistic Regression
    glm_results <- eventReactive(input$run_glm, {
      req(shared_state$data, input$target_y, input$predictors_x)
      df <- shared_state$data
      
      # Clean missing data for selected columns
      cols_needed <- c(input$target_y, input$predictors_x)
      sub_df <- na.omit(df[, cols_needed, drop = FALSE])
      
      y_factor <- as.factor(sub_df[[input$target_y]])
      num_levels <- length(levels(y_factor))
      
      if (num_levels < 2) {
        showNotification("Target variable Y must have at least 2 unique levels.", type = "error")
        return(NULL)
      }
      
      # CASE 1: BINARY LOGISTIC REGRESSION (2 Levels)
      if (num_levels == 2) {
        f_glm <- as.formula(paste("as.factor(", input$target_y, ") ~", paste(input$predictors_x, collapse = " + ")))
        fit_glm <- glm(f_glm, data = sub_df, family = binomial(link = "logit"))
        s_glm <- summary(fit_glm)
        
        # Calculate Odds Ratios
        or_val <- exp(coef(fit_glm))
        ci_val <- suppressMessages(exp(confint.default(fit_glm)))
        
        df_res <- as.data.frame(s_glm$coefficients)
        df_res$Term <- rownames(df_res)         
        df_res$`Target Level` <- paste0(levels(y_factor)[2], " (vs ", levels(y_factor)[1], ")")
        df_res$`Odds Ratio (OR)` <- round(or_val, 4)
        df_res$`2.5 % CI` <- round(ci_val[, 1], 4)
        df_res$`97.5 % CI` <- round(ci_val[, 2], 4)
        
        colnames(df_res)[1:4] <- c("Estimate (Log-Odds)", "Std. Error", "Statistic", "p-value")
        
        df_res$`Significance` <- ifelse(df_res$`p-value` <= 0.001, "***",
                                 ifelse(df_res$`p-value` <= 0.01, "**",
                                 ifelse(df_res$`p-value` <= 0.05, "*", "NS")))
        
        final_df <- df_res[, c("Target Level", "Term", "Estimate (Log-Odds)", "Odds Ratio (OR)", "2.5 % CI", "97.5 % CI", "Statistic", "p-value", "Significance")]
        final_df$`Estimate (Log-Odds)` <- round(final_df$`Estimate (Log-Odds)`, 4)
        final_df$Statistic <- round(final_df$Statistic, 3)         
        final_df$`p-value` <- round(final_df$`p-value`, 5)
        
        details_str <- paste0("Model Type: Binary Logistic Regression\n",
                              "Baseline Reference Level: '", levels(y_factor)[1], "'\n",
                              "AIC: ", round(s_glm$aic, 2), 
                              " | Residual Deviance: ", round(s_glm$deviance, 2))
        
        model_type_str <- "Binary Logistic Regression"
      } 
      
      # CASE 2: MULTINOMIAL LOGISTIC REGRESSION (3+ Levels)
      else {
        f_multi <- as.formula(paste("as.factor(", input$target_y, ") ~", paste(input$predictors_x, collapse = " + ")))
        
        # Capture stdout from multinom
        fit_multi <- suppressMessages(nnet::multinom(f_multi, data = sub_df, trace = FALSE))
        s_multi <- summary(fit_multi)
        
        # Compute Wald z-scores and p-values
        coeff_mat <- s_multi$coefficients
        se_mat <- s_multi$standard.errors
        z_mat <- coeff_mat / se_mat
        p_mat <- (1 - pnorm(abs(z_mat), 0, 1)) * 2
        or_mat <- exp(coeff_mat)
        
        # Reshape matrix output into a unified long data frame
        res_rows <- list()
        target_levels <- rownames(coeff_mat)
        
        for (lvl in target_levels) {
          terms <- colnames(coeff_mat)
          sub_res <- data.frame(
            `Target Level` = paste0(lvl, " (vs ", levels(y_factor)[1], ")"),
            Term = terms,
            `Estimate (Log-Odds)` = round(coeff_mat[lvl, ], 4),
            `Odds Ratio (OR)` = round(or_mat[lvl, ], 4),
            `2.5 % CI` = round(exp(coeff_mat[lvl, ] - 1.96 * se_mat[lvl, ]), 4),
            `97.5 % CI` = round(exp(coeff_mat[lvl, ] + 1.96 * se_mat[lvl, ]), 4),
            Statistic = round(z_mat[lvl, ], 3),
            `p-value` = round(p_mat[lvl, ], 5),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          
          sub_res$Significance <- ifelse(sub_res$`p-value` <= 0.001, "***",
                                  ifelse(sub_res$`p-value` <= 0.01, "**",
                                  ifelse(sub_res$`p-value` <= 0.05, "*", "NS")))
          
          res_rows[[lvl]] <- sub_res
        }
        
        final_df <- do.call(rbind, res_rows)
        
        details_str <- paste0("Model Type: Multinomial Logistic Regression\n",
                              "Outcome Categories (", num_levels, "): ", paste(levels(y_factor), collapse = ", "), "\n",
                              "Baseline Reference Level: '", levels(y_factor)[1], "'\n",
                              "Residual Deviance: ", round(fit_multi$deviance, 2), 
                              " | AIC: ", round(fit_multi$AIC, 2))
        
        model_type_str <- "Multinomial Logistic Regression"
      }
      
      # Log to shared_state for final report summary module
      shared_state$final_models[["Logistic Regression"]] <- data.frame(
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        Module    = "Logistic Regression",
        Action    = model_type_str,
        Details   = paste("Y:", input$target_y, "(", num_levels, "levels) ~ X:", paste(input$predictors_x, collapse = ", ")),
        stringsAsFactors = FALSE
      )
      
      list(table = final_df, details = details_str)
    })
    
    # Render DT Table
    output$glm_table <- renderDT({
      req(glm_results()$table)
      datatable(
        glm_results()$table, 
        rownames = FALSE, 
        options = list(pageLength = 15, scrollX = TRUE, dom = 'ftip')
      )
    })
    
    output$glm_details <- renderText({ glm_results()$details })
  })
}