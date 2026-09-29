summary_meta_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1/3,
      value_box(title = "Total Respondents", value = textOutput(ns("total_rows")), showcase = icon("users")),
      value_box(title = "Total Variables", value = textOutput(ns("total_cols")), showcase = icon("list")),
      value_box(title = "Completion Rate", value = textOutput(ns("complete_rate")), showcase = icon("check-circle"))
    ),
    accordion(
      accordion_panel("Raw Dataset View", DTOutput(ns("raw_table"))),
      accordion_panel("Metadata & Schema Inspection", DTOutput(ns("meta_table")))
    )
  )
}

summary_meta_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    
    output$total_rows <- renderText({ nrow(shared_state$data) })
    output$total_cols <- renderText({ ncol(shared_state$data) })
    output$complete_rate <- renderText({
      req(shared_state$data)
      if(nrow(shared_state$data) == 0) return("0%")
      rate <- mean(complete.cases(shared_state$data)) * 100
      paste0(round(rate, 1), "%")
    })
    
    output$raw_table <- renderDT({
      req(shared_state$data)
      datatable(shared_state$data, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$meta_table <- renderDT({
      req(shared_state$data)
      df <- shared_state$data
      meta <- data.frame(
        `Column Name` = names(df),
        `Data Type` = sapply(df, class),
        `Missing Values` = sapply(df, function(x) sum(is.na(x))),
        `Unique Values` = sapply(df, function(x) length(unique(x))),
        check.names = FALSE
      )
      datatable(meta, options = list(dom = 't'))
    })
  })
}