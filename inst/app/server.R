# Define server logic for the metabolomics data merge app
server <- function(input, output, session) {
  # Reactive expressions to read each uploaded file
  sampleInfo <- reactive({
    req(input$sampleInfo)
    read_data(input$sampleInfo$datapath)
  })
  posExp <- reactive({
    req(input$posExp)
    read_data(input$posExp$datapath)
  })
  posId <- reactive({
    req(input$posId)
    read_data(input$posId$datapath)
  })
  negExp <- reactive({
    req(input$negExp)
    read_data(input$negExp$datapath)
  })
  negId <- reactive({
    req(input$negId)
    read_data(input$negId$datapath)
  })

  # Reactive expressions for merged results in each mode
  result_pos <- reactive({
    req(sampleInfo(), posExp(), posId())
    easy_mergeMetabo(posExp(), posId(), mode = "pos", score_value = input$MS1_score_cutoff, sampleInfo())
  })
  result_neg <- reactive({
    req(sampleInfo(), negExp(), negId())
    easy_mergeMetabo(negExp(), negId(), mode = "neg", score_value = input$MS1_score_cutoff, sampleInfo())
  })

  # Reactive for merged POS/NEG result (only when checkbox is TRUE)
  merged_result <- reactive({
    req(result_pos(), result_neg())
    if (input$merge_result) {
      mergeMode(result_pos(), result_neg())
    } else {
      NULL
    }
  })

  # Render data tables in the UI
  output$result_pos_table <- renderDataTable({
    result_pos()
  })
  output$result_neg_table <- renderDataTable({
    result_neg()
  })
  output$merged_result <- renderDataTable({
    req(merged_result())
    merged_result()
  })

  # Download handlers for each output CSV
  output$download_pos <- downloadHandler(
    filename = function() { "result_pos.csv" },
    content = function(file) {
      write.csv(result_pos(), file, row.names = FALSE)
    }
  )
  output$download_neg <- downloadHandler(
    filename = function() { "result_neg.csv" },
    content = function(file) {
      write.csv(result_neg(), file, row.names = FALSE)
    }
  )
  output$download_merged <- downloadHandler(
    filename = function() { "result_merged.csv" },
    content = function(file) {
      req(merged_result())
      write.csv(merged_result(), file, row.names = FALSE)
    }
  )
}
