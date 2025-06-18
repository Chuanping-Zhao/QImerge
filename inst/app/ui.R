# Load package
# library(metaboShinyApp)

# Define UI for the metabolomics data merge app
shiny::fluidPage(
  titlePanel("代谢物定量/定性合并分析"),  #

  sidebarLayout(
    sidebarPanel(
      fileInput("sampleInfo", "上传 sampleInfo.xlsx"),
      fileInput("posExp", "上传 POS Measurements.csv"),
      fileInput("posId", "上传 POS Identifications.csv"),
      fileInput("negExp", "上传 NEG Measurements.csv"),
      fileInput("negId", "上传 NEG Identifications.csv"),
      numericInput("MS1_score_cutoff", "MS1 Score Cutoff", value = 0, step = 0.1),
      checkboxInput("merge_result", "是否合并 POS/NEG", value = FALSE),
      br(),
      downloadButton("download_pos", "下载 POS 结果"),
      downloadButton("download_neg", "下载 NEG 结果"),
      downloadButton("download_merged", "下载合并结果")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("合并 POS/NEG 结果", dataTableOutput("merged_result")),
        tabPanel("POS 合并结果", dataTableOutput("result_pos_table")),
        tabPanel("NEG 合并结果", dataTableOutput("result_neg_table"))
      )
    )
  )
)
