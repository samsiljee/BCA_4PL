# A shiny application to runa  4PL analysis on BCA data
# Sam Siljee
# Created 30 October 2023

library(shiny)

fluidPage(
  # Application title
  titlePanel("BCA analysis"),
  fluidRow(
    column(3, selectInput("replicates", "Replicates:",
      choices = c(
        "None" = 1,
        "Duplicate" = 2,
        "Triplicate" = 3
      ),
      selected = 3
    )),
    column(3, selectInput("direction", "Replicated across:",
      choices = c(
        "Columns" = "columns",
        "Rows" = "rows"
      )
    ))
  ),
  textAreaInput("raw", "Paste whole plate reading here:"),
  h4("Plate plan:"),
  plotOutput("plate_plan_plot", height = "500px"),
  fluidRow(
    column(1, h5("Index")),
    column(2, h5("Type")),
    column(3, h5("Sample name")),
    column(2, h5("Concentration")),
    column(2, h5("Dilution factor"))
  ),
  uiOutput("grid_input"),
  uiOutput("water_blank"),
  uiOutput("buffer_blank"),
  selectInput(
    "model_to_use",
    "Model to use:",
    choices = c(
      "4 parameter log-logistic" = "4PL",
      "5 parameter log-logistic" = "5PL",
      "Linear" = "LM"
    )
  ),
  h4("Results:"),
  tableOutput("results"),
  downloadButton("download_results", "Download")
)
