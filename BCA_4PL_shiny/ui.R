# A shiny application to runa  4PL analysis on BCA data
# Sam Siljee
# Created 30 October 2023

library(shiny)

fluidPage(
    # Application title
    titlePanel("BCA 4PL analysis"),
    selectInput("replicates", "Replicates:",
                choices = c(
                    "None" = 1,
                    "Duplicate" = 2,
                    "Triplicate" = 3),
                selected = 3),
    selectInput("direction", "Replicated across:",
                choices = c("Columns" = "columns",
                            "Rows" = "rows")),
    textAreaInput("raw", "Paste raw data here:"),
    plotOutput("plate_plan_plot"),
    fluidRow(column(1, h5("Index")),
             column(2, h5("Sample type")),
             column(2, h5("Sample name")),
             column(3, h5("Concentration"))),
    uiOutput("grid_input"),
    verbatimTextOutput("test_1"),
    verbatimTextOutput("test_2"),
    tableOutput("raw_table")

)
