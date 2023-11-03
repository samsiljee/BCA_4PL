# A shiyn application to runa  4PL analysis on BCA data
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
    selectInput("direction", "Duplicated across:",
                choices = c("Columns" = "columns",
                            "Rows" = "rows")),
    textAreaInput("raw", "Paste raw data here:"),
    plotlyOutput("selection_plot"),
    uiOutput("plate_plan_input"),
    tableOutput("raw_table")

)
