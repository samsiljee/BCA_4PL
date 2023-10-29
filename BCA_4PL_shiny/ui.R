# A shiyn application to runa  4PL analysis on BCA data
# Sam Siljee
# Created 30 October 2023

library(shiny)

fluidPage(
    # Application title
    titlePanel("BCA 4PL analysis"),
    selectInput("replicates", "Select replicates",
                choices = c("Duplicate" = "duplicate",
                            "Triplicate" = "triplicate")),
    textAreaInput("raw", "Copy raw data here"),
    tableOutput("raw_table")

)
