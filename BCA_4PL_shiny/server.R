# A shiyn application to runa  4PL analysis on BCA data
# Sam Siljee
# Created 30 October 2023

library(shiny)
library(stringr)
library(shinyMatrix)

function(input, output, session) {
  # reactive UI for plate plan input
  output$plate_plan_input <- renderUI({
    matrixInput("plate_plan",
      "Plate plan:",
      value = {
        if (input$direction == "columns") { # Make matrix for duplicates across columns
          blank_mat <- matrix(NA, 8, 12 / as.numeric(input$replicates))
          colnames(blank_mat) <- if (input$replicates == "1") {
            1:12
          } else {
            paste(seq(1, 12, by = as.numeric(input$replicates)),
                  "-",
                  seq(1, 12, by = as.numeric(input$replicates)) + as.numeric(input$replicates) - 1)
          }
          rownames(blank_mat) <- LETTERS[1:8]
          blank_mat
        } else { # Make matrix for duplicates across rows
          blank_mat <- matrix(NA, 8 / as.numeric(input$replicates), 12)
          colnames(blank_mat) <- 1:12
          rownames(blank_mat) <- switch(
              input$replicates,
              "1" = LETTERS[1:8],
              "2" = c("A - B", "C - D", "E - F", "G - H"),
              "3" = c("A - C", "D - F")
          )
          blank_mat
        }
      }
    )
  })

  # Load raw absorbance data from text input
  absorbance <- reactive({
    # Initialise blank data.frame
    dat <- data.frame()

    # Read in rows from text input
    rows_list <- input$raw %>%
      str_trim() %>%
      str_split(pattern = "\n") %>%
      unlist()

    # Convert to a table
    for (i in 1:length(rows_list)) {
      current_row <- rows_list[i] %>%
        str_split(pattern = "\t") %>%
        unlist()
      dat <- rbind(dat, current_row)
    }

    # Add row names
    rownames(dat) <- LETTERS[1:nrow(dat)]

    # Add column names
    colnames(dat) <- 1:ncol(dat)

    # Return the table
    return(dat)
  })

  # Render raw absorbance data table
  output$raw_table <- renderTable(absorbance(),
    rownames = TRUE
  )
}
