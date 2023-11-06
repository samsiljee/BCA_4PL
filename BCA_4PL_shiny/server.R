# A shiny application to run a 4PL analysis on BCA data
# Sam Siljee
# Created 30 October 2023

library(shiny)
library(stringr)
library(ggplot2)

server <- function(input, output, session) {
  # Make a matrix based on replication parameters
  blank_matrix <- reactive({
    if (input$direction == "columns") { # Make matrix for duplicates across columns
      blank_mat <- matrix(NA, 8, 12 / as.numeric(input$replicates))
      colnames(blank_mat) <- if (input$replicates == "1") {
        1:12
      } else {
        paste(
          seq(1, 12, by = as.numeric(input$replicates)),
          "-",
          seq(1, 12, by = as.numeric(input$replicates)) + as.numeric(input$replicates) - 1
        )
      }
      rownames(blank_mat) <- LETTERS[1:8]
      blank_mat
    } else { # Make matrix for duplicates across rows
      blank_mat <- matrix(NA, 8 / as.numeric(input$replicates), 12)
      colnames(blank_mat) <- 1:12
      rownames(blank_mat) <- switch(input$replicates,
        "1" = LETTERS[1:8],
        "2" = c("A - B", "C - D", "E - F", "G - H"),
        "3" = c("A - C", "D - F")
      )
      blank_mat
    }
  })

  # Create a data frame with individual coordinates
  coordinates <- reactive({
    expand.grid("rows" = 1:nrow(blank_matrix()), "cols" = 1:ncol(blank_matrix()))
  })

  # Create grid of UI elements for input
  output$grid_input <- renderUI({
    inputs <- lapply(1:nrow(coordinates()), function(unknown) {
      fluidRow(
        column(2, h5(paste("Unknown", unknown))),
        column(2, selectInput(
          paste0("type_", unknown),
          NULL,
          choices = c(
            "Sample", "Standard", "Blank"
          )
        )),
        column(2, textInput(
          paste0("name_", unknown),
          NULL,
          value = "Sample name"
        )),
        column(2,
               )
        )
    })
    do.call(tagList, inputs)
  })

  # Plot to display plate plan
  output$plate_plan_plot <- renderPlot({
    ggplot(coordinates(), aes(x = cols, y = rows)) +
      geom_point(color = "blue", size = 5) +
      labs(x = "Column", y = "Row") +
      theme_bw() +
      scale_y_reverse(
        breaks = 1:nrow(blank_matrix()),
        labels = function(x) rownames(blank_matrix())[x]
      ) +
      scale_x_continuous(
        breaks = 1:ncol(blank_matrix()),
        labels = function(x) colnames(blank_matrix())[x]
      ) +
      theme(
        panel.grid.minor = element_line(color = "gray", size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank()
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
