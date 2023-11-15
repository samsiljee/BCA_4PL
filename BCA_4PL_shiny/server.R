# A shiny application to run a 4PL analysis on BCA data
# Sam Siljee
# Created 30 October 2023

library(shiny)
library(stringr)
library(ggplot2)
library(dplyr)
library(wesanderson)

colour_pallet <- wes_palettes$AsteroidCity3[c(4,3,1,2)]

server <- function(input, output, session) {

  # Annotations ----
  
  # Create a data frame with individual coordinates
  coordinates <- reactive({
    switch(input$direction,
           "columns" = {
             df <- expand.grid("rows" = 1:nrow(blank_matrix()), "cols" = 1:ncol(blank_matrix()))
             df$Index <- 1:nrow(df)
             df
           },
           "rows" = {
             df <- expand.grid("cols" = 1:ncol(blank_matrix()), "rows" = 1:nrow(blank_matrix()))
             df$Index <- 1:nrow(df)
             df
           }
    )
  })
  
  # Initialise blank dataframe for grid input
  annotations <- reactiveVal(data.frame())
  
  # Create a dataframe reading in the values of the grid input
  annotations <- reactive({
    data <- data.frame()
    
    # Loop through the number of rows in coordinates
    for (index in 1:nrow(coordinates())) {
      current_row <- data.frame(
        Index = index,
        Type = factor(input[[paste0("type_", index)]], levels = c("Sample", "Standard", "Blank", "Unused")),
        Name = input[[paste0("name_", index)]],
        Concentration = input[[paste0("concentration_", index)]]
      )
      
      data <- rbind(data, current_row)
    }
    
    # Return the dataframe
    return(data)
  })
  
  # Combine annotations with coordinates to make metadata table
  metadata <- reactive({
    df <- merge(annotations(), coordinates(), by = "Index")
    
    # Generate labels, rows names, and col names
    df <- df %>%
      group_by(Type) %>%
      mutate(Label = paste(Type, row_number())) %>%
      ungroup() %>%
      mutate(Row_name = rownames(blank_matrix()[rows,]),
             Col_name = colnames(blank_matrix()[,cols]))
    
    df
  })
  
  # Display annotations to test
  output$test_1 <- renderPrint(print(metadata(), n = 100))
  output$test_2 <- renderPrint(blank_matrix())
  
  # Plate plan plot ----
  
  # Make a matrix to display plate plan based on replication parameters
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
  
  # Plot to display plate plan
  output$plate_plan_plot <- renderPlot({
    ggplot(metadata(), aes(x = cols, y = rows, label = Label, color = Type)) +
      geom_point(size = 5) +
      geom_text(col = "black", vjust = 1.5, hjust = 0.5) +
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
      ) +
      scale_color_manual(values = colour_pallet)
  })

  # Reactive UI ----
  
  test_vector <- reactive({
    1:nrow(metadata())
  })
  
  # Create grid of UI elements for input
  output$grid_input <- renderUI({
    inputs <- lapply(1:nrow(coordinates()), function(index) {
      fluidRow(
        column(1, h5(index)),
        column(2, selectInput(
          paste0("type_", index),
          NULL,
          choices = c(
            "Sample", "Standard", "Blank", "Unused"
          )
        )),
        column(2, textInput(
          paste0("name_", index),
          NULL
        )),
        column(2, numericInput(
          paste0("concentration_", index),
          NULL,
          value = NA
        ))
      )
    })
    do.call(tagList, inputs)
  })

  # Input ----
  
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
