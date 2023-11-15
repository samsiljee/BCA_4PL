# A shiny application to run a 4PL analysis on BCA data
# Sam Siljee
# Created 30 October 2023

library(shiny)
library(tidyverse)
library(wesanderson)

colour_pallet <- wes_palettes$AsteroidCity3[c(4, 3, 1, 2)]

server <- function(input, output, session) {
  # Display annotations to test
  output$test_1 <- renderPrint(print(metadata_long(), n = 100))
  output$test_2 <- renderPrint(print(absorbance_long(), n = 100))

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
      mutate(
        Row_name = rownames(blank_matrix()[rows, ]),
        Col_name = colnames(blank_matrix()[, cols])
      )

    df
  })

  # Make a long version of metadata to work with long absorbance data
  metadata_long <- reactive({
    if (input$replicates == "1") { # Return unchanged metadata table if not replicated
      df <- metadata()
    } else { # Handle case for missing rows if experiment done in triplicate with replicates across rows
      if (input$replicates == "3" & input$direction == "rows") {
        # Repeat metadata for triplicates
        df <- rbind( # Triplicate
          metadata(),
          metadata(),
          metadata()) %>%
          arrange(Index)
        # Correct Row_name
        df <- df %>% mutate(
          Row_name = c(
            rep(LETTERS[1:3], 12),
            rep(LETTERS[4:6], 12)
          )
        )
        # Add rows to make up to 96
        df <- rbind(df,
          data.frame(
            Index = 0,
            Type = "Unused",
            Name = NA,
            Concentration = NA,
            cols = NA,
            rows = NA,
            Label = NA,
            Row_name = rep(LETTERS[7:8], 12),
            Col_name = rep(1:12, each = 2)
          ))
        
        df
      } else {
        # Replicate data.frame for replicates
        df <- switch(input$replicates,
                     "1" = metadata(),
          "2" = rbind(metadata(), metadata()),
          "3" = rbind(metadata(), metadata(), metadata())
        )
        # Sort by index
        df <- arrange(df, Index)
        # Add columns and rows as needed
        if (input$direction == "columns") {
          df <- df %>%
            mutate(Col_name = switch(
              input$replicates,
              "2" = c(
                rep(1:2, 8),
                rep(3:4, 8),
                rep(5:6, 8),
                rep(7:8, 8),
                rep(9:10, 8),
                rep(11:12, 8)),
              "3" = c(
                rep(1:3, 8),
                rep(4:6, 8),
                rep(7:9, 8),
                rep(10:12, 8))
            )
            )
        } else {
          df <- df %>%
            mutate(Row_name = c(
              rep(LETTERS[1:2], 12),
              rep(LETTERS[3:4], 12),
              rep(LETTERS[5:6], 12),
              rep(LETTERS[7:8], 12)
            )
            )
        }
        
        df
      }
    }
  })
  
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

  # Function to reshape absorbance data
  reshape_absorbance <- function(dat, direction, replicates) {
    dat_long <- dat %>%
      as_tibble(rownames = "Row") %>%
      pivot_longer(cols = -Row, names_to = "Column", values_to = "Absorbance")

    return(dat_long)
  }

  # Reactive to reshape absorbance data
  absorbance_long <- reactive({
    reshape_absorbance(absorbance(), input$direction, input$replicates)
  })

  # Render raw absorbance data table
  output$raw_table <- renderTable(absorbance(),
    rownames = TRUE
  )
}
