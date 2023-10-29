# A shiyn application to runa  4PL analysis on BCA data
# Sam Siljee
# Created 30 October 2023

library(shiny)
library(stringr)

function(input, output, session) {
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
        for(i in 1:length(rows_list)) {
            current_row <- rows_list[i] %>%
                str_split(pattern = "\t") %>%
                unlist
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
                                    rownames = TRUE)
    
}
