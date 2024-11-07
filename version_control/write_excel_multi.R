#' Write Multiple Data Frames to Excel with Styling
#'
#' This function writes multiple data frames to an Excel file, with each data frame
#' placed on a separate sheet. The sheets are styled for publication-ready format,
#' including column width adjustments, header styling, and data formatting.
#'
#' @param dataframes A list of data frames to be written to the Excel file.
#' @param file_path The file path (including name and extension) where the Excel file will be saved.
#' @param sheet_names Optional. A vector of sheet names for each data frame. If not provided, 
#' default names ("Sheet1", "Sheet2", etc.) are used.
#' @param header_style_type The style for the header row. Options are "blue_bg", "green_bg", or "dark_grey_bg" (default: "dark_grey_bg").
#'
#' @return An Excel file containing the provided data frames, saved at the specified file path.
#'
#' @import openxlsx
write_excel_multi <- function(dataframes,
                              file_path,
                              sheet_names = NULL,
                              header_style_type = "dark_grey_bg") {
  
  # Load necessary package
  library(openxlsx)
  
  # Step 1: Create a new workbook
  wb <- openxlsx::createWorkbook()
  
  # Step 2: Loop through each data frame and add it as a new sheet
  for (i in seq_along(dataframes)) {
    df <- dataframes[[i]]
    
    # Determine sheet name
    sheet_name <- if (!is.null(sheet_names) && length(sheet_names) >= i) {
      sheet_names[[i]]
    } else {
      paste0("Sheet", i)
    }
    
    # Add a new worksheet with the specified name
    openxlsx::addWorksheet(wb, sheet_name)
    
    # Write the data frame to the worksheet
    openxlsx::writeData(wb, sheet_name, df)
    
    # Automatically adjust column widths based on content
    column_widths <- sapply(df, function(col) {
      if (is.numeric(col)) {
        formatted_col <- format(col, scientific = FALSE, digits = 10)
        max_width <- max(nchar(formatted_col)) * 1.3
      } else {
        max_width <- max(nchar(as.character(col))) * 1.3
        max_width <- min(max_width, 70)  # Limit max column width to 70
      }
      max(max_width, nchar(colnames(df)))
    })
    
    openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = column_widths)
    
    # Apply header style based on the selected type
    header_style <- switch(
      header_style_type,
      "blue_bg" = openxlsx::createStyle(
        fontSize = 14,
        fontColour = "white",
        fgFill = "#4F81BD",
        halign = "center",
        valign = "center",
        textDecoration = "bold"
      ),
      "green_bg" = openxlsx::createStyle(
        fontSize = 14,
        fontColour = "white",
        fgFill = "#4CAF50",
        halign = "center",
        valign = "center",
        textDecoration = "bold"
      ),
      "dark_grey_bg" = openxlsx::createStyle(
        fontSize = 14,
        fontColour = "white",
        fgFill = "#636363",
        halign = "center",
        valign = "center",
        textDecoration = "bold"
      ),
      stop("Unknown header style type. Choose one of: 'blue_bg', 'green_bg', 'dark_grey_bg'.")
    )
    
    # Apply header style to the first row
    openxlsx::addStyle(wb, sheet_name, style = header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
    
    # Set row heights for headers and data
    openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 20)  # Header
    openxlsx::setRowHeights(wb, sheet_name, rows = 2:(nrow(df) + 1), heights = 18)  # Data
    
    # Apply styling to data rows
    data_style <- openxlsx::createStyle(
      fontSize = 11,
      border = "TopBottomLeftRight",
      borderColour = "grey"
    )
    openxlsx::addStyle(wb, sheet_name, style = data_style, rows = 2:(nrow(df) + 1), cols = 1:ncol(df), gridExpand = TRUE)
    
    # Freeze the top row and add filters
    openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
    openxlsx::addFilter(wb, sheet_name, rows = 1, cols = 1:ncol(df))
  }
  
  # Step 3: Save the workbook to the specified file path
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  
  # Inform the user
  message("Data has been written to Excel with multiple sheets in a publication-ready format.")
}

#' @examples
#' if (interactive()) {
#'   # Example data frames
#'   dataframes <- list(mtcars, iris)
#'   sheet_names <- c("mtcars", "iris")
#'
#'   # Write to Excel with default header style
#'   write_excel_multi(dataframes, "output.xlsx", sheet_names)
#' }
