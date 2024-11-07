#' Write Data Frame(s) to Publication-Ready Excel File
#'
#' This function writes one or more data frames to an Excel file with customizable header styles,
#' automatic column width adjustments, and data formatting, ensuring a tidy, publication-ready output.
#'
#' @param data A data frame or a list of data frames. If a list is provided, each data frame will be written to a separate sheet.
#' @param file_path Full path (including file name) where the Excel file will be saved.
#' @param sheet_names A single sheet name (if `data` is a data frame) or a character vector of sheet names (if `data` is a list). Defaults to "Sheet1" for single data frame or "Sheet1", "Sheet2", etc., for unnamed lists.
#' @param header_style_type Style for the header row. Options: "blue_bg", "green_bg", "dark_grey_bg" (default: "dark_grey_bg").
#'
#' @return An Excel file saved at the specified location.

write_excel <- function(data, file_path, sheet_names = NULL, header_style_type = "dark_grey_bg") {
  
  options("openxlsx.maxWidth" = 60) 
  options("openxlsx.minWidth" = 5)
  
  # If data is a single data frame, wrap it in a list
  if (is.data.frame(data)) {
    data <- list(Sheet1 = data)
  } else if (!is.list(data)) {
    stop("Data must be a data frame or a list of data frames.")
  }
  
  # Handle unnamed lists: assign default sheet names if necessary
  if (is.null(names(data))) {
    if (is.null(sheet_names)) {
      sheet_names <- paste0("Sheet", seq_along(data))
    }
    names(data) <- sheet_names
  } else if (!is.null(sheet_names)) {
    names(data) <- sheet_names
  }
  
  # Check if sheet_names has correct length
  if (length(names(data)) != length(data)) {
    stop("Number of sheet names must match the number of data frames.")
  }
  
  # Step 1: Create a new workbook
  wb <- openxlsx::createWorkbook()
  
  # Step 2: Loop over data frames and add each to a separate worksheet
  for (i in seq_along(data)) {
    df <- data[[i]]
    sheet_name <- names(data)[i]
    
    # Add a worksheet for each data frame
    openxlsx::addWorksheet(wb, sheet_name)
    # Write data first
    openxlsx::writeData(wb, sheet_name, df)
    
    # Step 3: Automatically adjust column widths
    openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = "auto")

    # Step 4: Apply header style
    header_style <- switch(header_style_type,
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
    
    openxlsx::addStyle(wb, sheet_name, style = header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
    
    # Step 5: Adjust row heights
    openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 25)  # Header
    openxlsx::setRowHeights(wb, sheet_name, rows = 2:(nrow(df) + 1), heights = 18)
    
    # Step 6: Apply border style for data rows
    data_style <- openxlsx::createStyle(
      fontSize = 11,
      border = "TopBottomLeftRight",
      borderColour = "grey"
    )
    openxlsx::addStyle(wb, sheet_name, style = data_style, rows = 2:(nrow(df) + 1), cols = 1:ncol(df), gridExpand = TRUE)
    
    # Step 7: Freeze top row and add filters
    openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
    openxlsx::addFilter(wb, sheet_name, rows = 1, cols = 1:ncol(df))
  }
  
  # Step 8: Save the workbook to the specified file path
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  
  # Inform the user
  message("Data has been written to Excel.")
}
#' @examples
# if(interactive()){
#   # Save a single data frame
#   write_excel_combined(mtcars, "output_single.xlsx")
# 
#   # Save multiple data frames with default sheet names
#   data_list <- list(mtcars, iris)
#   write_excel_combined(data_list, "output_multi.xlsx")
# 
#   # Save multiple data frames with custom sheet names
#   write_excel_combined(data_list, "output_custom.xlsx", sheet_names = c("Cars", "Flowers"))
# 
# }
