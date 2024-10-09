# Function to make column names R-friendly
make_r_friendly_names <- function(x) {

  # Function to clean up names
  clean_names <- function(names) {
    names %>%
      gsub("[^[:alnum:]_]", "_", .) %>%  # Replace non-alphanumeric characters with underscores
      gsub("_+", "_", .) %>%              # Replace multiple underscores with a single underscore
      gsub("^_+|_+$", "", .) %>%          # Remove leading and trailing underscores
      make.names(unique = TRUE)           # Ensure names are unique and syntactically valid
  }
  
  if (is.data.frame(x)) {
    colnames(x) <- clean_names(colnames(x))
    return(x)
  } else if (is.character(x)) {
    return(clean_names(x))
  } else {
    stop("Input must be a data frame or a character vector.")
  }
}

# Example usage with a data frame
# df <- data.frame(`Abundance #1` = c(1, 2), `Sample: Value` = c(3, 4))
# df_clean <- df %>% make_r_friendly_names()

# Example usage with a character vector
# char_vector <- c("Abundance #1", "Sample: Value")
# clean_vector <- make_r_friendly_names(char_vector)
