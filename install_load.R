# Function to install and load packages (CRAN and Bioconductor)
install_load <- function(packages) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  
  sapply(packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      if (pkg %in% rownames(available.packages())) {
        install.packages(pkg, dependencies = TRUE)
      } else {
        BiocManager::install(pkg, ask = FALSE)
      }
      library(pkg, character.only = TRUE)
    }
  })
}

# Example usage
# install_load(c("tidyverse", "magrittr", "data.table", "lubridate"))

