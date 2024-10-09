# R Startup Scripts and Proteomics Helper Functions

Welcome to the **`r_startup_scripts`** repository! This repository contains R scripts and custom functions to streamline your R project setups and enhance your workflow, particularly for proteomics data analysis. These scripts are designed to be sourced directly from GitHub for easy integration into new projects.

## Table of Contents
- [Overview](#overview)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Available Functions](#available-functions)
- [Folder Structure Setup](#folder-structure-setup)
- [Contributing](#contributing)
- [License](#license)

## Overview
The repository includes two key scripts:
1. **`startup.R`**: A startup script to automate common setup tasks whenever you start a new R project.
2. **`functions.R`**: A collection of utility functions for various tasks, such as exporting data to Excel, creating publication-ready plots, and manipulating proteomics data.

## Features
- **Automatic Package Installation**: The `install_load()` function ensures required R packages are installed and loaded.
- **Folder Setup**: Automatically creates common project folders (`data`, `res`, `doc`, `pub`) if they do not exist.
- **Custom Helper Functions**: Pre-defined functions for tasks like exporting formatted Excel sheets, generating custom plots, and more.
- **Easy Integration**: Scripts can be sourced directly from GitHub, making them reusable and easy to update.

## Installation
To use these scripts in your R projects, you can source them directly from GitHub or Gitee:

### From Github
```r
# Source the startup script
source("https://raw.githubusercontent.com/SongbiaoZhu/r_startup_scripts/main/startup.R")

# Source the functions script
source("https://raw.githubusercontent.com/SongbiaoZhu/r_startup_scripts/main/functions.R")
```

### From Gitee
```r
# Source the startup script
source("https://gitee.com/zhu_song_biao/r_startup_scripts/raw/main/startup.R")

# Source the functions script
source("https://gitee.com/zhu_song_biao/r_startup_scripts/raw/main/functions.R")
```
## Usage
### Setting Up a New Project
1. **Create a New R Project** in RStudio.
2. **Source the startup script** using the command provided above. This will automatically create the folder structure and load the necessary packages.
3. **Source the functions script** to access the custom helper functions.

### Example Code
Here’s how to use the scripts in a new project:

```r
# Load project setup and utility functions
source("https://raw.githubusercontent.com/SongbiaoZhu/r_startup_scripts/main/startup.R")
source("https://raw.githubusercontent.com/SongbiaoZhu/r_startup_scripts/main/functions.R")

# Use one of the helper functions to export a formatted Excel file
export_pretty_excel(data = iris, filename = "example.xlsx", sheetname = "Iris Data")
```

## Available Functions
Here’s a brief overview of the key functions included in `functions.R`:

* `export_pretty_excel()`: Exports a data frame to a well-formatted Excel sheet.
* `create_folders()`: Creates a standardized folder structure for new projects.
* `install_load()`: Installs and loads required CRAN and Bioconductor packages.
* **Custom plotting functions**: Includes templates for density plots, correlation matrices, and volcano plots.

## Folder Structure Setup
The `startup.R` script creates the following project folders if they do not exist:

* `data/` – For raw and processed datasets.
* `res/` – For storing analysis results.
* `doc/` – For documentation, protocols, or report files.
* `pub/` – For final publication-ready outputs and figures.

## Contributing
Contributions to this repository are welcome! Feel free to:
* Submit **pull requests** for new features or bug fixes.
* Open **issues** if you encounter any problems or have suggestions.
If you want to contribute, please fork the repository and make your changes in a new branch.

## License
This repository is licensed under the MIT License. See the [LICENSE]() file for more details.

