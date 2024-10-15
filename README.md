# R Startup Scripts and Proteomics Helper Functions

Welcome to the **`r_startup_scripts`** repository! This repository contains R scripts and custom functions to streamline your R project setups and enhance your workflow, particularly for proteomics data analysis. These scripts are designed to be sourced directly from Gitee or GitHub for easy integration into new projects.

## Table of Contents
- [Overview](#overview)
- [Usage](#usage)
- [Available Functions](#available-functions)
- [Folder Structure Setup](#folder-structure-setup)
- [Contributing](#contributing)
- [License](#license)

## Overview
The repository includes two key scripts:
1. **`startup.R`**: A startup script to automate common setup tasks whenever you start a new R project.
2. **`plot_xxx.R`**: A collection of utility functions for plotting.
3. **`xxx_.R`**: A collection of utility functions for various tasks, such as exporting data to Excel, and manipulating proteomics data.

## Usage
### Setting Up a New Project
1. **Create a New R Project** in RStudio.
2. **Source the startup script** using the command provided above. This will automatically create the folder structure and load the necessary packages.
3. **Source the functions script** to access the custom helper functions.

### Example Code
Here’s how to use the scripts in a new project:

```r
# Load project setup and utility functions
## from gitee
### step 1. source startup.R
source("https://gitee.com/zhu_song_biao/r_startup_scripts/raw/main/startup.R")
### step 2. source xxx.R dependent on your specific need
source("https://gitee.com/zhu_song_biao/r_startup_scripts/raw/main/export_pretty_excel.R")

## or from github
### step 1. source startup.R
source("https://raw.githubusercontent.com/SongbiaoZhu/r_startup_scripts/main/startup.R")
### step 2. source xxx.R dependent on your specific need
source("https://raw.githubusercontent.com/SongbiaoZhu/r_startup_scripts/main/export_pretty_excel.R")

# Use the helper function to export a formatted Excel file
export_pretty_excel(data = iris, filename = "example.xlsx", sheetname = "Iris Data")
```

## Available Functions
Here’s a brief overview of the key functions included in `functions.R`:

* `install_load()`: Installs and loads required CRAN and Bioconductor packages.
* `export_pretty_excel()`: Exports a data frame to a well-formatted Excel sheet.
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
This repository is licensed under the ![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg). See the LICENSE file for more details.

