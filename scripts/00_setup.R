# 00_setup.R
# Purpose: Load packages, define paths, and global options for the project.

options(stringsAsFactors = FALSE)
set.seed(443)

# ---- Packages ----
pkgs <- c(
  "tidyverse",
  "ggplot2",
  "broom",
  "janitor",
  "lubridate",
  "scales",
  "patchwork"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  message("Missing packages: ", paste(missing, collapse = ", "))
  message("Install with: install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))")
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(broom)
  library(janitor)
  library(lubridate)
  library(scales)
  library(patchwork)
})

# ---- Paths ----
DIR_DATA_RAW   <- file.path("data", "raw")
DIR_DATA_PROC  <- file.path("data", "processed")
DIR_OUT_FIG    <- file.path("outputs", "figures")
DIR_OUT_TAB    <- file.path("outputs", "tables")

dir.create(DIR_DATA_RAW,  recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_DATA_PROC, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_OUT_FIG,   recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_OUT_TAB,   recursive = TRUE, showWarnings = FALSE)

# Utilities (optional)
if (file.exists(file.path("scripts", "99_utils.R"))) {
  source(file.path("scripts", "99_utils.R"))
}
