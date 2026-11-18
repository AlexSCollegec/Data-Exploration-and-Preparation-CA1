=====================================================================
  # CA1 - Data Exploration and Preparation
  # Domain : Energy
  # Topic  : How countries make their electricity (2000 - 2022)
  # Data   : Our World in Data - Energy dataset
  #          https://github.com/owid/energy-data
  # =====================================================================

# ---- 0. Packages ----------------------------------------------------
library(dplyr)     # data cleaning
library(tidyr)     # reshape data (wide <-> long)
library(ggplot2)   # charts
library(corrplot)  # correlation chart

dir.create("plots", showWarnings = FALSE)   # folder for saved charts
set.seed(123)                                # same results every run