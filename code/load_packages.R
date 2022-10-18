# ============================================= #
# script: load_packages.R
# Project: Longitudinal Invariance and Reli.
# Author(s): R.N. Padgett
# ============================================= #
# Data Created: 2020-11-28
# Date Modified: 2022-10-14
# By: R. Noah Padgett
# ============================================= #
# Purpose:
# This R script is for loading all necessary
#   R packages
#
# No output - just loading packages into the
#   environment
# ============================================= #
# Set up directory and libraries
rm(list=ls())
# list of packages
packages <- c(
  "tidyverse", "readr", "patchwork",
  "tidyr","data.table", "dplyr",
  "polycor", "lavaan","semTools",
  "kableExtra", "xtable",
  "psych", "psychometric",
  "ggcorrplot", "mvtnorm"
)
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(packages, library, character.only = TRUE)

w.d <- getwd()
