library(broom)
library(lfe)
library(tidyverse)
library(data.table)

chars_path <- "../../../International Stock Data/Public/Data/Characteristics/"

use_subset <- T

# Layout settings -----------------
theme_set(theme_bw(base_size = 13))
colours_theme <- c("#0C6291", "#A63446", RColorBrewer::brewer.pal(8, "Dark2"), 
                   "darkslategrey", "blue3", "red3", "purple2", "yellow2", "aquamarine",
                   "grey", "salmon", "antiquewhite", "chartreuse") 
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = colours_theme)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = colours_theme)
}

# Settings ---------------------
set <- list(
  update = T,    # update result file?
  sub_chars = F, # Use Value and Momentum or all 153 chars?
  sub_cntry = F, # Use only UK and Germany?
  screens = list(
    feat_pct = 0.5,
    size_grps = c("mega", "large", "small", "micro", "nano")  
  ),
  feat_prank = T,
  feat_impute = T,
  start = as.Date("1985-12-31"), # Global data starts in as.Date("1985-12-31"), but we need 1 year of return data to calculate rvol_252d
  end = as.Date("2023-11-30")  # ret_ld1 is not available for Dec-2023
)
# Use subset of data?
if (use_subset) {
  # set$update <- F
  set$sub_chars <- T
  set$sub_cntry <- T
  # set$screens$size_grps <- "mega"
  # set$start <- as.Date("2020-12-31")
}

# Run code --------------------
# Run ---------------------------
# source("Code/0-functions.R")
source("Code/1.1-data.R", echo=T)
source("Code/2.1-global_regressions.R")