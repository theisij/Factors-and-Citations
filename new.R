library(xtable)
library(scales)
library(tidyverse)
library(data.table)

# Settings ------------
lt_w <- 7
lt_h <- 6

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

# Function to calculate a number of breaks for a given range
calculate_breaks <- function(n) {
  function(x) {
    pretty(range(x), n)
  }
}

# Functions -------------------------
cite_fun <- function(x) {
  case_when(
    x=="google_cites" ~ "Google",
    x=="wos_cites" ~ "Web of Science",
    x=="sco_cites" ~ "Scopus"
  ) |> factor(levels = c("Google", "Web of Science", "Scopus"))
}

# Characteristic information --------
char_info <- readxl::read_xlsx("Paper/Factor Details_revision1.xlsx",
                               sheet = "details", range = "A1:T300") %>%
  select("characteristic"=abr_jkp, cite, "date_range"=`in-sample period`, direction, "cites"=`Google Scholar Citation Count`) %>%
  # filter(!is.na(characteristic)) %>%
  mutate(
    sample_start = date_range %>% str_extract("^\\d+") %>% as.integer(),
    sample_end = date_range %>% str_extract("\\d+$") %>% as.integer()
  ) |> 
  select(-date_range) |> 
  setDT()

# New classification ----------------------------------
new_class <- readxl::read_xlsx("Paper/new_classification.xlsx") %>%
  select("characteristic"=abr_jkp, cite, paper_about=`Paper is about`, factor_about = `Factor is about`) |> 
  setDT()
new_class[, paper_about := str_to_title(paper_about)]
new_class[, factor_about := str_to_title(factor_about)]
new_class[paper_about %in% c("Na", "Wrong Cite", "?"), paper_about := NA_character_]
# Extract year from cite
new_class[, year := str_extract(cite, "\\d{4}") %>% as.integer()]

# Theme info, including cites and OOS period ----------
paper_class <- new_class[!is.na(paper_about), .(cite, year, paper_about)] |> unique()
paper_class <- unique(char_info[, .(cite, cites, sample_start, sample_end)])[paper_class, on = "cite"]
theme_info <- copy(paper_class)
# Split cases with "/" into multiple rows with cites divided
theme_info[, n := str_count(paper_about, "/")+1]
theme_info <- theme_info[, .(cite, paper_about = strsplit(paper_about, "/")[[1]], cites=cites/n, year, sample_start, sample_end), by = .I]
# First with first year in each theme
theme_info[, first_year := min(year), by = paper_about]

theme_info <- theme_info[, n_total := sum(cites)][, .(
  n = .N, 
  cite = unique(cite[year==first_year])[1],
  start = as.integer(min(sample_start[year==first_year], na.rm=T)),
  end = as.integer(max(sample_end[year==first_year], na.rm=T)),
  cites = sum(cites),
  cites_rel=sum(cites)/unique(n_total)
), by = paper_about]
theme_info |> setnames(old = "paper_about", new = "theme")

# Add theme to char_info 
char_info <- new_class[, .(characteristic, "theme"=factor_about)][char_info[!is.na(characteristic)], on = "characteristic"]

# Market ----------------------
market <- fread("market_returns.csv")
market <- market[, .(excntry, eom, "mkt"=mkt_vw_exc)]

# Factors -----
lms <- fread("lms.csv")
lms <- market[lms, on = .(excntry, eom)]
lms <- lms[n_stocks_min>10, .(characteristic, excntry, eom, ret=ret_vw_cap, mkt)]
lms <- char_info[,.(characteristic, theme)][lms, on = "characteristic"]
# Filter/Winsorization 
lms <- lms[excntry!="ZWE"] # Zimbabwe is crazy
lms[, ret := pmax(pmin(quantile(ret, 0.999), ret), quantile(ret, 0.001))]

# Create theme portfolios in each country --------
themes <- lms[,.(
  n = .N,
  ret = mean(ret),
  mkt = unique(mkt)
), by = .(theme, excntry, eom)]
# Aggregate across MSCI development -----------------------
country_class <- readxl::read_xlsx("Country Classification.xlsx") |> setDT()
country_class[, msci_development := case_when(
  excntry=="USA" ~ "US",
  msci_development=="developed" ~ "Developed",
  TRUE ~ msci_development |> str_to_title()
) |> factor(levels = c("US", "Developed", "Emerging", "Frontier", "Not Rated", "Standalone"))]
themes <- country_class[, .(excntry, dev=msci_development)][themes, on = "excntry"]
regions <- themes[!(dev %in% c("Standalone", "Not Rated")) & year(eom)>=1972, .(
  n = .N,
  ret = mean(ret),
  mkt = mean(mkt)
), by = .(theme, dev, eom)] 

regions <- theme_info[,.(theme, end, cites)][regions, on = "theme"]

# Characteristic managed portfolios -------------
cmp <- fread("cmp.csv")
cmp[, size_grp := size_grp |> 
      str_to_title() |>
      factor(levels = str_to_title(c("mega", "large", "small", "micro", "nano")))]
# Compute returns by theme, size group, eom
cmp <- char_info[cmp, on = "characteristic"]
cmp <- cmp[n_stocks >= 20]
cmp[, ret := ret_weighted*as.numeric(direction)]
cmp <- cmp[,.(
  n = .N,
  ret = mean(ret)
), by = .(excntry, theme, size_grp, eom)]
# Add the market return
cmp <- market[cmp, on = .(excntry, eom)]
# Exclude data before 1972
cmp <- cmp[year(eom)>=1972]
cmp <- theme_info[,.(theme, end, cites)][cmp, on = "theme"]

# IS/OOS ----------------------------
data_is_oos <- rbind(
  regions |> rename("subset"=dev) |> select(c(theme, end, cites, eom, mkt, subset, ret)),
  cmp |> rename("subset"=size_grp) |> select(c(theme, end, cites, eom, mkt, subset, ret)) 
)
data_is_oos[, sample := if_else(year(eom)>end, "oos", "is")]
data_is_oos <- data_is_oos[, .(
  cites = unique(cites),
  ret = mean(ret),
  vol = sd(ret),
  sr = mean(ret)/sd(ret),
  alpha = coef(lm(ret ~ mkt, data=.SD))[1],
  res_vol = sd(resid(lm(ret ~ mkt, data=.SD)))
), by = .(theme, sample, subset)][, ir := alpha/res_vol][theme != "Fundamental Growth"]

# data_is_oos[, .(theme, cites, sample, subset, ir)] |> 
#   pivot_wider(names_from = "sample", values_from = "ir") |> 
#   mutate(
#     diff = is - oos,
#     abs_diff = abs(diff)
#   ) |> 
#   ggplot(aes(x = reorder(theme, cites), y = abs_diff)) +
#   geom_col() +
#   coord_flip() +
#   facet_wrap(~dev, scales = "free_x", nrow=1)


# Full sample ----------------------------
data_fs <- rbind(
  regions |> rename("subset"=dev) |> select(c(theme, cites, eom, mkt, subset, ret)) |> mutate(type = "region"),
  cmp |> rename("subset"=size_grp) |> select(c(theme, cites, eom, mkt, subset, ret)) |> mutate(type = "size") 
) |> filter(theme != "Fundamental Growth")

data_fs_ss <- data_fs[, .(
  n = .N,
  cites = unique(cites),
  ret = mean(ret),
  vol = sd(ret),
  sr = mean(ret)/sd(ret),
  alpha = coef(lm(ret ~ mkt, data=.SD))[1],
  res_vol = sd(resid(lm(ret ~ mkt, data=.SD)))
), by = .(theme, subset, type)][, ir := alpha/res_vol]
