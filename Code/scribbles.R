library(tidyverse)
library(data.table)

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

# Functions -------------------------
cite_fun <- function(x) {
  case_when(
    x=="google_cites" ~ "Google",
    x=="wos_cites" ~ "Web of Science",
    x=="sco_cites" ~ "Scopus"
  ) |> factor(levels = c("Google", "Web of Science", "Scopus"))
}

# Data ------------------------------
# Clusters ---
clusters <- fread("Data/Cluster Labels.csv") 
# Citations ---
# char_info <- readxl::read_xlsx("Paper/Factor Details (1).xlsx",
char_info <- readxl::read_xlsx("Paper/Factor Details_revision1.xlsx",
                                            sheet = "details", range = "A1:T300") %>%
  select("characteristic"=abr_jkp, cite, "date_range"=`in-sample period`, "google_cites"=`Google Scholar Citation Count`, "wos_cites"=`Web of Science Citation Count`, "sco_cites"=`Scopus Citation Count`) %>%
  filter(!is.na(characteristic)) %>%
  mutate(
    sample_start = date_range %>% str_extract("^\\d+") %>% as.integer(),
    sample_end = date_range %>% str_extract("\\d+$") %>% as.integer()
  ) |> 
  select(-date_range) |> 
  setDT()
base_chars <- char_info$characteristic

char_info <- clusters[char_info, on = "characteristic"]

char_info[is.na(google_cites)]

# Normalized cites ----------
char_info[, (c("google_cites_norm", "wos_cites_norm", "sco_cites_norm")) := lapply(.SD, function(x) x/.N), .SDcols = c("google_cites", "wos_cites", "sco_cites"), by = cite]

# Market ------
market <- fread("Data/market_returns.csv")
market <- market[, .(excntry, eom, "mkt"=mkt_vw_exc)]

# Factors -----
lms <- fread("Data/lms.csv")
lms <- market[lms, on = .(excntry, eom)]
lms <- lms[n_stocks_min>10, .(characteristic, excntry, eom, ret=ret_vw_cap, mkt)]
lms <- char_info[, .(characteristic, sample_start, sample_end)][lms, on = .(characteristic)]

# In-sample versus out-of-sample returns ---
lms[, period := case_when(
  year(eom)<=sample_end ~ "is",
  year(eom)>sample_end ~ "oos"
)]


lms_ss <- lms[, .(
  n = .N, 
  ret = mean(ret),
  alpha = mean(ret)-cov(ret,mkt)/var(mkt)*mean(mkt)
), by = .(excntry, characteristic, period)]
lms_ss[, n_is := sum(n[period=="is"]), by = .(excntry, characteristic)]
lms_ss[, n_oos := sum(n[period=="oos"]), by = .(excntry, characteristic)]

# In-sample versus out-of-sample: General ------------------------
lms_ss |> 
  mutate(type = if_else(excntry=="USA", "Sample: US", "Sample: Global ex US")) |> 
  filter(n_is >= 60 & n_oos>= 60) |> 
  select(type, excntry, characteristic, period, alpha) |> 
  pivot_wider(names_from = period, values_from = alpha) |>
  ggplot(aes(is, oos)) +
  geom_point() +
  geom_smooth(se=F, method = "lm") +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  facet_wrap(~type, scales = "free") +
  labs(x = "In-sample alpha", y = "Out-of-sample alpha")

# OOS relative to US IS ------------------------
lms_ss_us <- lms_ss[excntry=="USA"]

is_oos_ss <- lms_ss_us[period=="is" & excntry=="USA", .(characteristic, "n_is_us"=n, "alpha_us_is"=alpha)][lms_ss[period=="oos", .(excntry, characteristic, alpha, n_oos)], on = "characteristic"] |> 
  filter(n_is_us >= 60 & n_oos>= 60) |> 
  group_by(characteristic) |> 
  summarise(
    n = n(),
    is = mean(alpha_us_is),
    oos = mean(alpha)
  ) |> 
  setDT()

is_oos_ss |> 
  ggplot(aes(is, oos)) +
  geom_point() +
  geom_smooth(se=F, method = "lm") +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = "In-sample alpha (US)", y = "Out-of-sample alpha (All countries)")

char_info[, .(characteristic, cluster, google_cites, wos_cites, sco_cites)][is_oos_ss, on = "characteristic"] |> 
  pivot_longer(c(google_cites, wos_cites, sco_cites)) |>
  mutate(name = name |> cite_fun()) |>
  filter(!is.na(value)) |> 
  pivot_longer(c(is, oos), names_to = "period", values_to = "alpha") |>
  mutate(
    period = case_when(
      period=="is" ~ "In-sample alpha in the US",
      period=="oos" ~ "Out-of-sample alpha across all countries",
    )
  ) |> 
  ggplot(aes(value, alpha, colour = period)) +
  geom_point() +
  geom_smooth(se=F, method = "lm") + 
  scale_x_log10() +
  labs(x = "Citations", y = "Alpha") + 
  facet_wrap(~name, scales = "free_x") +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )




# Overview of citations ---------------------
# char_info_uni <- char_info[!is.na(cite), .(cluster, cite, google_cites, wos_cites, sco_cites)] |> unique()
cluster_cites <- char_info[, lapply(.SD, sum, na.rm=T), .SDcols = c("google_cites_norm", "wos_cites_norm", "sco_cites_norm"), by = cluster] 

cluster_cites |> 
  mutate(rank = google_cites_norm) |> 
  pivot_longer(-c(cluster, rank)) |>
  mutate(name = name |> str_remove("_norm") |> cite_fun()) |> 
  ggplot(aes(reorder(cluster, rank), value, fill = name)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~name, scales = "free_x") +
  labs(y = "Total citations") +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )

cluster_order <- cluster_cites[order(-google_cites_norm)]$cluster

char_info[!is.na(google_cites)] |> 
  mutate(
    cluster = cluster |> factor(levels = cluster_order),
    cites = google_cites,
    # name_pretty = paste0(characteristic, " - ", cite)
    name_pretty = paste0(cite, " [", characteristic, "]")
  ) |> 
  group_by(cluster) |> 
  mutate(
    rank = frank(-cites, ties.method = "random"),
    order = sum(google_cites_norm) + cites / 1000000
  ) |> 
  filter(rank<=2) |> 
  ggplot(aes(reorder(name_pretty, order), google_cites, fill=cluster)) +
  coord_flip() +
  geom_col() +
  labs(title = "Top 2 factors from each cluster") +
  theme(
    axis.title.y = element_blank()
  )
  


char_info[!is.na(cite), .(cite, google_cites, wos_cites, sco_cites)] |> 
  unique() |>
  mutate(rank = google_cites) |> 
  pivot_longer(-c(cite, rank)) |>
  mutate(name = name |> cite_fun()) |>
  filter(!is.na(value)) |> 
  ggplot(aes(reorder(cite, value), value, fill = name)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~name, scales = "free_x") +
  labs(y = "Total citations") +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )

# Citation-weighted factor index ----------------------------------------------------------------------------
type <- "google_cites_norm"
weights <- char_info[, .(characteristic, cite = get(type))][!is.na(cite)]
weights[, rank := frank(cite)]
weights[, rank_inv := frank(-cite)]
weights[, w := (rank-mean(rank))/sum(rank)]
weights[, w_inv := (rank_inv-mean(rank_inv))/sum(rank_inv)]


weights |> 
  filter(abs(w)>=0.004) |> 
  mutate(
    type = case_when(
      w>0 ~ "Long portfolio (cited)",
      w<0 ~ "Short portfolio (not cited)",
    )
  ) |> 
  ggplot(aes(reorder(characteristic, w), w)) +
  geom_point() +
  coord_flip() +
  labs(title = "Weights in cited-minus-not-cited factor") + 
  facet_wrap(~type, scales = "free") +
  theme(
    axis.title.y = element_blank()
  )

data <- weights[, .(characteristic, w, w_inv)][lms, on = "characteristic"][!is.na(w)]
data[excntry=="USA", .(min = min(eom)), by = characteristic][order(min)]
# Make sure that we have data for every factor

citation_pfs <- data[, .(
  n = .N, 
  mkt = unique(mkt),
  ret = sum(w*ret),
  ret_inv = sum(w_inv*ret)
), by = .(excntry, eom)] |> 
  melt(measure.vars = c("ret", "ret_inv"))

citation_pfs_us <- citation_pfs[year(eom)>=1973 & excntry == "USA"][order(variable, eom)]
citation_pfs_us[, cumret := cumsum(value), by = variable] 

citation_pfs_us |> 
  mutate(
    variable = case_when(
      variable == "ret" ~ "Citation-weighted",
      variable == "ret_inv" ~ "Inverse citation-weighted",
    )
  ) |> 
  ggplot(aes(eom, cumret, colour=variable)) + 
  geom_line() +
  labs(y = "Cumulative sum of returns") +
  theme(
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

# Citations and performance ---------------------------------------------------------------------------------
data <- clusters[lms, on = "characteristic"][year(eom)>=1973 & excntry=="USA"]
cluster_pfs <- data[, .(
  ret = mean(ret),
  mkt = unique(mkt)
), by = .(cluster, eom)]

cluster_pfs |> 
  group_by(cluster) |> 
  summarise(
    mean = mean(ret),
    beta = cov(mkt, ret)/var(mkt),
    alpha = mean - beta*mean(mkt)
  ) |> 
  left_join(cluster_cites, by ="cluster") |> 
  select(cluster, "Return"=mean, "Alpha"=alpha, "Cites"=google_cites_norm) |> 
  mutate(rank = Cites) |> 
  pivot_longer(-c(cluster, rank)) |> 
  mutate(name = name |> factor(levels = c("Cites", "Alpha", "Return"))) |> 
  ggplot(aes(reorder(cluster, rank), value, fill=name)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~name, scales = "free_x") +
  labs(y = "Performance since 1973") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# New classification ----------------------------------
new_class <- readxl::read_xlsx("Paper/new_classification.xlsx") %>%
  select("characteristic"=abr_jkp, paper_about=`Paper is about`, factor_about = `Factor is about`,"google_cites"=`Google Scholar Citation Count`, "wos_cites"=`Web of Science Citation Count`, "sco_cites"=`Scopus Citation Count`) |> 
  setDT()

# Paper citation information ---
paper_class <- new_class[!is.na(paper_about), .(paper_about, "cites"=google_cites)] |> unique()
paper_class <- paper_class[!(paper_about %in% c("NA", "WRONG CITE", "?"))]
# Split cases with "/" into multiple rows with cites divided
paper_class[, n := str_count(paper_about, "/")+1]
paper_class <- paper_class[, .(paper_about = strsplit(paper_about, "/")[[1]], cites=cites/n), by = .I]
# Handle case-sensitivity
paper_class[, paper_about := str_to_title(paper_about)]

ss <- paper_class[, n_total := sum(cites)][, .(
  n = .N, 
  cites=sum(cites)/unique(n_total)
), by = paper_about] |>
  arrange(-cites) |> 
  mutate(cumsum=cumsum(cites)) 

# Factor performance information -------------
ret <- new_class[,.(characteristic, factor_about)][lms, on = "characteristic"][year(eom)>=1973 & excntry=="USA"]
ret <- ret[, .(
  ret = mean(ret),
  mkt = unique(mkt)
), by = .(factor_about, eom)]

ret <- ret |> 
  group_by(factor_about) |> 
  summarise(
    mean = mean(ret),
    sd = sd(ret),
    beta = cov(mkt, ret)/var(mkt),
    res_vol = sd(ret - beta*mkt),
    alpha = mean - beta*mean(mkt)
  ) |>
  setDT()
ret[, paper_about := factor_about |> str_to_title()]

ss <- ret[ss, on = "paper_about"]

# PLOTS ------------------------------------
(plot_cites <- ss |>
  ggplot(aes(reorder(paper_about, cites), cites)) +
  geom_col() +
  coord_flip() +
  labs(x = "Theme", y = "Citations scaled by total citations", title = "Citations"))

(plot_cumsum <- ss |>
    ggplot(aes(reorder(paper_about, cites), cumsum, group=1)) +
    geom_point() +
    geom_line() +
    coord_flip(ylim = c(0, NA)) +
    labs(x = "Theme", y = "Citations scaled by total citations", title = "Cumulative citations") +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    ))

(plot_alpha <- ss |> 
  ggplot(aes(reorder(paper_about, cites), alpha*12)) +
  geom_col() +
  coord_flip(ylim=c(-0.01, NA)) +
  labs(x = "Theme", y = "Annualized alpha", title = "Average alpha") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ))

# Combine 
cowplot::plot_grid(plot_cites, plot_cumsum, plot_alpha, ncol=3, rel_widths = c(0.55, 0.45, 0.45))

ss[!is.na(alpha), cor(cites, alpha, method="spearman")]


# Check real assset illiquidity
data <- lms[year(eom)>=1973 & excntry=="USA"]
data <- data[characteristic=="aliq_at", .(eom, "new_x"=ret)][data, on = "eom"]
data[, cor(new_x, ret), by= characteristic][order(abs(V1))]

# Plot with Sharpe or Information ratio
(plot_alpha <- ss |> 
    ggplot(aes(reorder(paper_about, cites), alpha*12)) +
    geom_col() +
    coord_flip(ylim=c(-0.01, NA)) +
    labs(x = "Theme", y = "Annualized alpha", title = "Average alpha"))
(plot_sr <- ss |> 
    ggplot(aes(reorder(paper_about, cites), (mean/sd)*sqrt(12))) +
    geom_col() +
    coord_flip(ylim=c(-0.1, NA)) +
    labs(x = "Theme", y = "Annualized Sharpe ratio", title = "Average Sharpe ratio") +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    ))
(plot_ir <- ss |> 
    ggplot(aes(reorder(paper_about, cites), (alpha/res_vol)*sqrt(12))) +
    geom_col() +
    coord_flip(ylim=c(-0.1, NA)) +
    labs(x = "Theme", y = "Annualized information ratio", title = "Average information ratio") +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    ))
cowplot::plot_grid(plot_alpha, plot_sr, plot_ir, ncol=3, rel_widths = c(0.55, 0.45, 0.45))
