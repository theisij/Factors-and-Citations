## Citations overview -------------------------------------------------
theme_info[theme %in% perf_is$theme] |> 
  ggplot(aes(reorder(theme, cites), cites_rel)) +
  geom_col() +
  coord_flip() +
  labs(x = "Theme", y = "Citations scaled by total citations", title = "Citations") +
  theme(
    axis.title.y=element_blank()
  )
ggsave("figures/citations.pdf", width=lt_w, height=lt_h)

## Performance in different regions -----------------------------------
perf_is <- regions[!is.na(cites), .(
  cites = unique(cites),
  ret = mean(ret),
  alpha = coef(lm(ret ~ mkt, data=.SD))[1]
), by = .(theme, dev)]

cor_is <- perf_is[, .("Full sample"=round(cor(cites, alpha, method="spearman"), 2)), by = dev]

perf_is |> 
  ggplot(aes(reorder(theme, cites), alpha*12)) + 
  geom_col() + 
  coord_flip(ylim = c(-0.02, NA)) + 
  facet_wrap(~dev, nrow=1, scales = "free_x") +
  scale_y_continuous(breaks = calculate_breaks(3)) +
  labs(title = "Full sample performance by region", y = "Annualized alpha") +
  theme(
    axis.title.y=element_blank()
  )
ggsave("figures/region_perf_fs.pdf", width=lt_w, height=lt_h)
  
perf_oos <- regions[year(eom)>end, .(
  n = .N,
  cites = unique(cites),
  ret = mean(ret),
  alpha = coef(lm(ret ~ mkt, data=.SD))[1]
), by = .(theme, dev)][n>=120]

cor_oos <- perf_oos[, .("Out-of-sample"=round(cor(cites, alpha, method="spearman"), 2)), by = dev]

perf_oos |> 
  ggplot(aes(reorder(theme, cites), alpha*12)) + 
  geom_col() + 
  coord_flip(ylim = c(-0.02, NA)) + 
  facet_wrap(~dev, nrow=1, scales = "free_x") +
  scale_y_continuous(breaks = calculate_breaks(3)) +
  labs(title = "Out-of-sample performance by region", y = "Annualized alpha") +
  theme(
    axis.title.y=element_blank()
  )
ggsave("figures/region_perf_oos.pdf", width=lt_w, height=lt_h)

# Do the same as above but for cmp instead of regions -------
perf_is <- cmp[!is.na(cites), .(
  cites = unique(cites),
  ret = mean(ret),
  alpha = coef(lm(ret ~ mkt, data=.SD))[1]
), by = .(theme, size_grp)]

cor_is_size <- perf_is[, .("Full sample"=round(cor(cites, alpha, method="spearman"), 2)), by = size_grp]

perf_is |> 
  ggplot(aes(reorder(theme, cites), alpha*12)) + 
  geom_col() + 
  coord_flip(ylim = c(-0.02, NA)) + 
  facet_wrap(~size_grp, nrow=1, scales = "free_x") +
  scale_y_continuous(breaks = calculate_breaks(3)) +
  labs(title = "Full sample performance by size group", y = "Annualized alpha") +
  theme(
    axis.title.y=element_blank()
  )
ggsave("figures/size_group_perf_fs.pdf", width=lt_w, height=lt_h)

perf_oos <- cmp[year(eom)>end, .(
  n = .N,
  cites = unique(cites),
  ret = mean(ret),
  alpha = coef(lm(ret ~ mkt, data=.SD))[1]
), by = .(theme, size_grp)][n>=120]

cor_oos_size <- perf_oos[, .("Out-of-sample"=round(cor(cites, alpha, method="spearman"), 2)), by = size_grp]

perf_oos |> 
  ggplot(aes(reorder(theme, cites), alpha*12)) + 
  geom_col() + 
  coord_flip(ylim = c(-0.02, NA)) + 
  facet_wrap(~size_grp, nrow=1, scales = "free_x") +
  scale_y_continuous(breaks = calculate_breaks(3)) +
  labs(title = "Out-of-sample performance by size group", y = "Annualized alpha") +
  theme(
    axis.title.y=element_blank()
  )
ggsave("figures/size_group_perf_oos.pdf", width=lt_w, height=lt_h)


# Table overview -----------
rbind(
  cor_is[cor_oos, on = "dev"] |>rename("Subset"=dev) |> arrange(Subset) ,
  tibble(Subset="---", `Full sample`=NA, `Out-of-sample`=NA),
  cor_is_size[cor_oos_size, on = "size_grp"] |>rename("Subset"=size_grp) |> arrange(Subset) |> mutate(Subset = paste0("US-", Subset))
) |> 
  xtable::xtable(caption = "Spearman correlation of citations and returns within subset", label = "tbl:cor") |> 
  print.xtable(include.rownames = FALSE, caption.placement = "bottom") 

# In-sample / oos -------------------
is_oos <- c("ir", "alpha", "sr", "ret", "vol", "res_vol") |> map(function(x) {
  data_is_oos[, .(theme, cites, sample, subset, var=get(x))] |> 
    pivot_wider(names_from = "sample", values_from = "var") |> 
    mutate(
      diff = is - oos
    ) |> 
    group_by(subset) |>
    filter(!is.na(diff)) |>
    summarise(
      n = n(),
      cor_diff = cor(cites, diff, method="spearman"),
      cor_abs_diff = cor(cites, abs(diff), method="spearman")
    ) |> 
    mutate(type = x)
}) |> 
  bind_rows() |> 
  mutate(
    type = case_when(
      type=="alpha" ~ "Alpha",
      type=="res_vol" ~ "Residual volatility",
      type=="ir" ~ "Information ratio",
      type=="ret" ~ "Return",
      type=="vol" ~ "Volatility",
      type=="sr" ~ "Sharpe ratio"
    ) |> factor(levels = c("Alpha", "Residual volatility", "Information ratio", "Return", "Volatility", "Sharpe ratio"))
  )

is_oos |> 
  pivot_longer(cols = c(cor_diff, cor_abs_diff), names_to = "cor_type", values_to = "cor") |> 
  mutate(
    cor_type = case_when(
      cor_type=="cor_diff" ~ "x=IS-OOS",
      cor_type=="cor_abs_diff" ~ "x=abs(IS-OOS)"
    ) |> factor(levels = c("x=IS-OOS", "x=abs(IS-OOS)"))
  ) |> 
  filter(subset != "Frontier") |> 
  ggplot(aes(subset, cor, fill = type)) +
  geom_col(position = "dodge") +
  labs(y = "Spearman cor(cites, x)") +
  facet_wrap(~cor_type, scales = "free_y", ncol=1) +
  theme(
    axis.title.x=element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )
ggsave("figures/is_oos.pdf", width=lt_w, height=lt_h)

# Return correlation across different groups ----------------------------------
data_cors <- c("region", "size") |> map(function(z) {
  data_fs_wide <- data_fs[type==z] |> 
    select(c(theme, cites, eom, subset, ret)) |> 
    pivot_wider(names_from = "subset", values_from = "ret") |> 
    setDT()
  
  unique(data_fs_wide$theme) |> map(function(x) {
    sub <- data_fs_wide[theme==x]
    sub_cor <- sub |> select(-c(theme, cites, eom)) |> cor(use="pairwise.complete.obs")
    sub_cor <- sub_cor[lower.tri(sub_cor, diag=F)]
    # Output
    tibble(
      theme = x,
      cites = unique(sub$cites),
      avg_cor = mean(sub_cor)
    )
  }) |> bind_rows() |> mutate(type=z)
}) |> bind_rows()

data_cors |> 
  group_by(type) |> 
  mutate(cor = cor(cites, avg_cor, method="spearman", use="complete.obs")) |> 
  mutate(
    type = case_when(
      type=="region" ~ paste0("x=Regions---cor cites: ", round(cor, 2)),
      type=="size" ~ paste0("x=Size groups---cor cites: ", round(cor, 2))
    ) 
  ) |> 
  ggplot(aes(reorder(theme, cites), avg_cor)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~type, scales = "free_x", ncol=2) +
  labs(y = "Average return correlation across x") +
  theme(
    axis.title.y=element_blank()
  )
ggsave("figures/return_correlation.pdf", width=lt_w, height=lt_h)

# Decline relative to the US -----------------------------------
diffs_us <- c("ir", "alpha", "sr", "ret", "vol", "res_vol") |> map(function(x) {
  data_fs_ss[type=="region", .(theme, cites, subset, var=get(x))] |> 
    pivot_wider(names_from = "subset", values_from = "var") |>
    pivot_longer(c(Emerging, Developed, Frontier), names_to = "subset", values_to = "other") |>
    mutate(
      diff = US - other
    ) |> 
    group_by(subset) |>
    filter(!is.na(diff)) |>
    summarise(
      n = n(),
      cor_diff = cor(cites, diff, method="spearman"),
      cor_abs_diff = cor(cites, abs(diff), method="spearman")
    ) |> 
    mutate(type = x)
}) |> 
  bind_rows() |> 
  mutate(
    type = case_when(
      type=="alpha" ~ "Alpha",
      type=="res_vol" ~ "Residual volatility",
      type=="ir" ~ "Information ratio",
      type=="ret" ~ "Return",
      type=="vol" ~ "Volatility",
      type=="sr" ~ "Sharpe ratio"
    ) |> factor(levels = c("Alpha", "Residual volatility", "Information ratio", "Return", "Volatility", "Sharpe ratio"))
  )

diffs_us |> 
  pivot_longer(cols = c(cor_diff, cor_abs_diff), names_to = "cor_type", values_to = "cor") |> 
  mutate(
    cor_type = case_when(
      cor_type=="cor_diff" ~ "x=US-Region",
      cor_type=="cor_abs_diff" ~ "x=abs(US-Region)"
    ) |> factor(levels = c("x=US-Region", "x=abs(US-Region)"))
  ) |> 
  ggplot(aes(subset, cor, fill = type)) +
  geom_col(position = "dodge") +
  labs(y = "Spearman cor(cites, x)") +
  facet_wrap(~cor_type, scales = "free_y", ncol=1) +
  theme(
    axis.title.x=element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )
ggsave("figures/diffs_us.pdf", width=lt_w, height=lt_h)

# Decline relative to the mega caps-----------------------------------
diffs_mega <- c("ir", "alpha", "sr", "ret", "vol", "res_vol") |> map(function(x) {
  data_fs_ss[type=="size", .(theme, cites, subset, var=get(x))] |> 
    pivot_wider(names_from = "subset", values_from = "var") |>
    pivot_longer(c(Large, Small, Micro, Nano), names_to = "subset", values_to = "other") |>
    mutate(
      diff = Mega - other
    ) |> 
    group_by(subset) |>
    filter(!is.na(diff)) |>
    summarise(
      n = n(),
      cor_diff = cor(cites, diff, method="spearman"),
      cor_abs_diff = cor(cites, abs(diff), method="spearman")
    ) |> 
    mutate(type = x)
}) |> 
  bind_rows() |> 
  mutate(
    type = case_when(
      type=="alpha" ~ "Alpha",
      type=="res_vol" ~ "Residual volatility",
      type=="ir" ~ "Information ratio",
      type=="ret" ~ "Return",
      type=="vol" ~ "Volatility",
      type=="sr" ~ "Sharpe ratio"
    ) |> factor(levels = c("Alpha", "Residual volatility", "Information ratio", "Return", "Volatility", "Sharpe ratio")),
    subset = case_when(
      subset=="Large" ~ "Large",
      subset=="Small" ~ "Small",
      subset=="Micro" ~ "Micro",
      subset=="Nano" ~ "Nano"
    ) |> factor(levels = c("Large", "Small", "Micro", "Nano"))
  )

diffs_mega |> 
  pivot_longer(cols = c(cor_diff, cor_abs_diff), names_to = "cor_type", values_to = "cor") |> 
  mutate(
    cor_type = case_when(
      cor_type=="cor_diff" ~ "x=Mega-Size group",
      cor_type=="cor_abs_diff" ~ "x=abs(Mega-Size group)"
    ) |> factor(levels = c("x=Mega-Size group", "x=abs(Mega-Size group)"))
  ) |> 
  ggplot(aes(subset, cor, fill = type)) +
  geom_col(position = "dodge") +
  labs(y = "Spearman cor(cites, x)") +
  facet_wrap(~cor_type, scales = "free_y", ncol=1) +
  theme(
    axis.title.x=element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )
ggsave("figures/diffs_mega.pdf", width=lt_w, height=lt_h)

# Standard deviation across subsets ------------------------------
data_fs_ss[, lapply(.SD, sd), .SDcols = c("ir", "alpha", "sr", "ret", "vol", "res_vol"), by = .(cites, theme, type)] |> 
  pivot_longer(cols = c(ir, alpha, sr, ret, vol, res_vol), names_to = "var", values_to = "sd") |>
  group_by(type, var) |>
  summarise(
    cor = cor(cites, sd, method="spearman")
  ) |> 
  mutate(
    var = case_when(
      var=="alpha" ~ "Alpha",
      var=="res_vol" ~ "Residual volatility",
      var=="ir" ~ "Information ratio",
      var=="ret" ~ "Return",
      var=="vol" ~ "Volatility",
      var=="sr" ~ "Sharpe ratio"
    ) |> factor(levels = c("Alpha", "Residual volatility", "Information ratio", "Return", "Volatility", "Sharpe ratio"))
  ) |> 
  ggplot(aes(var, cor, fill = type)) +
  geom_col(position = "dodge") +
  labs(y = "Spearman cor(cites, sd across x)", fill="x:") +
  theme(
    legend.position = "top",
    axis.title.x=element_blank()
  )
ggsave("figures/sd_across_subsets.pdf", width=lt_w, height=lt_h*0.7)  
