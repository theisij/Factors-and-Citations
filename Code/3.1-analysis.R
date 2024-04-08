# Naming functions ---------------
terms_naming <- function(x) {
  case_when(
    x=="var" ~ "Raw",
    x=="var:t" ~ "Time",
    x=="var:me_perc" ~ "Size",
    x=="var:rvol_perc" ~ "Volatility",
    x=="var:devTRUE" ~ "Developed",
    x=="var:usTRUE" ~ "US",
    x=="var:postTRUE" ~ "Post publication",
    x=="var:shortTRUE" ~ "Short",
    x=="var:me_perc:microTRUE" ~ "Size-extremes",
    x=="var:rvol_perc:volatileTRUE" ~ "Volatility-extremes",
  ) |> factor(levels = c("Raw", "Time", "Post publication", "Short", "Size", "Volatility", "Developed", "US", "Size-extremes", "Volatility-extremes"))
}

# Re-level ------------------------
estimates <- estimates |> 
  mutate(model = model |> fct_relevel("none", "time", "post", "short", "liq", "rvol", "region", "us", "liq-extremes", "rvol-extremes")) |> 
  filter(feature != "Age")
if (FALSE) {
  estimates[, feature := str_to_title(feature)]
}
extremes <- c("rvol-extremes", "liq-extremes")

# Main estimates of interest -----------
estimates[, main := (
  model=="none" & term=="var" |
  model=="time" & term=="var:t" |
  model=="post" & term=="var:postTRUE" |
  model=="short" & term=="var:shortTRUE" |
  model=="liq" & term=="var:me_perc" |
  model=="rvol" & term=="var:rvol_perc" |
  model=="region" & term=="var:devTRUE" |
  model=="us" & term=="var:usTRUE" |
  model=="liq-extremes" & term=="var:me_perc:microTRUE" |
  model=="rvol-extremes" & term=="var:rvol_perc:volatileTRUE"
)]

# Var as a function of controls
estimates |> 
  filter(term=="var") |> 
  ggplot(aes(x=feature, y = estimate, fill = model)) +
  coord_flip() +
  geom_col(position = "dodge")
# Interactions>   
all_ints <- c("var:t", "var:postTRUE", "var:shortTRUE", "var:me_perc", "var:rvol_perc",  "var:devTRUE", "var:usTRUE")
estimates |> 
  # filter(term %in% all_ints & !(model %in% extremes)) |>
  filter(main==T & !(model %in% extremes)) |>
  mutate(term = term |> terms_naming()) |>
  left_join(theme_info[,.(feature=cluster, "Citations"=cites_rel)], by = "feature") |>
  ggplot(aes(x=reorder(feature, Citations), y = estimate)) +
  coord_flip() +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  facet_wrap(~term, scales = "free_x", nrow = 2) +
  labs(y = "Estimate") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 6)
  )
ggsave("figures/regs_overall.pdf", width=lt_w, height=lt_h)

# Correlations among all interactions -----
if (FALSE) {
  pdf("Figures/correlation_est.pdf")
  estimates |> 
    filter(main==T & !(model %in% extremes)) |> 
    mutate(term=terms_naming(term)) |>
    arrange(term) |> 
    select(feature, term, estimate) |> 
    pivot_wider(names_from = term, values_from = estimate) |> 
    left_join(theme_info[,.(feature=cluster, "Citations"=cites_rel)], by = "feature") |> 
    filter(feature != "Fundamental Growth") |> 
    select(-feature) |> 
    cor(method="spearman", use="pairwise.complete.obs") |> 
    corrplot::corrplot(type = "lower", method = "number")
  dev.off()
}

# Nothing ---------------------------------
estimates |> 
  filter(term %in% c("var") & model=="none") |> 
  ggplot(aes(x=reorder(feature, estimate), y = estimate)) +
  coord_flip() +
  geom_col(fill = colours_theme[1]) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  labs(title = "Raw: ret = b*char", y = "Estimate, b") +
  theme(
    axis.title.y = element_blank()
  )
ggsave("figures/regs_raw.pdf", width=lt_w, height=lt_h)

# Time trend ------------------------------
estimates |> 
  filter(term %in% c("var:t")) |> 
  ggplot(aes(x=reorder(feature, estimate), y = estimate)) +
  coord_flip() +
  geom_col(fill = colours_theme[1]) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  labs(title = "Time: ret = b*char + c*(char x time)", y = "Interaction estimate, c")  +
  theme(
    axis.title.y = element_blank()
  )
ggsave("figures/regs_time.pdf", width=lt_w, height=lt_h)

# Post-publication ------------------------------
estimates |> 
  filter(term %in% c("var:postTRUE")) |> 
  ggplot(aes(x=reorder(feature, estimate), y = estimate)) +
  coord_flip() +
  geom_col(fill = colours_theme[1]) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  labs(title = "Post-publication: ret = b*char + c*(char x post)", y = "Interaction estimate, c")  +
  theme(
    axis.title.y = element_blank()
  )
ggsave("figures/regs_post.pdf", width=lt_w, height=lt_h)

# Shorting ------------------------------
estimates |> 
  filter(term %in% c("var:shortTRUE")) |> 
  ggplot(aes(x=reorder(feature, estimate), y = estimate)) +
  coord_flip() +
  geom_col(fill = colours_theme[1]) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  labs(title = "Short side: ret = b*char + c*(char x short)", y = "Interaction estimate, c")  +
  theme(
    axis.title.y = element_blank()
  )
ggsave("figures/regs_shorting.pdf", width=lt_w, height=lt_h)

# Effect on long versus sort side
estimates |> 
  filter(model=="short") |> 
  select(feature, term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate) |>
  mutate(
    short_side = var + `var:shortTRUE`,
    long_side = var,
    sort_var = short_side
  ) |> 
  pivot_longer(c(short_side, long_side)) |>
  mutate(
    name = case_when(
      name == "short_side" ~ "Short side (b+c)",
      name == "long_side" ~ "Long side (b)"
    )
  ) |> 
  ggplot(aes(x=reorder(feature, sort_var), y = value, fill="name")) +
  coord_flip() +
  geom_col(fill = colours_theme[1]) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  # geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  labs(title = "Short side: ret = b*char + c*(char x short)", y = "Interaction estimate, c")  +
  facet_wrap(~name) +
  theme(
    axis.title.y = element_blank()
  )
ggsave("figures/regs_shorting_expanded.pdf", width=lt_w, height=lt_h)
# Size ---------------------------------
estimates |> 
  filter(term %in% c("var:me_perc") & model=="liq") |> 
  ggplot(aes(x=reorder(feature, estimate), y = estimate)) +
  coord_flip() +
  geom_col(fill = colours_theme[1]) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  labs(title = "Size: ret = b*char + c*(char x me)+ d*me", y = "Interaction estimate, c")  +
  theme(
    axis.title.y = element_blank()
  )
ggsave("figures/regs_size.pdf", width=lt_w, height=lt_h)

# Volatility ------------------------------
estimates |> 
  filter(term %in% c("var:rvol_perc") & model=="rvol") |> 
  ggplot(aes(x=reorder(feature, estimate), y = estimate)) +
  coord_flip() +
  geom_col(fill = colours_theme[1]) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  labs(title = "Volatility: ret = b*char + c*(char x rvol) + d*rvol", y = "Interaction estimate, c")  +
  theme(
    axis.title.y = element_blank()
  )
ggsave("figures/regs_rvol.pdf", width=lt_w, height=lt_h)

# Developed ------------------------------
estimates |> 
  filter(term %in% c("var:devTRUE")) |> 
  ggplot(aes(x=reorder(feature, estimate), y = estimate)) +
  coord_flip() +
  geom_col(fill = colours_theme[1]) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  labs(title = "Developed: ret = b*char + c*(char x dev)", y = "Interaction estimate, c")  +
  theme(
    axis.title.y = element_blank()
  )
ggsave("figures/regs_dev.pdf", width=lt_w, height=lt_h)

# US ------------------------------
estimates |> 
  filter(term %in% c("var:usTRUE")) |> 
  ggplot(aes(x=reorder(feature, estimate), y = estimate)) +
  coord_flip() +
  geom_col(fill = colours_theme[1]) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  labs(title = "US: ret = b*char + c*(char x us)", y = "Interaction estimate, c")  +
  theme(
    axis.title.y = element_blank()
  )
ggsave("figures/regs_us.pdf", width=lt_w, height=lt_h)

# Expected return in the beginning versus end of the sample, in and out of the U.S. ----------------
er_time <- estimates[model=="time", .(feature, term, estimate)] |> 
  pivot_wider(names_from = term, values_from = estimate) |>
  mutate(
    est_start = var,
    est_end = var+`var:t`*as.integer(interval(set$start+1, set$end+1) / months(1)),
  ) |> 
  mutate(sort_order = est_end) |> 
  select(feature, sort_order, est_start, est_end) |> 
  pivot_longer(c(est_start, est_end)) |> 
  mutate(
    name = case_when(
      name == "est_start" ~ "Expected return in 1985",
      name == "est_end" ~ "Expected return in 2023"
    )
  ) 

er_time |>
  ggplot(aes(x=reorder(feature, sort_order), y = value*12, fill = name)) +
  coord_flip(ylim = c(-0.02, NA)) +
  geom_col(position = "dodge") + 
  facet_wrap(~name) + 
  labs(y = "Expected annualized return", title = "Expected returns in 1985 vs. 2023") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none"
  )
ggsave("figures/er_start_end.pdf", width=lt_w, height=lt_h)  

er_time |> 
  left_join(theme_info[,.(feature=cluster, cites)], by = "feature") |>
  group_by(name) |> 
  summarise(
    cor = cor(value, cites, method = "spearman")
  )

# Specific themes ---------------------------
estimates |> 
  filter(feature=="Accruals") |> 
  filter(term %in% all_ints) |> 
  ggplot(aes(x=term, y = estimate)) +
  coord_flip() +
  geom_col(fill = colours_theme[1]) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  labs(title = "US: ret = b*char + c*(char x us)", y = "Interaction estimate, c")  +
  theme(
    axis.title.y = element_blank()
  )

# Correlation with citations ----------------
library(xtable)
est_with_cites <- theme_info[, .("feature"=cluster, cites)][estimates, on = "feature"][!is.na(cites)] |> 
  filter((model == "none" & term=="var") | term %in% all_ints)

est_with_cites[, .(
  cor = cor(estimate, cites, method = "spearman")
), by = model][order(model)] |> 
  mutate(
    Estimate = case_when(
      model == "none" ~ "Raw ($\\beta_1$)",
      model == "time" ~ "Time ($\\beta_2$)",
      model == "liq" ~ "Size ($\\beta_2$)",
      model == "rvol" ~ "Volatility ($\\beta_2$)",
      model == "region" ~ "Developed ($\\beta_2$)",
      model == "us" ~ "US ($\\beta_2$)"
    )
  ) |> 
  select(Estimate, "Spearman correlation(estimate, citations)"=cor) |>
  xtable::xtable(caption = "Spearman correlation of citations and estimate", label = "tbl:cor:new") |> 
  print.xtable(include.rownames = FALSE, caption.placement = "bottom", sanitize.text.function = identity) 

# Figure with citations:
theme_info[, .("feature"=cluster, cites)][estimates, on = "feature"][!is.na(cites)] |> 
  filter((model == "none" & term=="var") | term %in% all_ints) |> 
  mutate(
    model = case_when(
      model == "none" ~ "Raw (beta_1)",
      model == "time" ~ "Time (beta_2)",
      model == "liq" ~ "Size (beta_2)",
      model == "rvol" ~ "Volatility (beta_2)",
      model == "region" ~ "Developed (beta_2)",
      model == "us" ~ "US (beta_2)"
    ) |> fct_relevel("Raw (beta_1)", "Time (beta_2)", "Size (beta_2)", "Volatility (beta_2)", "Developed (beta_2)", "US (beta_2)")
  ) |> 
  ggplot(aes(x=reorder(feature, cites), y = estimate)) +
  coord_flip() +
  geom_col(fill = colours_theme[1]) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  labs(title = "Factors on y-axis sorted by citations", y = "Estimate")  +
  theme(
    axis.title.y = element_blank()
  ) + 
  facet_wrap(~model, scales = "free_x")
ggsave("figures/regs_cites.pdf", width=lt_w, height=lt_h*1.5)
