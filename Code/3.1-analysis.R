# Re-level ------------------------
estimates <- estimates |> 
  mutate(model = model |> fct_relevel("none", "time", "liq", "rvol", "region", "us"))
if (FALSE) {
  estimates[, feature := str_to_title(feature)]
}

# Var as a function of controls
estimates |> 
  filter(term=="var") |> 
  ggplot(aes(x=feature, y = estimate, fill = model)) +
  coord_flip() +
  geom_col(position = "dodge")
# Interactions>   
all_ints <- c("var:t", "var:me_perc", "var:rvol_perc",  "var:devTRUE", "var:usTRUE")
estimates |> 
  filter(term %in% all_ints) |> 
  ggplot(aes(x=feature, y = estimate)) +
  coord_flip() +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  facet_wrap(~term, scales = "free_x") 

# Correlations among all interactions -----
estimates |> 
  filter(term %in% all_ints | (term =="var" & model=="none")) |> 
  select(feature, term, estimate) |> 
  pivot_wider(names_from = term, values_from = estimate) |> 
  left_join(theme_info[,.(feature=cluster, cites_rel)], by = "feature") |> 
  filter(feature != "Fundamental Growth") |> 
  select(-feature) |> 
  cor(method="spearman") |> 
  corrplot::corrplot(type = "lower", method = "number")

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

# Size ---------------------------------
estimates |> 
  filter(term %in% c("var:me_perc")) |> 
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
  filter(term %in% c("var:rvol_perc")) |> 
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
estimates[model=="time", .(feature, term, estimate)] |> 
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
  ) |>
  ggplot(aes(x=reorder(feature, sort_order), y = value*12, fill = name)) +
  coord_flip() +
  geom_col(position = "dodge") + 
  facet_wrap(~name) +
  labs(y = "Expected annualized return", title = "Expected returns in 1985 vs. 2023") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none"
  )
ggsave("figures/er_start_end.pdf", width=lt_w, height=lt_h)  

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
