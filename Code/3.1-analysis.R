estimates <- estimates |> 
  mutate(model = model |> fct_relevel("none", "time", "liq", "region"))

# Var as a function of controls
estimates |> 
  filter(term=="var") |> 
  ggplot(aes(x=feature, y = estimate, fill = model)) +
  coord_flip() +
  geom_col(position = "dodge")
# Interactions>   
estimates |> 
  filter(term %in% c("var:t", "var:me_rank", "var:devTRUE", "var:usTRUE", "var:rvol_perc")) |> 
  ggplot(aes(x=feature, y = estimate)) +
  coord_flip() +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.25, position = position_dodge(0.9)) +
  facet_wrap(~term, scales = "free_x") 

