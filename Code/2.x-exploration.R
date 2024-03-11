# Cor market equity and illiquidity
chars[!is.na(ami) & !is.na(dolvol), .(
  cor_me_rvol=cor(rvol, me, method = "spearman", use = "complete.obs"), 
  cor_me_ami=cor(ami, me, method = "spearman", use = "complete.obs"), 
  cor_me_dolvol=cor(dolvol, me, method = "spearman", use = "complete.obs")
), by = eom] |> pivot_longer(-eom) |> 
  ggplot(aes(eom, value, colour=name)) + 
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = -1, linetype = "dashed")

# Coverage in and outside of the US
chars[,.N, by = .((excntry=="USA"), eom)] |> 
  ggplot(aes(eom, N, colour=excntry)) + 
  geom_line()
chars[,.N, by = .((excntry=="USA"), eom)][, pct := N/sum(N), by = eom] |> 
  ggplot(aes(eom, pct, colour=excntry)) + 
  geom_line()
# By going outside of the US we are drastically increasing the number of observations. Nice!!

