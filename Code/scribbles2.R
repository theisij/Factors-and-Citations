# Short side regressions --------------------------------------------------
# Existing ----------------------
b <- 0.01
c <- 0.02
data <- tibble(
  x = seq(0, 1, 0.01),
  short = as.integer(x<=0.5),
  y = b * x + c * short * x
) 

# Leads to wierd kink because x isn't centered
data |> 
  ggplot(aes(x, y)) +
  geom_point() 


lm(y ~ x+x:short, data = data) 
lm(y ~ x, data = data) 

# Better ---------------------
b <- 0.01
c <- 0.01
data <- tibble(
  x = seq(0, 1, 0.01)-0.5,
  short = as.integer(x<=0),
  y = b * x + c * short * x
) 

data |> 
  ggplot(aes(x, y)) +
  geom_smooth(se=F, method = "lm", formula = y ~ x) +
  geom_point() 


lm(y ~ x+x:short, data = data) 
lm(y ~ x, data = data) 


# Extremes regressions ---------------------
estimates[model=="none" | model == "liq-extremes"][feature=="Short-Term Reversal"]
# "liq" = felm(ret_exc_lead1m ~ var*me_perc | excntry_eom | 0 | eom, data = sub),
# "liq-extremes" = felm(ret_exc_lead1m ~ var*me_perc+var:me_perc:micro | excntry_eom | 0 | eom, data = sub)

# Existing -----
a <- 0.027
b <- 0.00766
c <- -0.03
d <- -0.03

data <- tibble(
  x = seq(0, 1, 0.01),
  me = runif(101, 0, 1),
  micro = as.integer(me<=0.5),
  y = a*x + b*me + c*x*me + d*x*me*micro
) 

data |> 
  ggplot(aes(x, y)) +
  geom_smooth(se=F, method = "lm", formula = y ~ x) +
  geom_point() +
  facet_wrap(~micro)

# Better -------
