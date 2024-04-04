if (set$update) {
  # Add regression data ---------------
  chars[, t := interval(set$start+1, eom+1) / months(1)]
  chars[, excntry_eom := paste0(excntry, eom)]
  chars[, dev := (excntry %in% countries[msci_development == "developed", excntry])]
  chars[, us := (excntry=="USA")]
  chars[, volatile := (rvol_perc>0.5)]
  chars[, micro := (me_perc<0.5)]
  # chars[, size_factor := case_when(
  #   size_grp %in% c("mega", "large") ~ "large",
  #   size_grp %in% c("small") ~ "small",
  #   size_grp %in% c("micro", "nano") ~ "micro"
  # ) |> factor(levels = c("large", "small", "micro"))]
  # chars[, size_grp := size_grp |> factor(levels = c("mega", "large", "small", "micro", "nano"))]
  # Estimate regression ----------------
  estimates <- clusters |> map(function(feat) {
    print(paste0(feat, ": ", match(feat, clusters), " of ", length(clusters)))
    # Data
    sub <- chars[, .(excntry, eom, dev, us, micro, volatile, excntry_eom, t, me_perc, rvol_perc, var = get(feat), ret_exc_lead1m)]
    # Short dummy
    sub[, short := (var < 0.5)]
    # Post sample dummy
    ps <- theme_info[cluster==feat]$end
    sub[, post := (year(eom) > ps)]
    print(summary(sub))
    # Regressions
    reg_list <- list(
      "none" = felm(ret_exc_lead1m ~ var | excntry_eom | 0 | eom, data = sub),
      "time" = felm(ret_exc_lead1m ~ var+var:t | excntry_eom | 0 | eom, data = sub),
      "liq" = felm(ret_exc_lead1m ~ var*me_perc | excntry_eom | 0 | eom, data = sub),
      "liq-extremes" = felm(ret_exc_lead1m ~ var*me_perc+var:me_perc:micro | excntry_eom | 0 | eom, data = sub),
      "rvol" = felm(ret_exc_lead1m ~ var*rvol_perc | excntry_eom | 0 | eom, data = sub),
      "rvol-extremes" = felm(ret_exc_lead1m ~ var*rvol_perc+var:rvol_perc:volatile | excntry_eom | 0 | eom, data = sub),
      "region" = felm(ret_exc_lead1m ~ var+var:dev | excntry_eom | 0 | eom, data = sub),
      "us" = felm(ret_exc_lead1m ~ var+var:us | excntry_eom | 0 | eom, data = sub),
      "short" = felm(ret_exc_lead1m ~ var+var:short | excntry_eom | 0 | eom, data = sub),
      "post" = felm(ret_exc_lead1m ~ var+var:post | excntry_eom | 0 | eom, data = sub)
    )
    # prettify
    names(reg_list) |> map(function(nm) {
      coefs <- reg_list[[nm]] |> tidy()
      r2 <- reg_list[[nm]] |> glance() |> select(r.squared, adj.r.squared, nobs)
      cbind(coefs, r2) |> mutate(model = nm, feature = feat) |> setDT()
    }) |> rbindlist()
  }) |> rbindlist()
  # Should give one warnings for liq req for market equity
  
  # Save output ----
  # If not already created, create a folder Data/generated/{current date}
  folder_path <- file.path("Data/Generated", format(Sys.Date(), "%Y-%m-%d"))
  if(!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
  estimates |> fwrite(paste0(folder_path, "/estimates.csv"))
} else {
  # Load output ----
  estimates <- fread("Data/Generated/2024-03-12/estimates.csv")
}

# 
if (FALSE) {
  data <- tibble(x=seq(0,1,0.01), x2=x^2, y = if_else(x<0.2, x, 0.2)) 
  data |> ggplot(aes(x, y)) + geom_point()+geom_smooth(method = "lm", se=F, formula = y ~ poly(x,2))
  lm(y ~ x + x2, data = data) |> summary()
}

# Correlation plot -------------
if (FALSE) {
  # Save the plot below to pdf
  pdf("Figures/correlation.pdf")
  
  if (FALSE) {
    # One period
    chars[eom == max(eom)][, clusters, with = FALSE] |>
      cor() |>
      corrplot::corrplot(type = "lower", method = "number", number.cex = 0.3, tl.cex = 0.7, 
                         order = "hclust", hclust.method = "average")
  } else {
    # All periods
    # Compute average correlation across all time periods
    cor_list <- unique(chars$eom) |> map(function(d) {
      chars[eom == eom][, clusters, with = FALSE] |>
        cor()
    }, .progress = T)
    # Compute average of matrix across cor list
    cor_avg <- Reduce(`+`, cor_list) / length(cor_list)
    # cor_avg |> fwrite("Data/Generated/cor_avg.csv")
    cor_avg |> corrplot::corrplot(type = "lower", method = "number", number.cex = 0.3, tl.cex = 0.7, 
                                  order = "hclust", hclust.method = "average")
  }
  
  # Close the PDF device
  dev.off()
  
  
}
