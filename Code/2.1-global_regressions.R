# Add regression data ---------------
chars[, t := interval(set$start+1, eom+1) / months(1)]
chars[, excntry_eom := paste0(excntry, eom)]
chars[, dev := (excntry %in% countries[msci_development == "developed", excntry])]
chars[, us := (excntry=="USA")]
# Estimate regression ----------------
estimates <- clusters |> map(function(feat) {
  print(paste0(feat, ": ", match(feat, clusters), " of ", length(clusters)))
  # Data
  sub <- chars[, .(excntry, eom, dev, us, excntry_eom, t, me_perc, rvol_perc, var = get(feat), ret_exc_lead1m)]
  # Regressions
  reg_list <- list(
    "none" = felm(ret_exc_lead1m ~ var | excntry_eom | 0 | excntry+eom, data = sub),
    "time" = felm(ret_exc_lead1m ~ var+var:t | excntry_eom | 0 | excntry+eom, data = sub),
    "liq" = felm(ret_exc_lead1m ~ var*me_perc | excntry_eom | 0 | excntry+eom, data = sub),
    "rvol" = felm(ret_exc_lead1m ~ var*rvol_perc | excntry_eom | 0 | excntry+eom, data = sub),
    "region" = felm(ret_exc_lead1m ~ var+var:dev | excntry_eom | 0 | excntry+eom, data = sub),
    "us" = felm(ret_exc_lead1m ~ var+var:us | excntry_eom | 0 | excntry+eom, data = sub)
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
if (set$update) {
  # If not already created, create a folder Data/generated/{current date}
  folder_path <- file.path("Data/Generated", format(Sys.Date(), "%Y-%m-%d"))
  if(!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
  estimates |> fwrite(paste0(folder_path, "/estimates.csv"))
}

