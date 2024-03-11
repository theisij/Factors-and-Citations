# Char info ------------------------------------
char_info <- readxl::read_xlsx("Paper/Factor Details_revision1.xlsx", sheet = "details") |> 
  select("characteristic"=abr_jkp, direction) |> 
  filter(!is.na(characteristic)) |> 
  mutate(direction= direction |> as.numeric()) |> 
  setDT()
# Add cluster labels
# cluster_labels <- fread("Data/Cluster Labels.csv")
new_class <- readxl::read_xlsx("Paper/new_classification.xlsx") %>%
  select("characteristic"=abr_jkp, cite, paper_about=`Paper is about`, factor_about = `Factor is about`) |> 
  setDT()
cluster_labels <- new_class[, .(characteristic, cluster=factor_about)][!is.na(characteristic)]
char_info <- char_info[cluster_labels, on = "characteristic"]

if (set$sub_chars) {
  char_info <- char_info[characteristic %in% c("ret_12_1", "be_me", "market_equity")]# Delete later}
}
features <- c(char_info$characteristic, "rvol")

# Countries ----------------------------------------------------
countries <- readxl::read_xlsx("Data/Country Classification.xlsx") |> setDT()
if (set$sub_cntry) {
  countries <- countries[excntry %in% c("GBR", "USA", "THA")]
} else {
  countries <- countries[msci_development %in% c("developed", "emerging")]
}

# Return cutoffs -------------------------------------------
ret_cutoffs <- fread("Data/return_cutoffs.csv", colClasses = c("eom" = "character"))
ret_cutoffs[, eom := eom %>% lubridate::fast_strptime(format = "%Y%m%d") %>% as.Date()]
ret_cutoffs <- ret_cutoffs[, .(eom, wins_high = ret_exc_99, wins_low = ret_exc_1)]  
# Change date become we winsorize on ret_lead1m
ret_cutoffs[, eom := eom+1-months(1)-1]

# Prepare characteristics ----------------------
chars <- countries$excntry |> map(function(cntry) {
  print(cntry)
  chars_cntry <- fread(paste0(chars_path, str_to_lower(cntry), ".csv"), 
                 select = unique(c("excntry", "source_crsp", "id", "eom", "size_grp", "dolvol_126d", "ami_126d", "rvol_252d", "me", "ret_exc_lead1m", features)),  
                 colClasses = c("eom" = "character"))
  chars_cntry[, eom := eom %>% lubridate::fast_strptime(format = "%Y%m%d") %>% as.Date()]
  chars_cntry[, dolvol := dolvol_126d]
  chars_cntry[, ami := ami_126d]
  chars_cntry[, rvol := rvol_252d]
  # Screens ------
  # Date screen
  print(paste0("   Date screen excludes ", round(mean(chars_cntry$eom < set$start | chars_cntry$eom > set$end) * 100, 2), "% of the observations"))
  chars_cntry <- chars_cntry[eom >= set$start & eom <= set$end]
  # Monitor screen impact
  n_start <- nrow(chars_cntry)
  me_start <- sum(chars_cntry$me, na.rm = T)
  # Require me and valid next-month ret
  if (FALSE) {
    print(paste0("   Non-missing me, rvol, ami, dolvol, and ret_1m excludes ", 
                 round(mean(is.na(chars_cntry$me) | is.na(chars_cntry$ret_exc_lead1m) | is.na(chars_cntry$dolvol) | is.na(chars_cntry$ami) | is.na(chars_cntry$rvol)) * 100, 2), "% of the observations"))
    chars_cntry <- chars_cntry[!is.na(me) & !is.na(chars_cntry$ret_exc_lead1m) & !is.na(chars_cntry$dolvol) & !is.na(chars_cntry$ami) & !is.na(chars_cntry$rvol)]
  } else {
    # Dolvol and ami is only available late in the sample for international firms. Use ME as a proxy for liquidity
    # chars_cntry[!is.na(ami) & !is.na(dolvol), .(cor_me_rvol=cor(rvol, me, method = "spearman", use = "complete.obs"), cor_me_ami=cor(ami, me, method = "spearman", use = "complete.obs"), cor_me_dolvol=cor(dolvol, me, method = "spearman", use = "complete.obs")), by = eom] |> pivot_longer(-eom) |> ggplot(aes(eom, value, colour=name)) + geom_line() 
    print(paste0("   Non-missing me and ret_1m excludes ", 
                 round(mean(is.na(chars_cntry$me) | is.na(chars_cntry$ret_exc_lead1m)) * 100, 2), "% of the observations"))
    chars_cntry <- chars_cntry[!is.na(me) & !is.na(chars_cntry$ret_exc_lead1m)]
  }
  # Size screen
  if(!is.null(set$screens$size_grps[1])) {
    print(paste0("   Size screen excludes ", round(mean(!(chars_cntry$size_grp %in% set$screens$size_grps)) * 100, 2), "% of the observations"))
    chars_cntry <- chars_cntry[chars_cntry$size_grp %in% set$screens$size_grps]
  }
  # Feature Screens----------
  feat_available <- chars_cntry %>% select(all_of(features)) %>% apply(1, function(x) sum(!is.na(x)))
  min_feat <- floor(length(features)*set$screens$feat_pct)
  print(paste0("   At least ", set$screens$feat_pct*100, "% of feature excludes ", round(mean(feat_available < min_feat)*100, 2), "% of the observations"))
  chars_cntry <- chars_cntry[feat_available >= min_feat]
  # Summary
  print(paste0("   In total, the final dataset has ", round( (nrow(chars_cntry) / n_start)*100, 2), "% of the observations and ", round((sum(chars_cntry$me) / me_start)*100, 2), "% of the market cap in the post ", set$screens$start, " data"))
  # Winsorize returns -------
  chars_cntry <- ret_cutoffs[chars_cntry, on = "eom"]
  chars_cntry[source_crsp==0 & ret_exc_lead1m>=wins_high, ret_exc_lead1m := wins_high]
  chars_cntry[source_crsp==0 & ret_exc_lead1m<=wins_low, ret_exc_lead1m := wins_low]
  chars_cntry[, c("source_crsp", "wins_low", "wins_high") := NULL]
  # Output
  return(chars_cntry)
}) |> rbindlist()

# Feature standardization 
if (set$feat_prank) {
  chars[, (features) := lapply(.SD, as.double), .SDcols = features]  # Convert feature columns to double to avoid loosing precision 
  for(f in features) {
    if (match(f, features) %% 10 == 0) print(paste0("Feature ", match(f, features), " out of ", length(features)))
    chars[, zero := (get(f) == 0)]
    chars[!is.na(get(f)), (f) := ecdf(get(f))(get(f)), by = eom] # Didn't have by statement before!!
    chars[zero == T, (f) := 0][, zero := NULL]  # Set exact zeros to 0 (ecdf always returns >0)
  }
} 
# Feature Imputation 
if (set$feat_impute) {
  if (set$feat_prank) {
    chars[, (features) := lapply(.SD, function(x) if_else(is.na(x), 0.5, x)), .SDcols=features]
  } else {
    chars[, (features) := lapply(.SD, function(x) if_else(is.na(x), median(x, na.rm=T), x)), .SDcols=features, by=eom]
  }
}
# Change direction and then aggregate to  cluster characteristic -----------
clusters <- unique(char_info$cluster)
cluster_ranks <- clusters %>% map(function(cl) {
  chars_sub <- char_info[cluster==cl]
  # print(paste0(cl, ", n: ", nrow(chars_sub)))
  data_sub <- chars[, chars_sub$characteristic, with=F]
  for (x in chars_sub$characteristic) {
    dir <- chars_sub[characteristic == x, direction]
    if (dir == -1) {
      data_sub[, (x) := 1-get(x)]
    }
  }
  data.table(x=data_sub %>% rowMeans()) %>% setnames(old = "x", new = cl)
}) %>% bind_cols()
# Add to existing data
chars <- chars[, .(excntry, id, eom, rvol_perc=rvol, me_perc=market_equity, me, ret_exc_lead1m)] |> 
  cbind(cluster_ranks)
# Re-standardize cluster features by date
chars[, (clusters) := lapply(.SD, function(x) ecdf(x)(x)), .SDcols = clusters, by = eom]
