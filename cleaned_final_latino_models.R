### CLEANED FINAL MODELS 
### List of models --------
short_ivs <- list()

short_ivs[[1]] <- c("distance_km","Party_5pt", "linked",
                    "Education", "Age", "Income")
# short_ivs[[2]] <- c("dist_sqd","Party_5pt", "linked",
#                     "Education", "Age", "Income")
short_ivs[[2]] <- c("psych_dist_lang","Party_5pt", "linked",
                    "Education", "Age", "Income")
short_ivs[[3]] <- c("distance_km*psych_dist_lang", "linked", "Party_5pt",
                    "Education", "Age", "Income")
# short_ivs[[5]] <- c("dist_sqd*psych_dist_lang", "linked", "Party_5pt",
#                     "Education", "Age", "Income")
### Setting up exclusive vs inclusive ------ 
incl <- subset(full_cmps_lat, subset = full_cmps_lat$State == 32 | full_cmps_lat$California == 1)
excl <- subset(full_cmps_lat, subset = full_cmps_lat$State == 3 | full_cmps_lat$Texas == 1)

### Survey designs ------------------
incl_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = incl)
excl_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = excl)


### Running the models ---------------

bin_function(dvs_binomial, short_ivs, incl_cmps, incl, "incl_runs")
bin_function(dvs_binomial, short_ivs, excl_cmps, excl, "excl_runs")

ols_function(dvs_ols, short_ivs, incl_cmps, incl, "incl_runs_sec")
ols_function(dvs_ols, short_ivs, excl_cmps, excl, "excl_runs_sec")

### Producing the tables -------------

stargazer(incl_runs, excl_runs, type = "latex", 
          dep.var.labels = "Increase Border Spending, Including A Border Wall",
          covariate.labels = c("Distance (in km)",
                               "Psych. Distance",
                               "Party", 
                               "Linked Fate",
                               "Education","Age",
                               "Income",
                               "Distance (in km): Psych. Distance",
                               "Constant"), column.labels = c("Most Inclusive",
                                                              "Least Inclusive"),
          column.separate = c(3,3))


stargazer(incl_runs_sec, excl_runs_sec,
          dep.var.labels = "Border Security as a National Priority",
          covariate.labels = c("Distance (in km)",
                               "Psych. Distance",
                               "Party", 
                               "Linked Fate",
                               "Education","Age",
                               "Income",
                               "Distance (in km): Psych. Distance",
                               "Constant"), column.labels = c("Most Inclusive",
                                                              "Least Inclusive"),
          column.separate = c(3,3))

