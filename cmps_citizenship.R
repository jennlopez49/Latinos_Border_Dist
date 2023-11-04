###### list of models ------
ivs_am <- list()
ivs_am[[1]] <- c("distance_km", "Remit", "Linked_Fate", "Party", "Education",
                 "Age", "Income", "Gender")
ivs_am[[2]] <- c("distance_km", "Remit", "identity_strength_recoded", "Party", 
                 "Education",
                 "Age", "Income", "Gender")
ivs_am[[3]] <- c("distance_km*Linked_Fate", "Remit", "Party", "Education",
                 "Age", "Income", "Gender")
ivs_am[[4]] <- c("distance_km*identity_strength_recoded", "Remit", "Party", 
                 "Education", "Age", "Income", "Gender")
ivs_am[[5]] <- c("psych_dist_lang", "Linked_Fate", "Remit", "Party", "Education",
                 "Age", "Income", "Gender")
ivs_am[[6]] <- c("psych_dist_lang","identity_strength_recoded", "Remit", "Party", 
                 "Education", "Age", "Income", "Gender", "distance_km")
ivs_am[[7]] <- c("psych_dist_lang","identity_strength_recoded", "Remit", "Party", 
                 "Education", "Age", "Income", "Gender", "distance_km")
ivs_am[[8]] <- c("psych_dist_lang*border_state", "Remit","identity_strength_recoded", "Party", 
                 "Education", "Age", "Income", "Gender")
### survey design -------
svy_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = full_cmps_lat)

##### regressions -------

ols_function(dvs_american, ivs_am, svy_cmps, full_cmps_lat, "citizenship_runs") 

