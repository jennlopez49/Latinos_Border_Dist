## IVs --------
ivs <- list()
ivs[[1]] <- c("distance_km", "Party_5pt", 
              "Education", "Age", "Income", "party_majority_state")
ivs[[2]] <- c("Psych_Distance", "Party_5pt",
              "Education", "Age", "Income", "party_majority_state")
ivs[[3]] <- c("distance_km", "Psych_Distance", "Party_5pt",
            "Education", "Age", "Income", "party_majority_state")
ivs[[4]] <- c("distance_km*Psych_Distance", "Linked_Fate", "Party_5pt",
              "Education", "Mexican", "Age", "Income", "Spanish", 
              "party_majority_state")
ivs[[5]] <- c("distance_km*Linked_Fate", "Psych_Distance", "Party_5pt",
         "Education", "Mexican", "Age", "Income", "Spanish", 
         "party_majority_state")
ivs[[6]] <- c("distance_km*identity_strength_recoded", "Psych_Distance", 
              "Party_5pt", "Education", "Mexican", "Age", "Income", 
              "distance_km", "party_majority_state")
ivs[[7]] <- c("Psych_Distance*identity_strength_recoded", "Psych_Distance", 
              "Party_5pt", "Education", "Mexican", "Age", "Income", "Spanish",
              "party_majority_state")
ivs[[8]] <- c("Psych_Distance*Linked_Fate", "Psych_Distance", "Party_5pt",
              "Education", "Mexican", "Age", "distance_km", "Spanish", 
              "party_majority_state")

## Survey Design Weight Object -----

svy_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = full_cmps_lat)

# running the regressions - - ------

bin_runs <- bin_function(dvs_binomial, ivs, svy_cmps, full_cmps_lat, "bin_runs")

ols_runs <- ols_function(dvs_ols, ivs, svy_cmps, full_cmps_lat, "ols_runs_full")

### visualizing int ------

plot_model(bin_runs$Increase_Border_Spending[[6]], type = "int", 
           legend.title = "Identity Strength")
stargazer(ols_runs_full, type = "text")

## tables for full samples ------------

stargazer(ols_runs_full, type = "text",
          covariate.labels = c("Distance (in km)", "Identity Strength",
                               "Psychological Distance", "Linked Fate",
                               "Party", "Education", "Mexican", "Age",
                               "Income", "Spanish", 
                               "State Leg Partisan Majority - Rep",
                               "Distance (in km):Psychological Distance",
                               "Distance (in km):Linked Fate",
                               "Distance (in km): Identity Strength",
                               "Psychological Distance:Linked Fate",
                               "Psychological Distance: Identity Strength",
                               "Constant"),
          dep.var.labels = "Increase Border Security as a National Priority",
          out = "table_2_full.html")

stargazer(bin_runs$Increase_Border_Spending, type = "text", 
          covariate.labels = c("Distance (in km)", "Identity Strength",
                               "Psychological Distance", "Linked Fate",
                               "Party", "Education", "Mexican", "Age",
                               "Income", "Spanish", 
                               "State Leg Partisan Majority - Rep",
                               "Distance (in km):Psychological Distance",
                               "Distance (in km):Linked Fate",
                               "Distance (in km): Identity Strength",
                               "Psychological Distance:Linked Fate",
                               "Psychological Distance: Identity Strength",
                               "Constant"),
          dep.var.labels = "Increase Border Spending, Including Wall",
          out = "table_1_full.html")



### subset to under 200 miles (in kilometers) & under 100 ---------------

lat_200 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km < 321.869)
lat_100 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km < 160.934)

### survey objects -------

svy_cmps_200 <- svydesign(id = ~ 1, weights = ~race_weight, data = lat_200)

svy_cmps_100 <- svydesign(id = ~ 1, weights = ~race_weight, data = lat_100)

####### subset regressions ---------
under_200 <- bin_function(dvs_binomial, ivs, svy_cmps_200, lat_200, "runs_200")


under_200_ols <- ols_function(dvs_ols, ivs, svy_cmps_200, lat_200, 
                              "runs_200_ols")
under_100 <- bin_function(dvs_binomial, ivs, svy_cmps_100, lat_100, "runs_100")
under_100_ols <- ols_function(dvs_ols, ivs, svy_cmps_100, lat_100, 
                              "runs_100_ols")

### tables for 200 mile subset -------
stargazer(under_200, type = "text",
          covariate.labels = c("Distance (in km)", "Identity Strength",
                               "Psychological Distance", "Linked Fate",
                               "Party", "Education", "Mexican", "Age",
                               "Income", "Spanish", 
                               # "State Leg Partisan Majority - Divided",
                               "State Leg Partisan Majority - Rep",
                               "Distance (in km):Psychological Distance",
                               "Distance (in km):Linked Fate",
                               "Distance (in km): Identity Strength",
                               "Psychological Distance: Identity Strength",
                               "Psychological Distance: Linked Fate",
                               "Constant"),
          dep.var.labels = "Increase Border Spending, Including Wall",
          out = "table_200_IncreaseBorderSpend.html")

stargazer(under_200_ols, type = "text",
          covariate.labels = c("Distance (in km)", "Identity Strength",
                               "Psychological Distance", "Linked Fate",
                               "Party", "Education", "Mexican", "Age",
                               "Income", "Spanish", 
                               # "State Leg Partisan Majority - Divided",
                               "State Leg Partisan Majority - Rep",
                               "Distance (in km):Psychological Distance",
                               "Distance (in km):Linked Fate",
                               "Distance (in km): Identity Strength",
                               "Psychological Distance: Identity Strength",
                               "Psychological Distance: Linked Fate",
                               "Constant"),
          dep.var.labels = "Increase Border Security as a National Priority",
          out = "table_200_IncreaseBorderSec.html")

stargazer(under_100$Increase_Border_Spending, type = "text",
          covariate.labels = c("Distance (in km)", "Identity Strength",
                               "Psychological Distance", "Linked Fate",
                               "Party", "Education", "Mexican", "Age",
                               "Income", "Spanish", 
                               # "State Leg Partisan Majority - Divided",
                               "State Leg Partisan Majority - Rep",
                               "Distance (in km):Psychological Distance",
                               "Distance (in km):Linked Fate",
                               "Distance (in km): Identity Strength",
                               "Psychological Distance: Identity Strength",
                               "Psychological Distance: Linked Fate",
                               "Constant"),
          dep.var.labels = "Increase Border Spending, Including Wall",
          out = "table_100_IncreaseBorderSpend.html"
          )
summary(under_100$Increase_Border_Spending[[1]])
stargazer(under_100_ols, type = "text",
          covariate.labels = c("Distance (in km) Logged", "Identity Strength",
                     "Psychological Distance", "Linked Fate",
                     "Party", "Education", "Mexican", "Age",
                     "Income", "Spanish", 
                     "State Leg Partisan Majority - Divided",
                     "State Leg Partisan Majority - Rep",
                     "Distance (in km) Logged:Psychological Distance",
                     "Distance (in km) Logged:Linked Fate",
                     "Distance (in km) Logged: Identity Strength",
                     "Psychological Distance: Identity Strength",
                     "Psychological Distance: Linked Fate",
                     "Constant"),
          dep.var.labels = "Increase Border Security as a National Priority",
          out = "table_100_IncreaseBorderSec.html")

###### visualizing int 

plot_model(under_100$Increase_Border_Spending[[7]], type = "int",
           legend.title = "Identity Strength")

