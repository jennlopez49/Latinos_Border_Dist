## IVs --------
ivs <- list()
ivs[[1]] <- c("distance_km", "Party_5pt", 
              "Education", "Age", "Income", "California",
              "Texas")
ivs[[2]] <- c("psych_dist_imm", "Party_5pt",
              "Education", "Age", "Income", "California",
              "Texas")
ivs[[3]] <- c("distance_km", "psych_dist_imm", "Party_5pt",
            "Education", "Age", "Income",  "California",
            "Texas")
ivs[[4]] <- c("distance_km*psych_dist_imm", "linked_simp", "Party_5pt",
              "Education", "Mexican", "Age", "Income", "Spanish", 
              "California",
              "Texas")
ivs[[5]] <- c("distance_km*Linked_Fate", "psych_dist_imm", "Party_5pt",
         "Education", "Mexican", "Age", "Income", "Spanish", "California",
         "Texas")
ivs[[6]] <- c("distance_km*identity_strength_recoded", "psych_dist_imm", 
              "Party_5pt", "Education", "Mexican", "Age", "Income", 
              "California",
              "Texas")
ivs[[7]] <- c("psych_dist_imm*identity_strength_recoded", 
              "Party_5pt", "Education", "Mexican", "Age", "Income", "Spanish",
              "California",
              "Texas")
ivs[[8]] <- c("psych_dist_imm*Linked_Fate", "Party_5pt",
              "Education", "Mexican", "Age", "distance_km", "Spanish", 
              "California",
              "Texas")
# ivs[[9]] <- c("poly(distance_km, 2)", "Party_5pt", 
#               "Education", "Age", "Income", "party_majority_state")

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
lat_over_100 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km > 160.934)

### survey objects -------

svy_cmps_200 <- svydesign(id = ~ 1, weights = ~race_weight, data = lat_200)

svy_cmps_100 <- svydesign(id = ~ 1, weights = ~race_weight, data = lat_100)
svy_cmps_over100 <- svydesign(id = ~ 1, weights = ~race_weight, data = lat_over_100)

####### subset regressions ---------
under_200 <- bin_function(dvs_binomial, ivs, svy_cmps_200, lat_200, "runs_200")


under_200_ols <- ols_function(dvs_ols, ivs, svy_cmps_200, lat_200, 
                              "runs_200_ols")
under_100 <- bin_function(dvs_binomial, ivs, svy_cmps_100, lat_100, "runs_100")
over_100 <- bin_function(dvs_binomial, ivs, svy_cmps_over100, lat_over_100, 
                         "runs_over100")
under_100_ols <- ols_function(dvs_ols, ivs, svy_cmps_100, lat_100, 
                              "runs_100_ols")
over_100_ols <- ols_function(dvs_ols, ivs, svy_cmps_over100, lat_over_100, 
                              "runs_over100_ols")

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

plots_100 <- plot_model(under_100_ols[[7]], type = "int",
           legend.title = "Identity Strength")
plots_100



#### two interactions 


main <- svyglm(Increase_Border_Spending ~ Party + Education + Age + Gender + Income + 
                    linked_simp*psych_dist_lang + linked_simp*distance_km + 
  party_majority_state, design = svy_cmps, data = full_cmps_lat, family = "quasibinomial")


#### BORDER VS NON BORDER 

border <- subset(full_cmps_lat, subset = full_cmps_lat$border_state == 1)
nonborder <- subset(full_cmps_lat, subset = full_cmps_lat$border_state == 0)

# designs

des_b <- svydesign(id = ~ 1, weights = ~race_weight, data = border)
des_nonb <- svydesign(id = ~ 1, weights = ~race_weight, data = nonborder)


main_b <- svyglm(Increase_Border_Spending ~ Party + Education + Age + Gender + 
                   Income + 
                   psych_dist_lang*linked_simp + Worried_Detained_Deported + 
                   party_majority_state, design = des_b, 
                 data = border, family = "quasibinomial")


main_dist <- svyglm(border_security_recoded ~ Party + Education + Age + Gender + 
                      Income + 
                      distance_km*linked_simp + Worried_Detained_Deported +
                      psych_dist_lang +
                      party_majority_state, design = svy_cmps, 
                    data = full_cmps_lat, family = gaussian(link = "identity"))

main_non <- svyglm(Increase_Border_Spending ~ Party + Education + Age + Gender + 
                   Income + 
                   psych_dist_lang*linked_simp + 
                   party_majority_state, design = des_nonb, 
                 data = nonborder, family = "quasibinomial")
