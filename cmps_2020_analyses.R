## IVs --------
ivs <- list()
ivs[[1]] <- c("distance_km", "Party_5pt", 
              "Education", "Age", "Income"
              # , "California",
              # "Texas"
              )
ivs[[2]] <- c("psych_dist_lang", "Party_5pt",
              "Education", "Age", "Income"
              # , "California",
              # "Texas"
              )
ivs[[3]] <- c("distance_km", "psych_dist_lang", "Party_5pt",
            "Education", "Age", "Income"
            # ,  "California",
            # "Texas"
            )
ivs[[4]] <- c("distance_km*psych_dist_lang", "linked", "Party_5pt",
              "Education", 
              # "Mexican", 
              "Age", "Income"
              # , "Spanish"
              # , 
              # "California",
              # "Texas"
              )
ivs[[5]] <- c("distance_km*linked", "psych_dist_lang", "Party_5pt",
         "Education", 
         # "Mexican", 
         "Age", "Income"
         # , "Spanish"
         # , "California",
         # "Texas"
         )
ivs[[6]] <- c("distance_km*identity_strength_recoded", "psych_dist_lang", 
              "Party_5pt", "Education", 
              # "Mexican", 
              "Age", "Income"
              # , "Spanish"
              # , 
              # "California",
              # "Texas"
              )
ivs[[7]] <- c("psych_dist_lang*identity_strength_recoded", 
              "Party_5pt", "Education", 
              # "Mexican", 
              "Age", "Income"
              # , "Spanish"
              # ,
              # "California",
              # "Texas"
              )
ivs[[8]] <- c("psych_dist_lang*linked", "Party_5pt",
              "Education", 
              # "Mexican", 
              "Age", "distance_km"
              # , "Spanish"
              # , 
              # "California",
              # "Texas"
              )
ivs[[9]] <- c("psych_dist_lang*linked_simp*distance_km", "Party_5pt",
              "Education", "Age", "distance_km"
              # , "Spanish"
              # , 
              # "California",
              # "Texas"
)
ivs[[10]] <- c("psych_dist_lang*id_simp*distance_km", "Party_5pt",
              "Education", "Age", "distance_km"
              # , "Spanish"
              # , 
              # "California",
              # "Texas"
)
ivs[[11]] <- c("distance_km*id_simp", "psych_dist_lang", 
              "Party_5pt", "Education", 
              # "Mexican", 
              "Age", "Income"
              # , 
              # "California",
              # "Texas"
)
# ivs[[9]] <- c("poly(distance_km, 2)", "Party_5pt", 
#               "Education", "Age", "Income", "party_majority_state")


######## COMPLICATED/3wayINT LIST OF MODELS --------------------------------------------------
rev_ivs <- list()

rev_ivs[[1]] <- c("distance_km", "psych_dist_lang", "Party_5pt",
                  "Education", "Age", "Income")
rev_ivs[[2]] <- c("distance_km*psych_dist_lang", "linked", "Party_5pt",
              "Education", "Age", "Income")
rev_ivs[[3]] <- c("distance_km*psych_dist_lang", "identity_strength_recoded", 
                  "Party_5pt",
                  "Education", "Age", "Income")
rev_ivs[[4]] <- c( "distance_km*identity_strength_recoded", "psych_dist_lang", 
              "Party_5pt", "Education", "Age", "Income")
rev_ivs[[5]] <- c("distance_km*linked", "psych_dist_lang", 
                  "Party_5pt", "Education", "Age", "Income")
rev_ivs[[6]] <- c( "Party_5pt",
              "Education", "Age", "distance_km", "linked*psych_dist_lang")
rev_ivs[[7]] <- c( "Party_5pt",
                  "Education", "Age", "distance_km", 
                  "identity_strength_recoded*psych_dist_lang")
rev_ivs[[8]] <- c("distance_km*psych_dist_lang*identity_strength_recoded",
                  "Party_5pt","Education", "Age")
rev_ivs[[9]] <- c("distance_km*linked*psych_dist_lang", "Party_5pt",
                  "Education", "Age")

short_ivs <- list()

short_ivs[[1]] <- c("distance_km","Party_5pt", "linked",
                  "Education", "Age", "Income")
short_ivs[[2]] <- c("dist_sqd","Party_5pt", "linked",
                    "Education", "Age", "Income")
short_ivs[[3]] <- c("psych_dist_lang","Party_5pt", "linked",
                    "Education", "Age", "Income")
short_ivs[[4]] <- c("distance_km*linked",
                    "Party_5pt", "Education", "Age", "Income")
short_ivs[[5]] <- c("dist_sqd*linked",
                    "Party_5pt", "Education", "Age", "Income")
# short_ivs[[3]] <- c("distance_km*identity_strength_recoded",
#                     "Party_5pt", "Education", "Age", "Income")
short_ivs[[6]] <- c("distance_km*psych_dist_lang", "linked", "Party_5pt",
                  "Education", "Age", "Income")
short_ivs[[7]] <- c("dist_sqd*psych_dist_lang", "linked", "Party_5pt",
                    "Education", "Age", "Income")
short_ivs[[8]] <- c("psych_dist_lang*linked", "Party_5pt",
                    "Education", "Age", "Income")
# short_ivs[[5]] <- c("distance_km*psych_dist_lang", "identity_strength_recoded", 
#                     "Party_5pt", "Education", "Age", "Income")
# short_ivs[[6]] <- c("distance_km*psych_dist_lang*identity_strength_recoded",
#                     "Party_5pt","Education", "Age")
short_ivs[[9]] <- c("distance_km*psych_dist_lang*linked", "Party_5pt",
                    "Education", "Age")
short_ivs[[10]] <- c("dist_sqd*psych_dist_lang*linked", "Party_5pt",
                    "Education", "Age")


# short_ivs[[3]] <- c("distance_km*psych_dist_lang", "identity_strength_recoded", 
#                   "Party_5pt",
                  # "Education", "Age", "Income")
# short_ivs[[4]] <- c( "distance_km*identity_strength_recoded", "psych_dist_lang", 
#                    "Party_5pt", "Education", "Age", "Income")
# short_ivs[[5]] <- c("distance_km*linked", 
# #                   "Party_5pt", "Education", "Age", "Income")
# short_ivs[[6]] <- c( "Party_5pt",
#                    "Education", "Age", "distance_km", "linked*psych_dist_lang")
# short_ivs[[7]] <- c( "Party_5pt",
#                    "Education", "Age", "distance_km", 
#                    "identity_strength_recoded*psych_dist_lang")
# short_ivs[[8]] <- c("distance_km*psych_dist_lang*identity_strength_recoded",
#                   "Party_5pt","Education", "Age")
# short_ivs[[9]] <- c("distance_km*linked*psych_dist_lang", "Party_5pt",
#                   "Education", "Age")



### SIMPLIFIED, PHY DIST ONLY MODELS --------

dist_models <- list()
dist_models[[1]] <- c("distance_km", "Party_5pt",
                      "Education", "Age", "Income", "psych_dist_lang")
dist_models[[2]] <- c("border_state", "Party_5pt",
                      "Education", "Age", "Income", "psych_dist_lang")
dist_models[[2]] <- c("distance_km", "linked","Party_5pt", "Education", "Age", 
                      "Income", "psych_dist_lang")
dist_models[[3]] <- c("distance_km", "identity_strength_recoded","Party_5pt",
                      "Education", "Age", "Income", "psych_dist_lang")
dist_models[[4]] <- c("distance_km", "linked","Party_5pt",
                      "Education", "Age", "Income", "psych_dist_lang",
                      "distance_km*linked")
dist_models[[5]] <- c("distance_km", "linked","Party_5pt",
                      "Education", "Age", "Income", "psych_dist_lang",
                      "distance_km*identity_strength_recoded")



### New Models --------
dist_models <- list()
dist_models[[1]] <- c("distance_km", "Republican",
                      "Education", "Age", "Income", "Immigrant", "linked")
dist_models[[2]] <- c("Inclusive", "Republican",
                      "Education", "Age", "Income", "Immigrant", "linked")
dist_models[[3]] <- c("distance_km", "Inclusive", "Republican", "Education", "Age", 
                      "Income", "Immigrant", "linked")
dist_models[[4]] <- c("distance_km*Inclusive", "Republican",
                      "Education", "Age", "Income", "Immigrant", "linked")
dist_models[[5]] <- c("distance_km*Inclusive*linked", "Republican",
                      "Education", "Age", "Income", "Immigrant")

## Survey Design Weight Object & FULL regs  -----
latino_cmps <- full_cmps_lat %>% filter(distance_km < 965.606)
svy_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = latino_cmps)

bin_function(dvs_binomial, short_ivs, svy_cmps, latino_cmps, "full_bin")
ols_function(dvs_ols, short_ivs, svy_cmps, latino_cmps, "full_ols")
stargazer(full_bin$Increase_Border_Spending[1:2], 
          full_ols$border_security_recoded[1:2], type = "latex",
          dep.var.labels = c("Increase Border Spending, Including A Border Wall",
                             "Border Security as a National Priority")
          # ,
          # covariate.labels = c("Distance (in km)",
          #                      "Psych. Distance",
          #                      "Party", 
          #                      # "Linked Fate: Psych. Distance",
          #                      # "Distance (in km): Linked Fate: Psych. Distance",
          #                      "Linked Fate",
          #                      # "Identity Strength: Psych. Distance",
          #                      # "Identity Strength",
          #                      "Education","Age",
          #                      "Income",
          #                      "Distance (in km): Linked Fate",
          #                      "Distance (in km): Psych. Distance",
          #                      "Psych. Distance: Linked Fate",
          #                      "Distance (in km): Psych. Distance: Linked Fate",
          #                      "Constant")
)

stargazer(full_ols, type = "latex",
          dep.var.labels = "Border Security as a National Priority",
          covariate.labels = c("Distance (in km)",
                               "Psych. Distance",
                               "Party", 
                               # "Linked Fate: Psych. Distance",
                               # "Distance (in km): Linked Fate: Psych. Distance",
                               "Linked Fate",
                               # "Identity Strength: Psych. Distance",
                               # "Identity Strength",
                               "Education","Age",
                               "Income",
                               "Distance (in km): Linked Fate",
                               "Distance (in km): Psych. Distance",
                               "Psych. Distance: Linked Fate",
                               "Distance (in km): Psych. Distance: Linked Fate",
                               "Constant")
)
# running the spline regressions - - ------

spline3 <- svyglm(Increase_Border_Spending ~  splines::bs(distance_km, 
                                                          knots = c(321.869)) +
                    Party + Education + Income, 
                  data = full_cmps_lat, design = svy_cmps)
distance_seq <- seq(min(full_cmps_lat$distance_km, na.rm = TRUE), 
                    max(full_cmps_lat$distance_km, na.rm = TRUE), 
                    length.out = 100)

predicted_probs <- predict(spline3, 
                           newdata = data.frame(distance_km = distance_seq,
                                                Party = median(full_cmps_lat$Party),
                                                Education = median(full_cmps_lat$Education),
                                                Income = median(full_cmps_lat$Income)), 
                           type = "response")

plot_data <- data.frame(distance = distance_seq, predicted_probs)

# Plot the observed data points and the fitted spline curve -------
ggplot(full_cmps_lat, aes(x = distance_km, y = Increase_Border_Spending)) +
  geom_point() +
  geom_line(data = plot_data, aes(x = distance, y = predicted_probs), 
            color = "blue", linewidth = 1) +
  labs(title = "Spline Regression with Fourth-Degree Polynomial", 
       x = "Distance", y = "Probability") +
  theme_minimal()

### RUNS FOR FULL SAMPLE ------

bin_runs <- bin_function(dvs_binomial, rev_ivs, svy_cmps, full_cmps_lat, "bin_runs")

ols_runs <- ols_function(dvs_ols, rev_ivs, svy_cmps, full_cmps_lat, "ols_runs_full")

### visualizing int ------

plot_model(bin_runs$Increase_Border_Spending[[6]], type = "int", 
           legend.title = "Identity Strength")
stargazer(ols_runs_full, type = "text")

## tables for full samples ------------


stargazer(ols_runs_full, type = "latex",
          covariate.labels = c("Distance (in km)",
                               "Psych. Distance",
                               "Linked Fate: Psych. Distance",
                               "Distance (in km): Linked Fate: Psych. Distance",
                               "Linked Fate",
                               "Identity Strength: Psych. Distance",
                               "Identity Strength",
                               "Party", "Education","Age",
                               "Income",
                               "Distance (in km): Psych. Distance",
                               "Distance (in km): Identity Strength",
                               "Distance (in km):Linked Fate",
                               "Psych. Distance: Identity Strength",
                               "Distance (in km): Psych. Distance:Identity Strength",
                               "Constant"),
          dep.var.labels = "Increase Border Security as a National Priority")

stargazer(bin_runs$Increase_Border_Spending, type = "latex",
          covariate.labels = c("Distance (in km)", 
                               "Psych. Distance", 
                               "Linked Fate: Psych. Distance",
                               "Distance (in km): Linked Fate: Psych. Distance",
                               "Linked Fate", 
                               "Identity Strength: Psych. Distance",
                               "Identity Strength",
                               "Party", "Education", "Age",
                               "Income",
                               "Distance (in km): Psych. Distance",
                               "Distance (in km): Identity Strength",
                               "Distance (in km): Linked Fate",
                               "Psych. Distance: Identity Strength",
                               "Distance (in km): Psych. Distance: Identity Strength",
                               "Constant"),
          dep.var.labels = "Increase Border Spending, Including Wall")



### subset to under 200 miles (in kilometers) & under 100 ---------------

lat_200 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km < 321.869)
lat_500 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km < 804.672)

ca_500 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km < 804.672 &
                   full_cmps_lat$California == 1)
tx_500 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km < 804.672 &
                   full_cmps_lat$Texas == 1)
az_500 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km < 804.672 &
                   full_cmps_lat$State == 3)
nm_500 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km < 804.672 &
                   full_cmps_lat$State == 32)
lat_100 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km < 160.934)
lat_over_100 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km > 160.934)

######## Subsets by State -------------------------
dem <- subset(full_cmps_lat, subset = full_cmps_lat$State == 32 | full_cmps_lat$California == 1)
rep <- subset(full_cmps_lat, subset = full_cmps_lat$State == 3 | full_cmps_lat$Texas == 1)
nonborder <- subset(full_cmps_lat, subset = full_cmps_lat$border_state == 0)

### survey objects -------

svy_cmps_200 <- svydesign(id = ~ 1, weights = ~race_weight, data = lat_200)

ca_cmps_500 <- svydesign(id = ~ 1, weights = ~race_weight, data = ca_500)
tx_cmps_500 <- svydesign(id = ~ 1, weights = ~race_weight, data = tx_500)
az_cmps_500 <- svydesign(id = ~ 1, weights = ~race_weight, data = az_500)
nm_cmps_500 <- svydesign(id = ~ 1, weights = ~race_weight, data = nm_500)

svy_cmps_100 <- svydesign(id = ~ 1, weights = ~race_weight, data = lat_100)
svy_cmps_over100 <- svydesign(id = ~ 1, weights = ~race_weight, data = lat_over_100)


### SVY DESIGNS FOR STATES -------

dem_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = dem)
rep_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = rep)
rep_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = nonborder)
####### subset regressions ---------
bin_function(dvs_binomial, ivs, ca_cmps_500, ca_500, "ca_runs_500")
bin_function(dvs_binomial, ivs, tx_cmps_500, tx_500, "tx_runs_500")
bin_function(dvs_binomial, ivs, az_cmps_500, az_500, "az_runs_500")
bin_function(dvs_binomial, ivs, nm_cmps_500, nm_500, "nm_runs_500")


##### STATE REGS -------

bin_function(dvs_binomial, short_ivs, dem_cmps, dem, "dem_runs")
bin_function(dvs_binomial, short_ivs, rep_cmps, rep, "rep_runs")

ols_function(dvs_ols, short_ivs, dem_cmps, dem, "dem_runs_sec")
ols_function(dvs_ols, short_ivs, rep_cmps, rep, "rep_runs_sec")

# ols_function(dvs_ols, dist_models, dem_cmps, dem, "dem_runs_sec")
# ols_function(dvs_ols, dist_models, rep_cmps, rep, "rep_runs_sec")

###### OTHER REGS -------
bin_function(dvs_binomial, rev_ivs, dem_cmps_500, dem_500, "dem_runs")
rep_runs <- bin_function(dvs_binomial, rev_ivs, rep_cmps, rep, "rep_runs")


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

### Less Inclusive States Tables --------

stargazer(rep_runs, type = "latex",
          dep.var.labels = "Increase Border Spending, Including A Border Wall",
          covariate.labels = c("Distance (in km)",
                               "Psych. Distance",
                               "Party", 
                               # "Linked Fate: Psych. Distance",
                               # "Distance (in km): Linked Fate: Psych. Distance",
                               "Linked Fate",
                               # "Identity Strength: Psych. Distance",
                               # "Identity Strength",
                               "Education","Age",
                               "Income",
                               "Distance (in km): Linked Fate",
                               "Distance (in km): Psych. Distance",
                               "Psych. Distance: Linked Fate",
                               "Distance (in km): Psych. Distance: Linked Fate",
                               "Constant")
          )

stargazer(rep_runs_sec, type = "latex",
          dep.var.labels = "Border Security as a National Priority",
          covariate.labels = c("Distance (in km)",
                               "Psych. Distance",
                               "Party", 
                               # "Linked Fate: Psych. Distance",
                               # "Distance (in km): Linked Fate: Psych. Distance",
                               "Linked Fate",
                               # "Identity Strength: Psych. Distance",
                               # "Identity Strength",
                               "Education","Age",
                               "Income",
                               "Distance (in km): Linked Fate",
                               "Distance (in km): Psych. Distance",
                               "Psych. Distance: Linked Fate",
                               "Distance (in km): Psych. Distance: Linked Fate",
                               "Constant")
)

### More Inclusive States Tables ----------

stargazer(dem_runs, type = "latex",
          dep.var.labels = "Increase Border Spending, Including A Border Wall",
          covariate.labels = c("Distance (in km)",
                               "Psych. Distance",
                               "Party", 
                               # "Linked Fate: Psych. Distance",
                               # "Distance (in km): Linked Fate: Psych. Distance",
                               "Linked Fate",
                               # "Identity Strength: Psych. Distance",
                               # "Identity Strength",
                               "Education","Age",
                               "Income",
                               "Distance (in km): Linked Fate",
                               "Distance (in km): Psych. Distance",
                               "Psych. Distance: Linked Fate",
                               "Distance (in km): Psych. Distance: Linked Fate",
                               "Constant")
          )

stargazer(dem_runs_sec, type = "latex",
          dep.var.labels = "Border Security as a National Priority",
          covariate.labels = c("Distance (in km)",
                               "Psych. Distance",
                               "Party", 
                                # "Linked Fate: Psych. Distance",
                               # "Distance (in km): Linked Fate: Psych. Distance",
                               "Linked Fate",
                               # "Identity Strength: Psych. Distance",
                               # "Identity Strength",
                               "Education","Age",
                               "Income",
                               "Distance (in km): Linked Fate",
                               "Distance (in km): Psych. Distance",
                               "Psych. Distance: Linked Fate",
                               "Distance (in km): Psych. Distance: Linked Fate",
                               "Constant")
)


### Table to compare to White base models 
bind_base <- list(dem_runs$Increase_Border_Spending[[1]], 
                 dem_runs$Increase_Border_Spending[[2]])
binr_base <- list(rep_runs$Increase_Border_Spending[[1]], 
                  rep_runs$Increase_Border_Spending[[2]])
olsd_base <- list(dem_runs_sec$border_security_recoded[[1]],
                  dem_runs_sec$border_security_recoded[[2]])
olsr_base <- list(rep_runs_sec$border_security_recoded[[1]],
                  rep_runs_sec$border_security_recoded[[2]])

stargazer(bind_base,binr_base, type = "latex", 
          dep.var.labels = c("Increase Border Spending"),
          column.labels = c(
          "Most Inc.", "Most Inc.", "Least Inc.", "Least Inc."))

stargazer(olsd_base,olsr_base, type = "latex", 
          dep.var.labels = c("Make Border Security a National Priority"),
          column.labels = c(
            "Most Inc.", "Most Inc.", "Least Inc.", "Least Inc."))

#### BORDER VS NON BORDER  ----------------

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


########## Three-Way Interaction Plot --------------

"psych_dist_imm*linked*distance_km"


# Create a data frame for prediction
new_data <- expand.grid(
  psych_dist_imm = seq(min(tx_200$psych_dist_imm, na.rm = TRUE), 
                       max(tx_200$psych_dist_imm, 
                                                         na.rm = TRUE),
           length.out = 100),
  linked = seq(min(tx_200$linked, na.rm = TRUE), max(tx_200$linked, na.rm = TRUE), 
           length.out = 100),
  distance_km = seq(min(tx_200$distance_km, na.rm = TRUE), 
                    max(tx_200$distance_km, na.rm = TRUE), length.out = 100),
  Party_5pt = median(tx_200$Party_5pt),
  Education = median(tx_200$Education),
  Mexican = median(tx_200$Mexican),
  Age = median(tx_200$Age),
  Spanish = median(tx_200$Spanish)
)
stargazer(nm_runs_500$Increase_Border_Spending, type = "text")
model <- tx_under_200$Increase_Border_Spending[[9]]
summary(model)
# Add predicted values to the data frame
new_data$y_pred <- predict(model, newdata = new_data)

# Create a 3D scatter plot
ggplot(new_data, aes(x = x1, y = x2, z = y_pred)) +
  geom_point(aes(color = x3)) +
  labs(title = "Three-Way Interaction: x1 * x2 * x3", x = "x1", y = "x2", z = "y_pred") +
  theme_minimal() +
  theme(legend.title = element_text(text = "x3"))

