#### preliminary analyses 

## Setting up Multiple DVs 
dvs <- c("border_sec_first", "Increase_Border_Spending"
         # , "Belong_USSociety",
         # "Accepted_Included_USSoc", "Value_Respect_inUSSoc"
         )


## IVs 
ivs <- list()
ivs[[1]] <- c("distance_km", "Party_5pt", 
              "Education", "Age", "Income", "party_majority_state")
ivs[[2]] <- c("Psych_Distance", "Party_5pt",
              "Education", "Age", "Income", "party_majority_state")
ivs[[3]] <- c("distance_km", "Psych_Distance", "Party_5pt",
            "Education", "Age", "Income", "party_majority_state")
ivs[[4]] <- c("distance_km*Psych_Distance", "Linked_Fate", "Party_5pt",
              "Education", "Mexican", "Age", "Income", "Spanish", "party_majority_state")
ivs[[5]] <- c("distance_km*Linked_Fate", "Psych_Distance", "Party_5pt",
         "Education", "Mexican", "Age", "Income", "Spanish", "party_majority_state")
ivs[[6]] <- c("distance_km*identity_strength_recoded", "Psych_Distance", "Party_5pt",
            "Education", "Mexican", "Age", "Income", "Spanish", "party_majority_state")
# ivs[[7]] <- c("distance_km*American_Imp", "Psych_Distance", "Party_5pt",
#               "Education", "Mexican", "Age", "Income", "Spanish", "party_majority_state")
# # ivs[[7]] <- c("distance_km*Linked_Fate", "Psych_Distance", "Party_5pt",
#               "Education", "Mexican", "Age", "Income", "Spanish")
# ivs[[8]] <- c("distance_km*Linked_Fate", "Psych_Distance", "Party_5pt",
#               "Education", "Mexican", "Age", "Income", "Spanish")
# ivs[[9]] <- c("distance_km*Linked_Fate", "Psych_Distance", "Party_5pt",
#               "Education", "Mexican", "Age", "Income", "Spanish", "Remit", 
#               "ImmEnf_Enc_PosNeg")
# ivs[[10]] <- c("distance_km*Psych_Distance", "Party_5pt",
#               "Education", "Age", "Income", "identity_strength")
## Survey Design Weight Object -- 

svy_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = full_cmps_lat)

# # trying to subset overall 
# subset_des <- subset(svy_cmps, subset = "Hispanic" == 1)


### prelim --- survey package
lm_mods1 <- list()

for (Y in dvs) {
  mod1 <- list()
  for (i in 1:length(ivs)) {
    form <- as.formula(paste(Y, " ~ ", paste(ivs[[i]], collapse = " + ")))
    mod1[[i]] <- svyglm(form, design = svy_cmps, family = gaussian()) 
    
    
  }
  lm_mods1[[Y]] <- mod1
}



## tables for full samples 

stargazer(lm_mods1$border_sec_first, type = "text")

stargazer(lm_mods1$Increase_Border_Spending[c(1:6)], type = "text", 
          covariate.labels = c("Distance (in km)", "Identity Strength",
                               "Psychological Distance", "Linked Fate",
                               "Party", "Education", "Mexican", "Age",
                               "Income", "Spanish", 
                               "State Leg Partisan Majority - Divided",
                               "State Leg Partisan Majority - Rep",
                               "Distance (in km):Psychological Distance",
                               "Distance (in km):Linked Fate",
                               "Distance (in km): Identity Strength",
                               "Constant"),
          dep.var.labels = "Increase Border Spending, Including Wall",
          out = "table_1_full.html")

# stargazer(lm_mods1$Belong_USSociety, type = "text")
# 
# stargazer(lm_mods1$Accepted_Included_USSoc, type = "text")
# 
# stargazer(lm_mods1$Value_Respect_inUSSoc, type = "text")


### subset to under 200 miles (in kilometers) & under 100 

lat_200 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km < 321.869)
lat_100 <- subset(full_cmps_lat, subset = full_cmps_lat$distance_km < 160.934)

### 

svy_cmps_200 <- svydesign(id = ~ 1, weights = ~race_weight, data = lat_200)

svy_cmps_100 <- svydesign(id = ~ 1, weights = ~race_weight, data = lat_100)


mods_200 <- list()

for (Y in dvs) {
  mod2 <- list()
  for (i in 1:length(ivs)) {
    form <- as.formula(paste(Y, " ~ ", paste(ivs[[i]], collapse = " + ")))
    mod2[[i]] <- svyglm(form, design = svy_cmps_200) 
    
    
  }
  mods_200[[Y]] <- mod2
}


mods_100 <- list()

for (Y in dvs) {
  mod_100 <- list()
  for (i in 1:length(ivs)) {
    form <- as.formula(paste(Y, " ~ ", paste(ivs[[i]], collapse = " + ")))
    mod_100[[i]] <- svyglm(form, design = svy_cmps_100) 
    
    
  }
  mods_100[[Y]] <- mod_100
}

### tables for 200 mile subset 
stargazer(mods_200$border_sec_first, type = "text")

stargazer(mods_200$Increase_Border_Spending, type = "text")

# stargazer(mods_200$Belong_USSociety, type = "text")
# 
# stargazer(mods_200$Accepted_Included_USSoc, type = "text")
# 
# stargazer(mods_200$Value_Respect_inUSSoc, type = "text")


### tables for 100 mile subset 
stargazer(mods_100$border_sec_first, type = "text")

stargazer(mods_100$Increase_Border_Spending, type = "text")

# stargazer(mods_100$Belong_USSociety, type = "text")
# 
# stargazer(mods_100$Accepted_Included_USSoc, type = "text")
# 
# stargazer(mods_100$Value_Respect_inUSSoc, type = "text")


# plots 

library(sjPlot)

plot_model(lm_mods1$Increase_Border_Spending[[6]], type = "int")
