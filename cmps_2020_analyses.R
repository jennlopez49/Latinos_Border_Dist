#### preliminary analyses 

## Setting up Multiple DVs 
dvs <- c("border_sec_first", "Increase_BorderSpend_Wall", "Belong_USSociety",
         "Accepted_Included_USSoc", "Value_Respect_inUSSoc")


## IVs 
ivs <- list()
ivs[[1]] <- c("distance_km", "Party_5pt",
              "Education", "Age", "Income")
ivs[[2]] <- c("Psych_Distance", "Party_5pt",
              "Education", "Age", "Income")
ivs[[3]] <- c("distance_km", "Psych_Distance", "Party_5pt",
            "Education", "Age", "Income")
ivs[[4]] <- c("distance_km", "Psych_Distance", "Linked_Fate", "Party_5pt",
              "Education", "Mexican", "Age", "Income", "Spanish")
ivs[[5]] <- c("distance_km", "Psych_Distance", "identity_strength", "Party_5pt",
         "Education", "Mexican", "Age", "Income", "Spanish")
ivs[[6]] <- c("distance_km*identity_strength", "Psych_Distance", "Party_5pt",
            "Education", "Mexican", "Age", "Income", "Spanish")
ivs[[7]] <- c("distance_km*Linked_Fate", "Psych_Distance", "Party_5pt",
              "Education", "Mexican", "Age", "Income", "Spanish")
ivs[[8]] <- c("distance_km*Linked_Fate", "Psych_Distance", "Party_5pt",
              "Education", "Mexican", "Age", "Income", "Spanish", "Remit", 
              "ImmEnf_Enc_PosNeg")
ivs[[9]] <- c("distance_km*Psych_Distance", "Party_5pt",
              "Education", "Age", "Income", "identity_strength")
## Survey Design Weight Object -- 

svy_cmps <- svydesign(id = ~ 1, weights = ~weight, data = latino_any_sub)



### prelim --- survey package
lm_mods1 <- list()

for (Y in dvs) {
  mod1 <- list()
  for (i in 1:length(ivs)) {
    form <- as.formula(paste(Y, " ~ ", paste(ivs[[i]], collapse = " + ")))
    mod1[[i]] <- svyglm(form, data=latino_any_sub, design = svy_cmps) 
    
    
  }
  lm_mods1[[Y]] <- mod1
}


### prelim --- weighted least squares

lm_mods_weight <- list()

for (Y in dvs) {
  mod1_weight <- list()
  for (i in 1:length(ivs)) {
    form <- as.formula(paste(Y, " ~ ", paste(ivs[[i]], collapse = " + ")))
    mod1_weight[[i]] <- lm(form, data=latino_any_sub, weights = weight) 
    
    
  }
  lm_mods_weight[[Y]] <- mod1_weight
}

summary(lm_mods_weight$border_sec_first[[9]])

## tables for full samples 

stargazer(lm_mods1$border_sec_first, type = "text")
stargazer(lm_mods_weight$border_sec_first[3:9], type = "text")

stargazer(lm_mods1$Increase_BorderSpend_Wall, type = "text")

stargazer(lm_mods1$Belong_USSociety, type = "text")

stargazer(lm_mods1$Accepted_Included_USSoc, type = "text")

stargazer(lm_mods1$Value_Respect_inUSSoc, type = "text")


### subset to under 200 miles (in kilometers)

latino_under_200 <- subset(latino_any_sub, subset = latino_any_sub$distance_km < 321.869)

latino_under_100 <- subset(latino_any_sub, subset = latino_any_sub$distance_km < 160.934)

### subset runs

svy_cmps_200 <- svydesign(id = ~ 1, weights = ~weight, data = latino_under_200)

mods_200 <- list()

for (Y in dvs) {
  mod2 <- list()
  for (i in 1:length(ivs)) {
    form <- as.formula(paste(Y, " ~ ", paste(ivs[[i]], collapse = " + ")))
    mod2[[i]] <- svyglm(form, data=latino_under_200, design = svy_cmps_200) 
    
    
  }
  mods_200[[Y]] <- mod2
}


### tables for 200 mile subset 
stargazer(mods_200$border_sec_first, type = "text")

stargazer(mods_200$Increase_BorderSpend_Wall, type = "text")

stargazer(mods_200$Belong_USSociety, type = "text")

stargazer(mods_200$Accepted_Included_USSoc, type = "text")

stargazer(mods_200$Value_Respect_inUSSoc, type = "text")
