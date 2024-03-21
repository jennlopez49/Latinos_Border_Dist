### CLEANED FINAL MODELS 

mismatches <- data.frame(zips = c(96704,13456, 10001, 15001, 98104, 33101,
                                  33122, 28304, 56230, 40214, 33327, 84047), num = c(1:12))
cleaned_full <- full_cmps_lat %>% filter(! zips %in% mismatches$zips)
cleaned <- cleaned_full[!cleaned_full$State == 1 | cleaned_full$State == 12,]


cleaned_states <- cleaned %>% filter(State == 3 | State == 5 | State == 32 |
                                       State == 44)
cleaned_states$Remittances_Index <- ifelse(cleaned_states$Remit_Children == 1 |
                                             cleaned_states$Remit_Friends == 1 |
                                             cleaned_states$Remit_Grandparents == 1 |
                                             cleaned_states$Remit_OtherFam == 1 | 
                                             cleaned_states$Remit_Parents == 1, 1,
                                           0)
cleaned_states <- cleaned_states %>% mutate(
  Remittances_Scale = (Remit_Children + Remit_Friends + Remit_Grandparents +
                         Remit_OtherFam + Remit_Parents)
)
### List of models --------
short_ivs <- list()

short_ivs[[1]] <- c("distance_km","Party_5pt", "linked",
                    "Education", "age_sqd", "Income")
# short_ivs[[2]] <- c("dist_sqd","Party_5pt", "linked",
#                     "Education", "Age", "Income")
short_ivs[[2]] <- c("psych_dist_lang", "Party_5pt", "linked",
                    "Education", "age_sqd", "Income")
# short_ivs[[3]] <- c("Psych_Distance","Party_5pt", "linked",
#                     "Education", "Age", "Income")
short_ivs[[3]] <- c("distance_km*psych_dist_lang", "linked", "Party_5pt",
                    "Education", "age_sqd", "Income")
# short_ivs[[5]] <- c("distance_km*Psych_Distance", "linked", "Party_5pt",
#                     "Education", "Age", "Income")

no_nas_ivs <- list()

no_nas_ivs[[1]] <- c("log_dist","Party_5pt", "linked",
                    "Education", "age_sqd", "Income")
no_nas_ivs[[2]] <- c("family_birth", "missing_birth", "Party_5pt", "linked",
                    "Education", "age_sqd", "Income")
no_nas_ivs[[3]] <- c("log_dist","family_birth", "missing_birth", "Party_5pt", "linked",
                     "Education", "age_sqd", "Income")
no_nas_ivs[[4]] <- c("log_dist*family_birth", "missing_birth", "linked", "Party_5pt",
                    "Education", "age_sqd", "Income")
no_nas_ivs[[5]] <- c("log_dist","family_birth", "missing_birth", "Party_5pt", "linked",
                     "Education", "age_sqd", "Income", "Inclusive")
no_nas_ivs[[6]] <- c("log_dist*family_birth*Inclusive", "missing_birth", "linked", "Party_5pt",
                     "Education", "age_sqd", "Income")
# ### Setting up exclusive vs inclusive ------ 
# incl <- subset(cleaned, subset = cleaned$State == 32 | cleaned$California == 1)
# excl <- subset(cleaned, subset = cleaned$State == 3 | cleaned$Texas == 1)
# ## Excluding the mis-matches 

### Survey designs ------------------
new_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = cleaned_states)


incl <- subset(new_cmps, subset = new_cmps$variables$State == 32 | new_cmps$variables$California == 1)
excl <- subset(new_cmps, subset = new_cmps$variables$State == 3 | new_cmps$variables$Texas == 1)
### Running the models ---------------

### running on full sample to focus on null results - 
bin_function(dvs_binomial, no_nas_ivs, new_cmps,new_cmps, "full_sample_bin")
ols_function(dvs_ols, no_nas_ivs, new_cmps, new_cmps,"full_sample_ols")

bin_function(dvs_binomial, short_ivs, incl, incl, "incl_runs")
bin_function(dvs_binomial, short_ivs, excl, excl, "excl_runs")

ols_function(dvs_ols, short_ivs, incl, incl, "incl_runs_sec")
ols_function(dvs_ols, short_ivs, excl, excl, "excl_runs_sec")

### no nas--- all models except the full sample 3-way interaction
bin_function(dvs_binomial, no_nas_ivs[1:4], incl, incl, "incl_runs_nas")
bin_function(dvs_binomial, no_nas_ivs[1:4], excl, excl, "excl_runs_nas")

ols_function(dvs_ols, no_nas_ivs, incl, incl, "incl_runs_sec_nas")
ols_function(dvs_ols, no_nas_ivs, excl, excl, "excl_runs_sec_nas")

### Clustering standard errors at the state level for the interaction models ----
incl_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = incl)
excl_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = excl)

mod1 <- coeftest(incl_runs$Increase_Border_Spending[[3]], 
         vcov. = vcovCL(incl_runs$Increase_Border_Spending[[3]], 
                        cluster = ~State))


mod2 <- coeftest(excl_runs$Increase_Border_Spending[[3]], 
         vcov. = vcovCL(excl_runs$Increase_Border_Spending[[3]], 
                        cluster = ~State))
stargazer(mod1, mod2, type = "latex", 
          dep.var.labels = "Increase Border Spending, Including A Border Wall",
          covariate.labels = c("Distance (in km)",
                               "Acculturation",
                               "Party",
                               "Linked Fate",
                               "Education","Age",
                               "Income",
                               "Distance (in km): Acculturation",
                               "Constant"), column.labels = c("Most Inclusive",
                                                              "Least Inclusive"),
          column.separate = c(1,1))
### Producing the tables -------------

stargazer(full_sample_bin$Increase_Border_Spending[c(1,2,4,6)], type = "latex", 
          dep.var.labels = "Increase Border Spending, Including A Border Wall",
          covariate.labels = c("Distance Logged",
                               "Acculturation", "Inclusive", "Missing Acculturation",
                               "Party",
                               "Linked Fate",
                               "Education","Age",
                               "Income", "Distance (Logged): Acculturation",
                               "Distance (Logged): Inclusive",
                               "Acculturation: Inclusive",
                               "Distance (Logged): Acculturation: Inclusive",
                               "Constant"))

stargazer(full_sample_ols$border_security_recoded[c(1,2,4,6)], type = "latex", 
          dep.var.labels = "Increase Border Spending, Including A Border Wall",
          covariate.labels = c("Distance (in km)",
                               "Acculturation", "Inclusive", "Missing Acculturation",
                               "Party",
                               "Linked Fate",
                               "Education","Age",
                               "Income", "Distance (in km): Acculturation",
                               "Distance (in km): Inclusive",
                               "Acculturation: Inclusive",
                               "Distance (in km): Acculturation: Inclusive",
                               "Constant"))

stargazer(incl_runs, excl_runs, type = "latex", 
          dep.var.labels = "Increase Border Spending, Including A Border Wall",
          covariate.labels = c("Distance (in km)",
                               "Acculturation", "Missing Acculturation",
                               "Party",
                               "Linked Fate",
                               "Education","Age",
                               "Income",
                               "Distance (in km): Acculturation",
                               "Constant"), column.labels = c("Most Inclusive",
                                                              "Least Inclusive"),
          column.separate = c(3,3))


stargazer(incl_runs_sec, excl_runs_sec, type = "latex",
          dep.var.labels = "Border Security as a National Priority",
          covariate.labels = c("Distance (in km)",
                               "Acculturation","Missing Acculturation",
                               "Party", 
                               "Linked Fate",
                               "Education","Age",
                               "Income",
                               "Distance (in km): Acculturation",
                               "Constant"), column.labels = c("Most Inclusive",
                                                              "Least Inclusive"),
          column.separate = c(3,3))



### Testing the individual vars for the index -----
index_ivs <- list()

index_ivs[[1]] <- c("distance_km*Spanish", "Party_5pt", "linked",
                    "Education", "Age", "Income")
index_ivs[[2]] <- c("distance_km*Imm_Comm","Party_5pt", "linked",
                    "Education", "Age", "Income")
index_ivs[[3]] <- c("distance_km*Psych_Distance", "linked", "Party_5pt",
                    "Education", "Age", "Income")
bin_function(dvs_binomial, index_ivs, incl_cmps, incl, "incl_runs_index")
bin_function(dvs_binomial, index_ivs, excl_cmps, excl, "excl_runs_index")

ols_function(dvs_ols, index_ivs, incl_cmps, incl, "incl_runsind_sec")
ols_function(dvs_ols, index_ivs, excl_cmps, excl, "excl_runsind_sec")

#### Tables for Appendix  ---- 


stargazer(incl_runs_index, excl_runs_index, type = "latex", 
          dep.var.labels = "Increase Border Spending, Including A Border Wall",
          covariate.labels = c("Distance (in km)",
                               "Spanish",
                               "Part of Immigrant Community",
                               "Acculturation Index (Generation)",
                               "Party",
                               "Linked Fate",
                               "Education","Age",
                               "Income", 
                               "Distance (in km): Spanish",
                               "Distance (in km): Part of Immigrant Community",
                               "Distance (in km): Psych. Distance",
                               "Constant"), column.labels = c("Most Inclusive",
                                                              "Least Inclusive"),
          column.separate = c(3,3))



stargazer(incl_runsind_sec, excl_runsind_sec, type = "latex", 
          dep.var.labels = "Make Border Security a National Priority",
          covariate.labels = c("Distance (in km)",
                               "Spanish",
                               "Part of Immigrant Community",
                               "Acculturation Index (Generation)",
                               "Party",
                               "Linked Fate",
                               "Education","Age",
                               "Income", 
                               "Distance (in km): Spanish",
                               "Distance (in km): Part of Immigrant Community",
                               "Distance (in km): Generation",
                               "Constant"), column.labels = c("Most Inclusive",
                                                              "Least Inclusive"),
          column.separate = c(3,3))


#### Testing other specifications for distance --------
alt_ivs <- list()

alt_ivs[[1]] <- c("I(log(distance_km))","Party_5pt", "linked",
                    "Education", "age_sqd", "Income")
alt_ivs[[2]] <- c("dist_sqd","Party_5pt", "linked",
                    "Education", "age_sqd", "Income")
# alt_ivs[[3]] <- c("psych_dist_lang","Party_5pt", "linked",
#                     "Education", "age_sqd", "Income")
alt_ivs[[3]] <- c("I(log(distance_km))", "psych_dist_lang", "Party_5pt", "linked",
                  "Education", "age_sqd", "Income")
alt_ivs[[4]] <- c("I(log(distance_km))*psych_dist_lang", "linked", "Party_5pt",
                    "Education", "age_sqd", "Income")
alt_ivs[[5]] <- c("dist_sqd*psych_dist_lang", "linked", "Party_5pt",
                    "Education", "age_sqd", "Income")
# running the models 

bin_function(dvs_binomial, alt_ivs, incl_cmps, incl, "incl_runs_alt")
bin_function(dvs_binomial, alt_ivs, excl_cmps, excl, "excl_runs_alt")

ols_function(dvs_ols, alt_ivs, incl_cmps, incl, "incl_runs_sec_alt")
ols_function(dvs_ols, alt_ivs, excl_cmps, excl, "excl_runs_sec_alt")

##### Appendix Tables ------
stargazer(incl_runs_alt, excl_runs_alt, type = "latex",
          dep.var.labels = "Most Inclusive - Increase Border Spending, Including A Border Wall",
          covariate.labels = c("Distance (in km) Logged",
                               "Distance Sqd",
                               "Acculturation",
                               "Party",
                               "Linked Fate",
                               "Education","Age",
                               "Income", 
                               "Distance (in km) Logged: Acculturation",
                               "Distance (in km) Sqd: Acculturation",
                               "Constant"),column.labels = c("Most Inclusive",
                                                             "Least Inclusive"),
          column.separate = c(5,5))

stargazer(incl_runs_sec_alt, excl_runs_sec_alt, type = "latex",
          dep.var.labels = "Most Inclusive - Make Border Security a National Priority",
          covariate.labels = c("Distance (in km) Logged",
                               "Distance Sqd",
                               "Acculturation",
                               "Party",
                               "Linked Fate",
                               "Education","Age",
                               "Income", 
                               "Distance (in km) Logged: Acculturation",
                               "Distance (in km) Sqd: Acculturation",
                               "Constant"),column.labels = c("Most Inclusive",
                                                             "Least Inclusive"),
          column.separate = c(5,5))
