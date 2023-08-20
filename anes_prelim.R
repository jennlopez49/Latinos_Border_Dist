## migration variable --- 1:15, with 15 being furthest away ----

latinos_16 <- latinos_16 %>% mutate(
  p_born = case_when(V161315 == 1 ~ 3,
                     V161315 == 2 ~ 2,
                     V161315 == 3 ~ 1),
  g_born =  case_when(V161317 == 0 ~ 5,
                      V161317 == 1 ~ 4,
                      V161317 == 2 ~ 3,
                      V161317 == 3 ~ 2,
                      V161317 == 4 ~ 1),
  Migration_Dist = p_born*g_born, 
  Migration_Dist_Factor = case_when(Migration_Dist == 1 ~ 1,
                                    Migration_Dist == 2 ~ 1,
                                    Migration_Dist == 3 ~ 1,
                                    Migration_Dist == 4 ~ 1,
                                    Migration_Dist == 5 ~ 2,
                                    Migration_Dist == 6 ~ 2,
                                    Migration_Dist == 7 ~ 2,
                                    Migration_Dist == 8 ~ 2,
                                    Migration_Dist == 9 ~ 2,
                                    Migration_Dist == 10 ~ 2,
                                    Migration_Dist == 11 ~ 3,
                                    Migration_Dist == 12 ~ 3,
                                    Migration_Dist == 13 ~ 3,
                                    Migration_Dist == 14 ~ 3,
                                    Migration_Dist == 15 ~ 3),
  Identity_Imp = case_when(Identity_Importance == "Not at all Important" ~ "Low",
                           Identity_Importance == "A little Important" ~ "Low",
                           Identity_Importance == "Moderately Important" ~ "Med",
                           Identity_Importance == "Very Important" ~ "High",
                           Identity_Importance == "Extremely Important" ~ "High"),
  Migration_Dist_Add = p_born + g_born,
  Grandparents_Short = case_when(V161317 == 0 ~ 0,
                                 V161317 > 0 & V161317 < 3 ~ 1,
                                 V161317 > 2 ~ 2),
  Migration_Dist_Add_Short = Grandparents_Short + p_born,
  scaled_p = scale(latinos_16$p_born),
  scaled_g = scale(latinos_16$g_born),
  Migration_Dist_Add_Scaled = scaled_p + scaled_g
)

latinos_20 <- latinos20 %>% mutate(
  p_born = case_when(V201553 == 1 ~ 3,
                     V201553 == 2 ~ 2,
                     V201553 == 3 ~ 1),
  g_born =  case_when(V201555 == 0 ~ 5,
                      V201555 == 1 ~ 4,
                      V201555 == 2 ~ 3,
                      V201555 == 3 ~ 2,
                      V201555 == 4 ~ 1),
  Migration_Dist = p_born*g_born
)
## Checking ---- cronbach's alpha 
born_scaled <- data.frame(p = latinos_16$scaled_p, g = latinos_16$scaled_g)
born_index <- data.frame(p = latinos_16$p_born, g = latinos_16$g_born)

cronbach.alpha(born_index, CI = TRUE, na.rm = TRUE)
cronbach.alpha(born_scaled, CI = TRUE, na.rm = TRUE)

# re-leveling migration dist to make it easier to understand 

latinos_16$Migration_Dist_Factor <-as.factor(latinos_16$Migration_Dist_Factor)
latinos_16$Migration_Dist_Factor <-relevel(latinos_16$Migration_Dist_Factor,
                                           ref = "High Dist")
latinos_16$Gender <-relevel(as.factor(latinos_16$Gender), ref = "Male")
latinos_16$Identity_Imp <-relevel(as.factor(latinos_16$Identity_Imp), 
                                         ref = "High")
latinos_16$Identity_Importance <-relevel(as.factor(latinos_16$Identity_Importance), 
                                  ref = "Not at all Important")

## setting up the survey design --------
svy_16<- svydesign(id = ~ 1, weights = ~V160101, data = latinos_16)

svy_20 <- svydesign(id = ~ 1, weights = ~V200010a, data = latinos20)

# baseline models 2016  --- as OLS --------

base_model <- svyglm(Border_Reordered ~ Age + Ideology + Party + Identity_Importance + 
                    Gender + 
                      Education +
                      Migration_Dist, data = latinos_16, design = svy_16)
base_model_fact <- svyglm(Border_Reordered ~ Age + Ideology + Party + Identity_Importance + 
                       Gender + 
                       Education +
                         Migration_Dist_Factor, 
                       data = latinos_16, design = svy_16)
base_ols <- lm(Border_Reordered ~ Age + Ideology + Party + Identity_Importance + 
                            Education +
                 Migration_Dist_Add_Scaled, 
                          data = latinos_16)

## Identity * Psych Dist 2016 -----------

psych_model <- svyglm(Border_Reordered ~ Age + Ideology + Party + 
                       Gender +
                        Education +
                        Migration_Dist*Identity_Importance, data = latinos_16, 
                       design = svy_16)
psych_model_fact <- svyglm(Border_Reordered ~ Age + Ideology + Party + 
                        Gender +
                        Education +
                         Migration_Dist_Factor*Identity_Importance, 
                       data = latinos_16, 
                       design = svy_16)
summary(psych_model)

psych_ols <- lm(Border_Reordered ~ Age + Party + Education + 
                  Migration_Dist*Identity_Importance, data = latinos_16)
summary(psych_ols)
# printing table of both for initial findings

stargazer(base_model,base_model_fact, psych_model, psych_model_fact,
          type = "text", out = "intial_08.html")

stargazer(base_ols,psych_ols, 
          type = "text", out = "ols_08.html")

# baseline models 2020  --- as OLS --------

base_model_20 <- svyglm(V201426x ~ Age + Ideology + Party + 
                       Gender + Education + Economy_Past + Attention_Politics, 
                       data = latinos_20, 
                     family = "gaussian", rescale = TRUE, design = svy_20)
summary(base_model_20)

psych_model_20 <- svyglm(V201426x ~ Age + Ideology + Party + 
                          Gender + Education + Economy_Past + 
                           Attention_Politics + Migration_Dist, 
                        data = latinos_20, 
                        family = "gaussian", rescale = TRUE, design = svy_20)
### models with border wall binary var + binomial models --------------
# bin_base <- svyglm(Border_Bin ~ Age + Ideology + Party + Identity_Importance + 
#                             Gender + 
#                             Education +
#                             Migration_Dist_Factor, 
#                           data = latinos_16, 
#                           family = binomial(link = "logit"), rescale = TRUE, 
#                           design = svy_16)
# 
# bin_psych <- svyglm(Border_Bin ~ Age + Ideology + Party + 
#                              Gender +
#                              Education +
#                              Migration_Dist_Factor*Identity_Importance, 
#                            data = latinos_16, family = binomial(link = "logit"),
#                            rescale = TRUE, design = svy_16)
# stargazer(bin_base, bin_psych, type = "text", out = "binomial_08.html")