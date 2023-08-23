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
  Grandparents_Short = case_when(V161317 == 0 ~ 1,
                                 V161317 > 0 & V161317 < 3 ~ 2,
                                 V161317 > 2 ~ 3),
  Migration_Dist_Add_Short = Grandparents_Short + p_born,
  Education_Cont = case_when(Education == "Less than HS" ~ 1,
                             Education == "HS" ~ 2,
                             Education == "Some College" ~ 3,
                             Education == "Bachelor's" ~ 4,
                             Education == "Post-Graduate" ~ 5),
  Party_Cont = case_when(Party == "Strong Democrat" ~ 1,
                         Party == "Not very strong Democrat" ~ 2,
                         Party == "Independent-Democrat" ~ 3,
                         Party == "Independent" ~ 4,
                         Party == "Independent-Republican" ~ 5,
                         Party == "Not very strong Republican" ~ 6,
                         Party == "Strong Republican" ~ 7),
  Migration_Lang_Mult = Migration_Dist*Language_Cont,
  Migration_Lang_Add = Migration_Dist_Add*Language_Cont,
  Migration_Lang_Add_Short = Migration_Dist_Add_Short + Language_Cont,
  Migration_Weighted = p_born^2 + g_born
  # scaled_p = scale(latinos_16$p_born),
  # scaled_g = scale(latinos_16$g_born),
  # Migration_Dist_Add_Scaled = scaled_p + scaled_g
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


latinos_16$Border_Wall <- as.factor(latinos_16$Border_Wall)

## setting up the survey design --------
svy_16<- svydesign(id = ~ V160201, weights = ~V160102, strata = ~ V160202, 
                   nest = TRUE, survey.lonely.psu = "adjust",
                   data = latinos_16)

svy_20 <- svydesign(id = ~ 1, weights = ~V200010a, data = latinos20)

# baseline models 2016  --- as OLS --------
# base_model <- svyglm(Border_Reordered ~ Age + Ideo_8 + Party_Cont +
#                        Education_Cont +
#                        I(p_born * g_born), data = latinos_16, design = svy_16)

base_model <- svyglm(Border_Reordered ~ Age + Ideo_8 + Party_Cont +
                      Education_Cont +
                      Migration_Dist_Add, data = latinos_16, design = svy_16)

# base_model_lang <- svyglm(Border_Reordered ~ Age + Ideo_8 + Party_Cont +
#                             Education_Cont +
#                             Migration_Lang_Add,
#                        data = latinos_16, design = svy_16)
base_ols <- lm(Border_Reordered ~ Age + 
                Ideo_8 + Party_Cont +
                 # Identity_Importance + 
                            Education_Cont + 
                 # Language + Latin_Country + 
                 Migration_Dist_Add, 
                          data = latinos_16)
# base_ols_lang <- lm(Border_Reordered ~ Age + 
#                       # Ideo_8 + Party_Cont +
#                  # Identity_Importance + 
#                  Education_Cont + 
#                  # Language + Latin_Country + 
#                  Migration_Lang_Mult, 
#                data = latinos_16)

## Identity * Psych Dist 2016 -----------
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

psych_dist_w <- svyglm(Border_Reordered ~ Age + Ideo_8 + Party_Cont + Education_Cont + 
                         Economy_PastYear + Attention_to_Politics  +
                         Latin_Country +
                         Migration_Dist_Add*Latino_Identity,
                           data = latinos_16, 
                           design = svy_16)

psych_att_w <- svyglm(Border_Reordered ~ Age + Ideo_8 + Party_Cont + Education_Cont + 
                        Latino_Identity +Economy_PastYear + 
                        Latin_Country +
                        Migration_Dist_Add*Attention_to_Politics,
                       data = latinos_16, 
                       design = svy_16)
summary(psych_model)

psych_ols <- lm(Border_Reordered ~ Age + Ideo_8 + Party_Cont + Education_Cont + 
                 Economy_PastYear + Attention_to_Politics + 
                  Latin_Country +
                  Migration_Dist_Add*Latino_Identity, data = latinos_16)

att_ols <- lm(Border_Reordered ~ Age + Ideo_8 + Party_Cont + Education_Cont + 
                Economy_PastYear + Latino_Identity +
                 Latin_Country +
                Migration_Dist_Add*Attention_to_Politics, data = latinos_16)
summary(psych_ols)
# printing table of both for initial findings

stargazer(base_model,psych_dist_w, psych_att_w,
          type = "text",   dep.var.labels = "Attitude Towards Border Wall"
          ,
          covariate.labels = c("Age", "Ideology", "Party",
                               "Education",
                               "Economy - Much Better",
                               "Economy - Much Worse",
                               "Economy - Somewhat Better",
                               "Economy - Somewhat Worse",
                               "Attention to Politics - Always",
                               "Attention to Politics - Most of the time",
                               "Attention to Politics - Never",
                               "Attention to Politics - Some of the time",
                               "Distance to Migration Experience|Attention to Politics - Always",
                               "Distance to Migration Experience|Attention to Politics - Most of the time",
                               "Distance to Migration Experience|Attention to Politics - Never",
                               "Distance to Migration Experience|Attention to Politics - Some of the time",
                               "Hispanic Ancestry - Mexican",
                               "Hispanic Ancestry - Other/More than one",
                               "Hispanic Ancestry - Puerto Rican",
                               "Distance to Migration Experience",
                               "Latino Identity",
                               "Distance to Migration Experience|Latino Identity")
          , out = "weighted_2016.html"
          )

stargazer(base_ols, psych_ols, att_ols,
          type = "text",
          dep.var.labels = "Attitude Towards Border Wall"
          ,
          covariate.labels = c("Age", "Ideology", "Party",
                               "Education",
                               "Economy - Much Better",
                               "Economy - Much Worse",
                               "Economy - Somewhat Better",
                               "Economy - Somewhat Worse",
                               "Attention to Politics - Always",
                               "Attention to Politics - Most of the time",
                               "Attention to Politics - Never",
                               "Attention to Politics - Some of the time",
                               "Distance to Migration Experience|Attention to Politics - Always",
                               "Distance to Migration Experience|Attention to Politics - Most of the time",
                               "Distance to Migration Experience|Attention to Politics - Never",
                               "Distance to Migration Experience|Attention to Politics - Some of the time",
                               "Hispanic Ancestry - Mexican",
                               "Hispanic Ancestry - Other/More than one",
                               "Hispanic Ancestry - Puerto Rican",
                               "Distance to Migration Experience",
                               "Latino Identity",
                               "Distance to Migration Experience|Latino Identity")
          , out = "ols_2016.html"
          )

