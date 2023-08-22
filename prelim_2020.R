### Preliminary Regressions 2020 -----------------------------------------------
latinos20 <- latinos20_clean[!is.na(latinos20_clean$V200010b),]

latinos20$Lat_Identity <- latinos20$Linked_Fate + latinos20$Hispanic_Candidate
latinos20$Migration_Language <- latinos20$Migration_Dist + latinos20$Language_Cont
## setting up the survey design for weighting --------

svy_20 <- svydesign(id = ~ V200010c, weights = ~V200010b,
                    strata = ~ V200010d, survey.lonely.psu = "adjust", nest = TRUE,
                    data = latinos20)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

# baseline models 2020  --- as OLS and Weighted --------
base_ols_20 <- lm(Border_Wall ~ Age + Ideo_8 + Party_Cont + 
                        Gender + Education_Cont + Migration_Dist, 
                      data = latinos20)

base_weight <- svyglm(Border_Wall ~ Age + Ideo_8 + Party_Cont + 
                          Gender + Education_Cont + Migration_Dist, 
                        data = latinos20, design = svy_20)
summary(base_weight)

## migration dist interaction models --------------------

psych_ols_20 <- lm(Border_Wall ~ Age + Ideo_8 + Party_Cont + 
                         Gender + Education_Cont 
                    + Economy_Past + Latino_Identity
                   # + Attention_Politics
                   + Hispanic_Ancestry +
                       + Migration_Dist*Attention_Politics, 
                       data = latinos20)

psych_weight <- svyglm(Border_Wall ~ Age + Ideo_8 + Party_Cont + 
                           Gender + Education_Cont + Economy_Past +
                         + Latino_Identity +
                         # Attention_Politics + 
                         Hispanic_Ancestry +
                         Migration_Dist*Attention_Politics, 
                         data = latinos20, design = svy_20)

psych_lat_20 <- lm(Border_Wall ~ Age + Ideo_8 + Party_Cont + 
                     Gender + Education_Cont 
                   + Economy_Past 
                   # + Lat_Identity
                    + Attention_Politics
                   + Hispanic_Ancestry +
                     + Migration_Dist*Latino_Identity, 
                   data = latinos20)

psych_lat_weight <- svyglm(Border_Wall ~ Age + Ideo_8 + Party_Cont + 
                         Gender + Education_Cont + Economy_Past +
                         # + Lat_Identity +
                          Attention_Politics + 
                         Hispanic_Ancestry +
                         Migration_Dist*Latino_Identity, 
                       data = latinos20, design = svy_20)

