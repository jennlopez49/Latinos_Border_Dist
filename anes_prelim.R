## setting up the survey design 
svy_16<- svydesign(id = ~ 1, weights = ~V160101, data = latinos_16)

svy_20 <- svydesign(id = ~ 1, weights = ~V200010a, data = latinos20)

## migration variable --- 1:15, with 15 being furthest away 

latinos_16 <- latinos_16 %>% mutate(
  p_born = case_when(V161315 == 1 ~ 3,
                     V161315 == 2 ~ 2,
                     V161315 == 3 ~ 1),
  g_born =  case_when(V161317 == 0 ~ 5,
                      V161317 == 1 ~ 4,
                      V161317 == 2 ~ 3,
                      V161317 == 3 ~ 2,
                      V161317 == 4 ~ 1),
  Migration_Dist = p_born*g_born
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
## Checking
#table(latinos_16$Migration_Dist)

# baseline models 2016  --- as OLS --------

base_model <- svyglm(V161196x ~ Age + Ideology + Party + Identity_Importance + 
                    Gender + 
                      Education +
                      Migration_Dist, data = latinos_16, 
                  family = "gaussian", rescale = TRUE, design = svy_16)
summary(base_model)


## Identity * Psych Dist 2016 -----------

psych_model <- svyglm(V161196x ~ Age + Ideology + Party + 
                       Gender +
                        Education +
                        Migration_Dist*Identity_Importance, data = latinos_16, 
                     family = "gaussian", rescale = TRUE, design = svy_16)
summary(psych_model)

# printing table of both for initial findings

stargazer(base_model, psych_model, type = "text", out = "intial_08.html")

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