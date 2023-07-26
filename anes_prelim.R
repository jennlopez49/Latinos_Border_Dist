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

## Checking
#table(latinos_16$Migration_Dist)

# baseline models --- as OLS 

base_model <- svyglm(V161196x ~ Age + Ideology + Party + Identity_Importance + 
                    Gender + Migration_Dist, data = latinos_16, 
                  family = "gaussian", rescale = TRUE, design = svy_16)
summary(base_model)


## Identity * Psych Dist 

psych_model <- svyglm(V161196x ~ Age + Ideology + Party + 
                       Gender + Migration_Dist*Identity_Importance, data = latinos_16, 
                     family = "gaussian", rescale = TRUE, design = svy_16)
summary(psych_model)
