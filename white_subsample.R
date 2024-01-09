### SURVEY DESIGN 
white_sub <- full_cmps %>% filter(S2_Race_Prime == 1 & Hispanic == 0 &
                                    distance_km < 965.606)
white_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = white_sub)

## Base Model
base <- list()
base[[1]] <- c("distance_km","Party_5pt",
                    "Education", "Age", "Income")
base[[2]] <- c("dist_sqd","Party_5pt",
               "Education", "Age", "Income")
# base[[3]] <- c("distance_km","Party_5pt",
#                "Education", "Age", "Income", "Inclusive")
# base[[4]] <- c("dist_sqd","Party_5pt",
#                "Education", "Age", "Income", "Inclusive")

## Mods 
bin_function(dvs_binomial, base,white_cmps, white_sub, "base_white_bin")
ols_function(dvs_ols, base,white_cmps, white_sub, "base_white_ols")

## Full Table 

stargazer(base_white_bin, base_white_ols, type = "latex",
          dep.var.labels = c("Increase Border Spending", 
                             "Border Security as a National Priority"))
### Most inclusive vs least inclusive sub-sample 

l_inc <- white_sub %>% filter(Inclusive == 0)
m_inc <- white_sub %>% filter(Inclusive == 1)

# svy design
l_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = l_inc)
m_cmps <- svydesign(id = ~ 1, weights = ~race_weight, data = m_inc)


# least inc 
bin_function(dvs_binomial, base,l_cmps, l_inc, "linc_white_bin")
ols_function(dvs_ols, base,l_cmps, l_inc, "linc_white_ols")

# most inc 
bin_function(dvs_binomial, base,m_cmps, m_inc, "minc_white_bin")
ols_function(dvs_ols, base,m_cmps, m_inc, "minc_white_ols")

stargazer(linc_white_bin, minc_white_bin, linc_white_ols, minc_white_ols,
          type = "text",
          dep.var.labels = c("Increase Border Spending", 
                             "Border Security as a National Priority"),
          column.labels = c("Least Inclusive", "Least Inclusive", "Most Inclusive", 
                            "Most Inclusive","Least Inclusive","Least Inclusive",
                            "Most Inclusive", "Most Inclusive"))
### MARGINAL EFFECTS 
m <- margins(base_white_bin$Increase_Border_Spending[[2]], design = white_cmps)
plot(m)
