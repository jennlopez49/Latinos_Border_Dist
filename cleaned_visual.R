### Cleaned code for figures
### Table 1 Full Sample Plot -----
full_sample_plot <-plot_model(full_sample_bin$Increase_Border_Spending[[4]], type = "int",
           show.legend = TRUE) + 
  labs(x= "Distance (in km)") + theme_bw() +
  scale_color_discrete(name = "Acculturation",
                       labels = c("Lowest", "Highest"))
ggsave("full_sampple_plot.pdf", width = 7, height = 4)
### Table 2 Plots ---------------------
## Plots
mod2 <- plot_model(incl_runs_nas$Increase_Border_Spending[[3]], type = "int",
                   show.legend = TRUE) + 
  labs(x= "Distance (in km)", title = "Most Inclusive") + theme_bw() +
  scale_color_discrete(name = "Acculturation",
                       labels = c("Lowest", "Highest"))  + 
  facet_wrap(~ "Increase Border Spending") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) 
mod3 <- plot_model(excl_runs_nas$Increase_Border_Spending[[3]], type = "int") + 
  labs(x = "Distance (in km)", y = element_blank(), title = "Least Inclusive")
mod3_leg <- mod3 + theme_bw() + scale_color_discrete(name = "Acculturation",
                                        labels = c("Lowest", "Highest")) + 
  facet_wrap(~ "Increase Border Spending") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme(strip.text.x = element_blank())
mod2_noleg <- mod2 + theme(legend.position = "none", 
                           strip.text.x = element_blank())

fig1_combo <- mod2_noleg + mod3_leg

ggsave("fig1_combo.pdf", width = 7, height = 4)



### Table 3 Plots ------------------

mod2_2 <- plot_model(incl_runs_sec_nas$border_security_recoded[[3]], type = "int",
                   show.legend = TRUE) + 
  labs(x= "Distance (in km)", title = "Most Inclusive") + theme_bw() +
  scale_color_discrete(name = "Acculturation",
                       labels = c("Lowest", "Highest"))  + 
  # facet_wrap(~ "border security recoded") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) 
mod3_2 <- plot_model(excl_runs_sec_nas$border_security_recoded[[3]], type = "int") + 
  labs(x = "Distance (in km)", y = element_blank(), title = "Least Inclusive")
mod3_2leg <- mod3_2 + theme_bw() + scale_color_discrete(name = "Acculturation",
                                                     labels = c("Lowest", "Highest")) + 
  # facet_wrap(~ "border security recoded") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme(strip.text.x = element_blank())
mod2_2noleg <- mod2_2 + theme(legend.position = "none", 
                           strip.text.x = element_blank())

fig2_combo <- mod2_2noleg + mod3_2leg

ggsave("fig2_combo.pdf", width = 7, height = 4)

mod3 <- plot_model(incl_runs_sec$border_security_recoded[[2]], type = "pred", 
                   terms = "psych_dist_lang") + 
  labs(title= "Most Inclusive", x = "Acculturation",
       y = "Make Border Security a National Priority") + theme_sjplot() + 
  theme(plot.title = element_text(hjust = 0.5))
mod6 <- plot_model(excl_runs_sec$border_security_recoded[[2]], type = "pred", 
                   terms = "psych_dist_lang") + 
  labs(title = "Least Inclusive", x = "Acculturation", y = NULL) + 
  theme_sjplot() + 
  theme(plot.title = element_text(hjust = 0.5))
fig2_combo <- mod3 + mod6
wrap_elements(fig2_combo) + 
  labs(title = "Predicted Probabilities of Attitudes on Border Security as a National Priority") + 
  theme(plot.title = element_text(hjust = 0.5))

### Table 1 (No NAs) Plot --------------
mod2_n <- plot_model(incl_runs_nas$Increase_Border_Spending[[3]], type = "int",
                   show.legend = TRUE) + 
  labs(x= "Distance (in km)", title = "Most Inclusive") + theme_bw() +
  scale_color_discrete(name = "Acculturation",
                       labels = c("Lowest", "Highest"))  + 
  facet_wrap(~ "Increase Border Spending") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) 
mod3_n <- plot_model(excl_runs_nas$Increase_Border_Spending[[3]], type = "int") + 
  labs(x = "Distance (in km)", y = element_blank(), title = "Least Inclusive")
mod3_leg_n <- mod3_n + theme_bw() + scale_color_discrete(name = "Acculturation",
                                                     labels = c("Lowest", "Highest")) + 
  facet_wrap(~ "Increase Border Spending") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme(strip.text.x = element_blank())
mod2_noleg_n <- mod2_n + theme(legend.position = "none", 
                           strip.text.x = element_blank())

fig1_combo_n <- mod2_noleg_n + mod3_leg_n

wrap_elements(fig1_combo_n) + 
  labs(title = "Predicted Probabilities of Interaction Model on Border Spending") + 
  theme(plot.title = element_text(hjust = 0.5))


### Distribution Plots ---------------
### Border Distribution ---------
inc_borderwall <- incl$variables %>% ggplot(aes(Increase_Border_Spending,)) +
  geom_histogram(bins = 3,fill = "dodgerblue") + 
  stat_bin(aes(label = ifelse(after_stat(count) > 0, scales::percent(after_stat(count)/sum(after_stat(count))), "")), 
           geom = "text", 
           vjust = -0.5, 
           size = 3, 
           color = "black") + 
  labs(title = "Most Inclusive", y = "Count", x = NULL) + theme_bw() +
  scale_x_continuous(labels = c("","Oppose", "", "Support", "")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,700)) 

exc_borderwall <- excl$variables %>% ggplot(aes(Increase_Border_Spending)) +
  geom_histogram(bins = 3, fill = "darkblue") + 
  stat_bin(aes(label = ifelse(after_stat(count) > 0, scales::percent(after_stat(count)/sum(after_stat(count))), "")), 
           geom = "text", 
           vjust = -0.5, 
           size = 3, 
           color = "black") +
  labs(title = "Least Inclusive", y = NULL, x = NULL) + theme_bw() +
  scale_x_continuous(labels = c("","Oppose", "", "Support", "")) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(limits = c(0,700)) 
# together 

both_bordr <- inc_borderwall + exc_borderwall + plot_layout(guides = "collect")
ggsave("both_border.pdf", width = 7, height = 4)

# wrap_elements(panel = both_bordr) +
#   labs(tag = "Attitudes Towards Border Spending, Including a Wall") +
#   theme(
#     plot.tag = element_text(size = rel(1.6)),
#     plot.tag.position = "top"
#   )

### National Sec Distribution ---------



i_bordersec <- incl$variables %>% ggplot(aes(border_security_recoded)) +
  geom_histogram(bins = 9, fill = "dodgerblue") + 
  stat_bin(aes(label = ifelse(after_stat(count) > 0, scales::percent(after_stat(count)/sum(after_stat(count))), "")), 
           geom = "text", 
           vjust = -0.5, 
           size = 3, 
           color = "black") + theme_bw() + 
  labs(title = "Most Inclusive", y = "Count", x = NULL) +
  scale_x_continuous(labels = c("", "(1) Oppose","2", "3", "4","(5) Support")) +
 theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(limits = c(0,275)) 


e_bordersec <- excl$variables %>% ggplot(aes(border_security_recoded)) +
  geom_histogram(bins = 9, fill = "darkblue") + 
  stat_bin(aes(label = ifelse(after_stat(count) > 0, scales::percent(after_stat(count)/sum(after_stat(count))), "")), 
           geom = "text", 
           vjust = -0.5, 
           size = 3, 
           color = "black") + theme_bw() + 
  labs(title = "Least Inclusive", y = NULL, x = NULL) +
  scale_x_continuous(labels = c("", "(1) Oppose","2", "3", "4","(5) Support")) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(limits = c(0,275)) 
# both

both_sec <- i_bordersec + e_bordersec + plot_layout(guides = "collect")

ggsave("both_border_sec.pdf", width = 7, height = 4)

### Psychological Distance Distribution -----------


i_psych <- incl$variables %>% ggplot(aes(psych_dist_lang)) +
  geom_histogram(bins = 23, fill = "dodgerblue") +  theme_bw() + 
  labs(title = "Most Inclusive", y = "Count", x = NULL) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(limits = c(0,60)) 
# +
#   scale_x_continuous(labels = c("", "(1) Weakest","2", "3", "4","(5) Strongest"))

e_psych <- excl$variables %>% ggplot(aes(psych_dist_lang)) +
  geom_histogram(bins = 25, fill = "darkblue") + theme_bw() + 
  labs(title = "Least Inclusive", y = NULL, x = NULL) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(limits = c(0,60)) 
#+
#   scale_x_continuous(labels = c("", "(1) Weakest","2", "3", "4","(5) Strongest"))
# both

both_psych <- i_psych + e_psych + plot_layout(guides = "collect")

ggsave("both_psych.pdf", width = 7, height = 4)

### Correlation Matrix --------------
cor_data <- full_cmps_lat %>% select(Spanish, Imm_Comm, Parents_Born, 
                                    Grandparents_Born, Psych_Distance, 
                                    distance_km, Remit, family_birth, Inclusive,
                                    border_state) 
cors <- cor(cor_data, use = "pairwise.complete.obs") %>% as.data.frame()
xtable(cors)
#### Summary Stats -------
sum_data_incl <- incl %>% select(border_security_recoded,
                                    distance_km, dist_sqd, 
                                    psych_dist_lang, Age, age_sqd) 
sum_data_incl$dist_logged <- log(sum_data_incl$distance_km)
stargazer(as.data.frame(sum_data_incl), type = "latex",
          covariate.labels = c("Border Security", "Distance (in km)",
                               "Distance (Sqd)",
                               "Acculturation",
                               "Age",
                               "Age Sqd"), dep.var.labels = "Summary Statistics")

sum_data_excl <- excl %>% select(border_security_recoded,
                                 distance_km, dist_sqd, 
                                 psych_dist_lang, Age, age_sqd) 
sum_data_excl$dist_logged <- log(sum_data_excl$distance_km)
stargazer(as.data.frame(sum_data_excl), type = "latex",
          covariate.labels = c("Border Security", "Distance (in km)",
                               "Distance (Sqd)",
                               "Acculturation",
                               "Age",
                               "Age Sqd", "Distance (Logged)"), 
          dep.var.labels = "Summary Statistics")


#### PCA Analysis of Acculturation ----

reg_incl <-  lm(psych_dist_lang ~ Spanish + Imm_Comm + Parents_Born + 
             Grandparents_Born, data = incl)
X_incl <-  model.matrix(reg_incl)
X_incl |> head()

reg_excl <-  lm(psych_dist_lang ~ Spanish + Imm_Comm + Parents_Born + 
             Grandparents_Born, data = excl)
X_excl <-  model.matrix(reg_excl)
X_excl |> head()

pc_incl <- stats::prcomp(X_incl)
summary(pc_incl)

pc_excl <- stats::prcomp(X_excl)
summary(pc_excl)

screeplot(pc_incl, main = "Acculturation PCA in Inclusive Subset")
screeplot(pc_excl, main = "Acculturation PCA in Exclusive Subset")

# combined
reg <-  lm(psych_dist_lang ~ Spanish + Imm_Comm + Parents_Born + 
                  Grandparents_Born, data = cleaned)
X <-  model.matrix(reg)
X |> head()

pc <- stats::prcomp(X)
summary(pc)
screeplot(pc, main = "Acculturation PCA for Full Sample")

### Factor Analysis ---------------
acc <- full_cmps_lat %>% dplyr::select(Spanish, Imm_Comm, Parents_Born, 
                                      Grandparents_Born)
KMO(acc)
bart_spher(acc, use = 'complete.obs')

# scree plot
png("screeplot.png")
screeplot <- scree(acc)
dev.off()


fa_1 <- fa(acc, nfactors = 1, rotate = 'oblimin') # 1
ggsave("screeplot.png", screeplot, width = 7, height = 4) 

fa_1[["Vaccounted"]] %>%
  as.data.frame() %>%
  #select(1:5) %>% Use this if you have many factors and only want to show a certain number
  rownames_to_column("Property") %>%
  mutate(across(where(is.numeric), round, 3)) %>% xtable()
add_info <- cbind(fa_1$communalities, 
                  fa_1$uniquenesses,
                  fa_1$complexity) %>%
  # make it a data frame
  as.data.frame() %>%
  # column names
  rename("Communality" = V1,
         "Uniqueness" = V2,
         "Complexity" = V3) %>%
  #get the item names from the vector
  rownames_to_column("item")

fa.sort(fa_1)$loadings %>% unclass() %>%
  as.data.frame() %>%
  rownames_to_column("item") %>%
  left_join(add_info) %>%
  mutate(across(where(is.numeric), round, 3)) %>% xtable()
#### Missingness --------
sum(is.na(cleaned$Grandparents_Born))
sum(is.na(cleaned$Parents_Born))
sum(is.na(full_cmps_lat$Spanish))
sum(is.na(full_cmps_lat$Imm_Comm))

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


mod_test <- svyglm(Increase_Border_Spending ~ log_dist + Age + Education + 
                     Income + 
                     Republican, design = incl,
                   family = "binomial")
mod_test2 <- svyglm(Increase_Border_Spending ~ log_dist + Age + Education + 
                      Income + 
                      Republican + Remittances_Index, design = incl,
                    family = "binomial")

mod_test_remit <- svyglm(Remittances_Scale ~ log_dist + Age + Education + 
                           Income + Republican, design = new_cmps)



#### Power Analysis --------- 
subset_three <- cleaned_states %>% select(Inclusive, family_birth, distance_km,
                                          Increase_Border_Spending)
correlations_three <- cor(subset_three, use = "pairwise.complete.obs")

power.results <-  power_interaction_3way_r2(N = 2016,         # Sample size
                                          b.x1x2x3 = .00001523,  # Interaction regression coefficient
                                          r.x1.y = 0.006857641,     # Main effects
                                          r.x2.y = -0.07169315,
                                          r.x3.y = -0.007645255,
                                          r.x1x2.y = -0.00001629,  # 2-way interactions
                                          r.x1x3.y = -0.0001846,
                                          r.x2x3.y = -0.001437,
                                          r.x1.x2 = -0.010605472,    # Correlation between main effects
                                          r.x1.x3 = 0.082992450,
                                          r.x2.x3 = 0.02129139)

excl_cors <- excl$variables %>% select(family_birth, distance_km,
                                       Increase_Border_Spending)
excl_cors$int <- excl_cors$family_birth*excl_cors$distance_km
correlations_excl <- cor(excl_cors, use = "pairwise.complete.obs")

### odds ratios 
odds_ratios_full <- exp(coef(full_sample_bin$Increase_Border_Spending[[6]])) %>% as.data.frame()
odds_ratios_excl <- exp(coef(excl_runs_nas$Increase_Border_Spending[[4]])) %>% as.data.frame()

### power -- interaction

r_1 <- psrsq(excl_runs_nas$Increase_Border_Spending[[4]], method = c("Cox-Snell","Nagelkerke"))
r_2 <- psrsq(excl_runs_nas$Increase_Border_Spending[[3]], method = c("Cox-Snell","Nagelkerke"))

f2 <- (r_1 - r_2)/(1-r_1)

pwr.f2.test(u = 9, v = 1034, f2 = f2, sig.level = 0.05)

### power - 3-way interaction
r_3_1 <- psrsq(full_sample_ols$Increase_Border_Spending[[6]], method = c("Cox-Snell","Nagelkerke"))
r_3_2 <- psrsq(full_sample_ols$Increase_Border_Spending[[5]], method = c("Cox-Snell","Nagelkerke"))

f2_3 <- (r_3_1 - r_3_2)/(1-r_3_1)

pwr.f2.test(u = 13, v = 1983, f2 = f2_3, sig.level = 0.05)

### logistic power analysis --- 
power_interaction(n.iter = 100, N = 953, r.x1.y = -0.09184342, 
                  r.x2.y = -0.06706666, r.x1x2.y = -0.1185832, 
                  r.x1.x2 = 0.11963341, rel.x1 = 1, rel.x2 = 1, rel.y = 1)

#### Coefficent Plots for Slides-------
## split to allow for better visualization
# coef_names_control <- c(Income = "Income", age_sqd = "Age", Education = "Education", 
#                 Party_5pt = "Party", linked = "Linked Fate",
#                 missing_birth = "Missing Acculturation", 
#                 family_birth = "Acculturation", distance_km =
#                 "Distance (in km)")
# full_controls <- coefplot(full_sample_bin$Increase_Border_Spending[[4]], intercept = FALSE, 
#          coefficients = c("age_sqd", "Income", "Education", "Party_5pt", 
#                           "linked", "missing_birth", "family_birth", 
#                           "distance_km"),
#          newNames = coef_names_control)
# ggsave("full_controls.pdf", width = 7, height = 4)

### distance H1a & H1b 
# coef_names_dist <- c(distance_km = "Distance (Rescaled)")
# full_dist_spend <- coefplot(full_sample_bin_re$Increase_Border_Spending[[1]], intercept = FALSE,
#                       coefficients = "distance_km", lwdInner = .5, lwdOuter = .1,
#          title = "Increase Border Spending", newNames = coef_names_dist) 
# full_dist_sec <- coefplot(full_sample_ols_re$border_security_recoded[[1]], intercept = FALSE,
#          coefficients = "distance_km", lwdInner = .5, lwdOuter = .1,
#          title = "Make Border Security a National Priority", newNames = coef_names_dist) 
# 
# full_dist_spend <- full_dist_spend + scale_x_continuous(limits = c(-.8, .5))
# full_dist_sec <- full_dist_sec + scale_x_continuous(limits = c(-.8, .5))
# 
# combined_full_dist <- full_dist_spend + full_dist_sec
# ggsave("combined_full_dist.pdf", width = 8, height = 4)

#interactions plotted 

mod.labs <- c("Increase Border Spending", "Border Security a National Priority")

# Create a named vector to map old labels to new labels
label_mapping <- setNames(mod.labs, levels(full_sample_bin_re$Increase_Border_Spending[[4]]$Model))

coef_names_full_int <- c("distance_km:family_birth" = "Distance (in km) x Acculturation",
                         "distance_km:Inclusive" = "Distance (in km) x Inclusive",
                         "family_birth:Inclusive" = "Acculturation x Inclusive",
                         "distance_km:family_birth:Inclusive" = "Distance (in km) x Acculturation x Inclusive")


combined <- multiplot(full_sample_bin_re$Increase_Border_Spending[[4]], 
          full_sample_ols_re$border_security_recoded[[4]],
          intercept = FALSE, 
          coefficients = c("distance_km:family_birth", 
                           "family_birth:Inclusive",
                           "distance_km:family_birth:Inclusive"),
          lwdInner = .5, lwdOuter = .2,
          newNames = coef_names_full_int, single = FALSE, legend.position = "none")

combined_int <- combined + facet_wrap(~Model, labeller = as_labeller(setNames(mod.labs, sort(unique(combined$data$Model)))))
          

ggsave("combined_int.pdf", width = 9, height = 4)

# Missing Interaction 

coef_names_miss_int <- c("distance_km:Inclusive" = "Distance (in km) x Inclusive")


combined_miss <- multiplot(full_sample_bin_re$Increase_Border_Spending[[4]], 
                      full_sample_ols_re$border_security_recoded[[4]],
                      intercept = FALSE, 
                      coefficients = c("distance_km:Inclusive"),
                      lwdInner = .5, lwdOuter = .2,
                      newNames = coef_names_full_int, single = FALSE, legend.position = "none")

combined_miss_int <- combined_miss + facet_wrap(~Model, labeller = as_labeller(setNames(mod.labs, sort(unique(combined$data$Model)))))
ggsave("combined_miss_int.pdf", width = 9, height = 4)


### Inclusive versus Exclusive 
coef_names_int <- c("distance_km:family_birth" = "Distance (in km) x Acculturation")

# incl_controls <- coefplot(incl_runs_re$Increase_Border_Spending[[3]], intercept = FALSE, 
#                           coefficients = c("age_sqd", "Income", "Education", "Party_5pt", 
#                                            "linked", "missing_birth", "family_birth", 
#                                            "distance_km"),
#                           newNames = coef_names_control, "Most Inclusive")
# 
# excl_controls <- coefplot(excl_runs_re$Increase_Border_Spending[[3]], intercept = FALSE, 
#                           coefficients = c("age_sqd", "Income", "Education", "Party_5pt", 
#                                            "linked", "missing_birth", "family_birth", 
#                                            "distance_km"),
#                           newNames = coef_names_control, title = "Least Inclusive")
# 
# combined_coefs <- incl_controls + excl_controls
# ggsave("combined_coefs.pdf", width = 8, height = 4)
# 
# excl_int <- coefplot(excl_runs_re$Increase_Border_Spending[[3]], intercept = FALSE, 
#                          coefficients = c("distance_km:family_birth"),
#                      lwdInner = .5, lwdOuter = .1,
#                          newNames = coef_names_int, "Least Inclusive") + 
#   scale_x_continuous(limits = c(-.035, 0.01))
# 
# incl_int <- coefplot(incl_runs_re$Increase_Border_Spending[[3]], intercept = FALSE, 
#                      coefficients = c("distance_km:family_birth"),
#                      lwdInner = .5, lwdOuter = .1,
#                      newNames = coef_names_int, title = "Most Inclusive") + 
#   scale_x_continuous(limits = c(-.035, 0.01))
# 
# combined_int <- incl_int + excl_int
# ggsave("combined_int_bin.pdf", width = 8, height = 4)

mod_context <- c("Least Inclusive", "Most Inclusive")
context_int <- multiplot(incl_runs_re$Increase_Border_Spending[[3]], 
                           excl_runs_re$Increase_Border_Spending[[3]],
                           intercept = FALSE, 
                           coefficients = c("distance_km:family_birth"),
                           lwdInner = .5, lwdOuter = .2,
                           newNames = coef_names_int, single = FALSE, 
                         legend.position = "none")

mulcontext_com_int <- context_int + facet_wrap(~Model, 
                                                labeller = as_labeller(setNames(mod_context, 
                                                                                sort(unique(context_int$data$Model)))))
ggsave("context_com_int.pdf", width = 9, height = 4)
# ### border security 
# 
# incl_controls_sec <- coefplot(excl_runs_sec$border_security_recoded[[3]], intercept = FALSE, 
#                           coefficients = c("age_sqd", "Income", "Education", "Party_5pt", 
#                                            "linked", "missing_birth", "family_birth", 
#                                            "distance_km"),
#                           newNames = coef_names_control, "Most Inclusive")
# 
# excl_controls_sec <- coefplot(excl_runs_sec$border_security_recoded[[3]], intercept = FALSE, 
#                           coefficients = c("age_sqd", "Income", "Education", "Party_5pt", 
#                                            "linked", "missing_birth", "family_birth", 
#                                            "distance_km"),
#                           newNames = coef_names_control, title = "Least Inclusive")
# 
# combined_coefs_sec <- incl_controls_sec + excl_controls_sec
# ggsave("combined_coefs_sec.pdf", width = 7, height = 4)
context_int_sec <- multiplot(incl_runs_sec_re$border_security_recoded[[3]], 
                         excl_runs_sec_re$border_security_recoded[[3]],
                         intercept = FALSE, 
                         coefficients = c("distance_km:family_birth"),
                         lwdInner = .5, lwdOuter = .2,
                         newNames = coef_names_full_int, single = FALSE, legend.position = "none")

context_com_int_sec <- context_int_sec + facet_wrap(~Model, 
                                               labeller = as_labeller(setNames(mod_context, 
                                                                               sort(unique(context_int_sec$data$Model)))))
ggsave("context_com_int_sec.pdf", width = 9, height = 4)


# excl_int_sec <- coefplot(excl_runs_sec_re$border_security_recoded[[3]], intercept = FALSE, 
#                      coefficients = c("distance_km:family_birth"),
#                      lwdInner = .5, lwdOuter = .1,
#                      newNames = coef_names_int, "Least Inclusive") + 
#   scale_x_continuous(limits = c(-.02, .02))
# 
# incl_int_sec <- coefplot(incl_runs_sec_re$border_security_recoded[[3]], intercept = FALSE, 
#                      coefficients = c("distance_km:family_birth"),
#                      lwdInner = .5, lwdOuter = .1,
#                      newNames = coef_names_int, title = "Most Inclusive") + 
#   scale_x_continuous(limits = c(-.02, .02))
# 
# combined_int_sec <-  incl_int_sec + excl_int_sec 
# ggsave("combined_int_sec.pdf", width = 8, height = 4)

#### DISTANCE alone 

context_dist <- multiplot(incl_runs_re$Increase_Border_Spending[[1]], 
                         excl_runs_re$Increase_Border_Spending[[1]],
                         intercept = FALSE, 
                         coefficients = c("distance_km"),
                         lwdInner = .5, lwdOuter = .2,
                         newNames = coef_names_dist, single = FALSE, legend.position = "none")

context_com_dist <- context_dist + facet_wrap(~Model, 
                                               labeller = as_labeller(setNames(mod_context, 
                                                                               sort(unique(context_dist$data$Model)))))
ggsave("context_com_dist.pdf", width = 9, height = 4)
# ## spending 
# excl_dist_spend <- coefplot(excl_runs_re$Increase_Border_Spending[[1]], intercept = FALSE, 
#                           coefficients = c("distance_km"),
#                           lwdInner = .5, lwdOuter = .1,
#                           newNames = coef_names_dist, "Least Inclusive") +
#   scale_x_continuous(limits = c(-2.5, 2))
# 
# incl_dist_spend <- coefplot(incl_runs_re$Increase_Border_Spending[[1]], intercept = FALSE, 
#                           coefficients = c("distance_km"),
#                           lwdInner = .5, lwdOuter = .1,
#                           newNames = coef_names_dist, title = "Most Inclusive") +
#   scale_x_continuous(limits = c(-2.5, 2))
# 
# combined_dist_spend <-  incl_dist_spend + excl_dist_spend 
# ggsave("combined_dist_spend.pdf", width = 7, height = 4)
## security 

context_dist_sec <- multiplot(incl_runs_sec_re$border_security_recoded[[1]],
                          excl_runs_sec_re$border_security_recoded[[1]],
                          intercept = FALSE, 
                          coefficients = c("distance_km"),
                          lwdInner = .5, lwdOuter = .2,
                          newNames = coef_names_dist, single = FALSE, legend.position = "none")

context_com_dist_sec <- context_dist_sec + facet_wrap(~Model, 
                                              labeller = as_labeller(setNames(mod_context, 
                                                                              sort(unique(context_dist_sec$data$Model)))))
ggsave("context_com_dist_sec.pdf", width = 9, height = 4)
# excl_dist_sec <- coefplot(excl_runs_sec_re$border_security_recoded[[1]], intercept = FALSE, 
#                          coefficients = c("distance_km"),
#                          lwdInner = .5, lwdOuter = .3,
#                          newNames = coef_names_dist, title = "Least Inclusive") + 
#    scale_x_continuous(limits = c(-1.5, .5))
# 
# incl_dist_sec <- coefplot(incl_runs_sec_re$border_security_recoded[[1]], intercept = FALSE, 
#                          coefficients = c("distance_km"),
#                          lwdInner = .5, lwdOuter = .1,
#                          newNames = coef_names_dist, title = "Most Inclusive")+
#   scale_x_continuous(limits = c(-1.5, .5))
# 
# combined_dist_sec <-  incl_dist_sec + excl_dist_sec 
# ggsave("combined_dist_sec.pdf", width = 8, height = 4)


### ACCULTURATION ALONE
coef_acc <- c("family_birth" = "Acculturation")


context_acc <- multiplot(incl_runs_re$Increase_Border_Spending[[2]],
                              excl_runs_re$Increase_Border_Spending[[2]],
                              intercept = FALSE, 
                              coefficients = c("family_birth"),
                              lwdInner = .5, lwdOuter = .2,
                              newNames = coef_acc, single = FALSE, legend.position = "none")

context_com_acc <- context_acc + facet_wrap(~Model, 
                                                      labeller = as_labeller(setNames(mod_context, 
                                                                                      sort(unique(context_acc$data$Model)))))
ggsave("context_com_acc.pdf", width = 9, height = 4)
# 
# excl_acc_spend <- coefplot(excl_runs_re$Increase_Border_Spending[[2]], intercept = FALSE, 
#                             coefficients = c("family_birth"),
#                             lwdInner = .5, lwdOuter = .1,
#                             newNames = coef_acc, "Least Inclusive") +
#   scale_x_continuous(limits = c(-.01, .01))
# 
# incl_acc_spend <- coefplot(incl_runs_re$Increase_Border_Spending[[2]], intercept = FALSE, 
#                             coefficients = c("family_birth"),
#                             lwdInner = .5, lwdOuter = .1,
#                             newNames = coef_acc, title = "Most Inclusive") +
#   scale_x_continuous(limits = c(-.01, .01))
# 
# combined_acc_spend <-  incl_acc_spend + excl_acc_spend 
# ggsave("combined_acc_spend.pdf", width = 8, height = 4)
## security 

context_acc_sec <- multiplot(incl_runs_sec_re$border_security_recoded[[2]],
                         excl_runs_sec_re$border_security_recoded[[2]],
                         intercept = FALSE, 
                         coefficients = c("family_birth"),
                         lwdInner = .5, lwdOuter = .2,
                         newNames = coef_acc, single = FALSE, legend.position = "none")

context_com_acc_sec <- context_acc_sec + facet_wrap(~Model, 
                                            labeller = as_labeller(setNames(mod_context, 
                                                                            sort(unique(context_acc_sec$data$Model)))))
ggsave("context_com_acc_sec.pdf", width = 9, height = 4)
# 
# excl_acc_sec <- coefplot(excl_runs_sec_re$border_security_recoded[[2]], intercept = FALSE, 
#                           coefficients = c("family_birth"),
#                           lwdInner = .5, lwdOuter = .1,
#                           newNames = coef_acc, "Least Inclusive") + 
#   scale_x_continuous(limits = c(-.01, .01))
# 
# incl_acc_sec <- coefplot(incl_runs_sec_re$border_security_recoded[[2]], intercept = FALSE, 
#                           coefficients = c("family_birth"),
#                           lwdInner = .5, lwdOuter = .1,
#                           newNames = coef_acc, title = "Most Inclusive") +
#   scale_x_continuous(limits = c(-.01, .01))
# 
# combined_acc_sec <-  incl_acc_sec + excl_acc_sec 
# ggsave("combined_acc_sec.pdf", width = 8, height = 4)
# 

