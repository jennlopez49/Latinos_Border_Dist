## visualizations of main IVs and DVs

labels <- c("Oppose", "Support")

hist_borderspend <- full_cmps_lat %>% ggplot(aes(Increase_Border_Spending)) + geom_histogram(bins = 3) + 
  xlab("Increase Border Spending") + ylab("Count") + scale_x_continuous(labels = c("","Oppose", "", "Support", ""))


hist_border <- full_cmps_lat %>% ggplot(aes(border_sec_first)) + geom_histogram() + 
  xlab("Increase Border Security as a National Priority") + ylab("Count") 


hist_identity <- full_cmps_lat %>% ggplot(aes(identity_strength_recoded)) + geom_histogram() + 
  xlab("Identity Strength") + ylab("Count")

hist_linked_fate <- full_cmps_lat %>% ggplot(aes(Linked_Fate)) + 
  geom_histogram(bins = 15, binwidth = .5) + 
  xlab("Linked Fate") + ylab("Count")


hist_psych <- full_cmps_lat %>% ggplot(aes(Psych_Distance)) + 
  geom_histogram() + 
  xlab("Psychological Distance") + ylab("Count")



###### descriptive table 

max(full_cmps_lat$distance_km, na.rm = TRUE)


##### DEMS VS REPS


labels <- c("Oppose", "Support")

d_borderwall <- dem %>% ggplot(aes(Increase_Border_Spending,)) +
  geom_histogram(bins = 3,fill = "blue") + 
  xlab("Most Inclusive") + ylab("Count") + 
  scale_x_continuous(labels = c("","Oppose", "", "Support", ""))

r_borderwall <- rep %>% ggplot(aes(Increase_Border_Spending)) +
  geom_histogram(bins = 3, fill = "red") + xlab("Least Inclusive") + ylab(NULL) + 
  scale_x_continuous(labels = c("","Oppose", "", "Support", ""))
# together 

both_bordr <- d_borderwall + r_borderwall + plot_layout(guides = "collect")

wrap_elements(panel = both_bordr) +
  labs(tag = "Attitudes Towards Border Spending, Including a Wall") +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )


hist_border <- full_cmps_lat %>% ggplot(aes(border_sec_first)) + geom_histogram() + 
  xlab("Increase Border Security as a National Priority") + ylab("Count") 



d_bordersec <- dem %>% ggplot(aes(border_security_recoded)) +
  geom_histogram(bins = 9, fill = "blue") + 
  xlab("Most Inclusive") + ylab("Count") +
  scale_x_continuous(labels = c("", "(1) Oppose","2", "3", "4","(5) Support"))

r_bordersec <- rep %>% ggplot(aes(border_security_recoded)) +
  geom_histogram(bins = 9, fill = "red") + xlab("Least") + ylab(NULL) +
  scale_x_continuous(labels = c("", "(1) Oppose","2", "3", "4","(5) Support"))
# both

both_sec <- d_bordersec + r_bordersec + plot_layout(guides = "collect")

wrap_elements(panel = both_sec) +
  labs(tag = "Attitudes towards Making Border Security a National Priority") +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )



d_linked <- dem %>% ggplot(aes(linked)) +
  geom_histogram(bins = 9, fill = "blue") + 
  xlab("Most Inclusive") + ylab("Count") +
  scale_x_continuous(labels = c("", "(1) Least","2", "3", "4","(5) Most"))

r_linked <- rep %>% ggplot(aes(linked)) +
  geom_histogram(bins = 9, fill = "red") + xlab("Least Inclusive") + ylab(NULL) +
  scale_x_continuous(labels = c("", "(1) Least","2", "3", "4","(5) Most"))
# both

both_linked <- d_linked + r_linked + plot_layout(guides = "collect")

wrap_elements(panel = both_linked) +
  labs(tag = "Linked Fate") +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )


d_id <- dem %>% ggplot(aes(identity_strength_recoded)) +
  geom_histogram(bins = 9, fill = "blue") + 
  xlab("Most Inclusive") + ylab("Count") +
  scale_x_continuous(labels = c("", "(1) Weakest","2", "3", "4","(5) Strongest"))

r_id <- rep %>% ggplot(aes(identity_strength_recoded)) +
  geom_histogram(bins = 9, fill = "red") + xlab("Least Inclusive") + ylab(NULL) +
  scale_x_continuous(labels = c("", "(1) Weakest","2", "3", "4","(5) Strongest"))
# both

both_id <- d_id + r_id + plot_layout(guides = "collect")

wrap_elements(panel = both_id) +
  labs(tag = "Identity Strength") +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )


d_psych <- dem %>% ggplot(aes(psych_dist_lang)) +
  geom_histogram(bins = 23, fill = "blue") + 
  xlab("Most Inclusive") + ylab("Count") 
# +
#   scale_x_continuous(labels = c("", "(1) Weakest","2", "3", "4","(5) Strongest"))

r_psych <- rep %>% ggplot(aes(psych_dist_lang)) +
  geom_histogram(bins = 25, fill = "red") + xlab("Least Inclusive") + ylab(NULL) 
#+
#   scale_x_continuous(labels = c("", "(1) Weakest","2", "3", "4","(5) Strongest"))
# both

both_psych <- d_psych + r_psych + plot_layout(guides = "collect")

wrap_elements(panel = both_psych) +
  labs(tag = "Psychological Distance (Distance to the Migration Experience)") +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )



#### REGRESSIONS -------
## regression # 6 
lf_intPlot <- plot_model(rep_runs$Increase_Border_Spending[[6]], type = "int", 
                       xlabel = "Linked Fate", title = "",
                       legend.title = "Psychological Distance")
lf_int_demPlot <- plot_model(dem_runs$Increase_Border_Spending[[6]], type = "int", 
                        axis.title = "", title = "", axis.labels = FALSE,
                         legend.title = "Psychological Distance")

lf_intPlot + lf_int_demPlot + plot_layout(guides = "collect")

plot_models(list(dem_runs$Increase_Border_Spending[[6]],
                 rep_runs$Increase_Border_Spending[[6]]),
            rm.terms = c("Education", "Age", "Party_5pt"),
            axis.labels = c("Linked Fate:Psychological Distance",
                            "Psychological Distance","Linked Fate", 
                            "Distance (in km)"), legend.title = "Models",
            m.labels = c("Most Inclusive", "Least Inclusive"))


### 8 

id_intPlot <- plot_model(rep_runs$Increase_Border_Spending[[5]], type = "int", 
                         xlabel = "Linked Fate", title = "",
                         axis.title = "Support for Border Spending, Including Wall",
                         show.legend = FALSE)
id_int_demPlot <- plot_model(dem_runs$Increase_Border_Spending[[5]], type = "int", 
                             axis.title = "",
                             xlabel = "",
                              title = "", axis.labels = FALSE,
                             legend.title = "Linked Fate")
linked_int_rep <- id_intPlot + labs(x = "Least Inclusive")
linked_int_dem <- id_int_demPlot + labs(x = "Most Inclusive")
both <- wrap_elements(linked_int_rep) + wrap_elements(linked_int_dem) + 
  plot_layout(guides = "collect")
both + labs(caption = "Psychological Distance") +
  theme(plot.caption = element_text(hjust=0, size=rel(1.2)))

### Model 3 
intPlot <- plot_model(rep_runs_sec$border_security_recoded[[3]], type = "int", 
                         xlabel = "Linked Fate", title = "",
                         axis.title = "Support for Making Border Security a National Priority",
                         show.legend = FALSE)
int_demPlot <- plot_model(dem_runs_sec$border_security_recoded[[3]], type = "int", 
                             axis.title = "",
                             xlabel = "",
                             title = "", axis.labels = FALSE,
                             legend.title = "Linked Fate")
int_rep <- intPlot + labs(x = "Least Inclusive")
int_dem <- int_demPlot + labs(x = "Most Inclusive")
both <- wrap_elements(int_rep) + wrap_elements(int_dem) + 
  plot_layout(guides = "collect")
both + labs(caption = "Distance (in km)") +
  theme(plot.caption = element_text(hjust=0, size=rel(1.2)))
