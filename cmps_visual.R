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
