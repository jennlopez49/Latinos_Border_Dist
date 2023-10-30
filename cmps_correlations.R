## correlations 
cor_data <- full_cmps_lat %>% select(identity_strength, Linked_Fate, Psych_Distance)
cor_data_no_nas <- na.omit(cor_data)
cor(cor_data_no_nas)
