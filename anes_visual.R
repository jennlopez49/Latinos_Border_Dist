# Visualizations of Primary IVs 

ggplot(latinos_16, aes(x = Migration_Dist)) + geom_histogram() + ylab("Count") +
  xlab("Migration Distance (Multiplicative)")


ggplot(latinos_16, aes(x = Migration_Dist_Factor)) + geom_histogram() + ylab("Count") +
  xlab("Migration Distance (Multiplicative) Collapsed")

ggplot(latinos_16, aes(x = Migration_Dist_Add)) + geom_histogram() + ylab("Count") +
  xlab("Distance to Migration Experience, 2016")

ggplot(latinos_16, aes(x = Linked_Fate)) + geom_histogram() + ylab("Count") +
  xlab("Linked Fate")

ggplot(latinos_16, aes(x = Latino_Identity)) + geom_histogram() + ylab("Count") +
  +   xlab("Latino Identity")

## Differences in Grandparents

ggplot(latinos_16, aes(x = g_born)) + geom_histogram() + ylab("Count") +
  xlab("Grandparents In of the US")

ggplot(latinos_16, aes(x = Grandparents_Short)) + geom_histogram() + ylab("Count") +
  xlab("Grandparents Outside of the US")


### Visualizations of DV 

ggplot(latinos_16, aes(x = Border_Reordered)) + geom_histogram() + ylab("Count") +
  xlab("Attitude Towards Border Wall")

coefplot(psych_ols, 
         title = "Coefficients for Attitudes towards Border Wall in 2016",
         intercept = FALSE) + 
         scale_y_discrete(labels = c("Age","Ideology", "Party","Education",
         "Economy in Past Year: Much better","Economy in Past Year: Much worse",
         "Economy in Past Year: Somewhat better","Economy in Past Year: Somewhat worse",
         "Attention to Politics: Always",
         "Attention to Politics: Most of the time","Attention to Politics: Never",
         "Attention to Politics: Some of the time",  "Hispanic Ancestry: Mexican",
         "Hispanic Ancestry: Other/More than one", "Hispanic Ancestry: Puerto Rican",
         "Migration Distance", "Latino Identity", 
         "Migration Distance|Latino Identity"))


