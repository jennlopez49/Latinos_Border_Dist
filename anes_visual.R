# Visualizations of Primary IVs 

ggplot(latinos_16, aes(x = Migration_Dist)) + geom_histogram() + ylab("Count") +
  xlab("Migration Distance (Multiplicative)")


ggplot(latinos_16, aes(x = Migration_Dist_Factor)) + geom_histogram() + ylab("Count") +
  xlab("Migration Distance (Multiplicative) Collapsed")

ggplot(latinos_16, aes(x = Migration_Dist_Add)) + geom_histogram() + ylab("Count") +
  xlab("Migration Distance (Additive)")


## Differences in Grandparents

ggplot(latinos_16, aes(x = g_born)) + geom_histogram() + ylab("Count") +
  xlab("Grandparents In of the US")

ggplot(latinos_16, aes(x = Grandparents_Short)) + geom_histogram() + ylab("Count") +
  xlab("Grandparents Outside of the US")


### Visualizations of DV 

ggplot(latinos_16, aes(x = Border_Reordered)) + geom_histogram() + ylab("Count") +
  xlab("Attitude Towards Border Wall")
