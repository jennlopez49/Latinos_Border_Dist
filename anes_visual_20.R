# Migration IV 
ggplot(latinos20_clean, aes(x = Migration_Dist)) + 
  geom_histogram() + ylab("Count") +
  xlab("Distance to Migration Experience")

# Latino Identity IVs

ggplot(latinos20, aes(x = Linked_Fate)) + geom_histogram() + ylab("Count") +
  xlab("Linked Fate") 
ggplot(latinos20, aes(x = Hispanic_Candidate)) + geom_histogram() + ylab("Count") +
  xlab("Importance of Hispanic Candidate") 
ggplot(latinos20, aes(x = UnfairLaws_Hisp)) + geom_histogram() + ylab("Count") +
  xlab("Importance in Fixing Unfair Laws for Hispanics") 

ggplot(latinos20, aes(x = Latino_Identity)) + geom_histogram() + ylab("Count") +
  xlab("Overall Scores for Latino Identity") 


ggplot(latinos20, aes(x = Lat_Identity)) + geom_histogram() + ylab("Count") +
  xlab("Overall Scores for Latino Identity without Unfair Laws") 
## Differences in Grandparents

ggplot(latinos20_clean, aes(x = Grandparents_Born)) + geom_histogram() + ylab("Count") +
  xlab("Grandparents Born in the US (5 - All, 1 - None)")



### Visualizations of DV 

ggplot(latinos20_clean, aes(x = Border_Wall)) + geom_histogram() + ylab("Count") +
  xlab("Attitude Towards Border Wall")
