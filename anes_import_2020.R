### 2020 Import and Data Wrangling -------------
anes_20 <- read.csv("anes_20/anes_timeseries_2020_csv_20220210.csv")


# Selecting Variables 
# V020001 case id, V201600 Gender, V201005 pol interest, V201014b state of reg,
# V201200 lib-cons, V201231x party id, V201324 natl econ, V201325 retro natl econ,
# V201327x natl econ summ, V201507x - age, V201508 - marital, V201511x -edu,
# V201534x - employment, V201546 - hisp/lat, V201555 - grandparents born out of us,
# V201558x - hisp country ancestry, V201562 - language, V201575 - state where r grew up,
# V202498 - identity strength (restricted for now) , V201424x - building wall 



anes20 <- anes_20 %>% select(V200001, V201600, V201005, V201014b, V201200, 
                             V201231x, V201324, V201325, V201327x, V201507x,
                             V201508,  V201511x, V201534x, V201546, V201555,
                             V201553,
                             V201558x, V201562, V201575, 
                             # V202498, 
                             V201426x,
                             V200010a, V202479, V202220, V202485, V202504,
                             V201306, V201424, V202506, V200010b, V200010c, 
                             V200010d)
anes20 <- anes20 %>% mutate(
  Gender = case_when(V201600 == 1 ~ "Male",
                     V201600 == 2 ~ "Female"),
  Attention_Politics = case_when(V201005 == 1 ~ "Always",
                                 V201005 == 2 ~ "Most of the time",
                                 V201005 == 3 ~ "About half the time",
                                 V201005 == 4 ~ "Some of the time",
                                 V201005 == 5 ~ "Never"),
  # Border_State = case_when(V201014b == 6 ~ "Border State",
  #                          V201014b == 4 ~ "Border State",
  #                          V201014b == 48 ~ "Border State",
  #                          V201014b == 35 ~ "Border State",
  #                          .default = "Non_Border"),
  Ideology = case_when(V201200 == 1 ~ "Extremely Liberal",
                       V201200 == 2 ~ "Liberal",
                       V201200 == 3 ~ "Slightly Liberal",
                       V201200 == 4 ~ "Moderate",
                       V201200 == 5 ~ "Slightly Conservative",
                       V201200 == 6 ~ "Conservative",
                       V201200 == 7 ~ "Extremely Conservative",
                       V201200 == 99 ~ "Don't Know"),
  Ideo_8 = case_when(Ideology == "Extremely Liberal" ~ 1,
                     Ideology == "Liberal" ~ 2,
                     Ideology == "Slightly Liberal" ~ 3,
                     Ideology == "Moderate" ~ 4,
                     Ideology == "Don't Know" ~ 5,
                     Ideology == "Slightly Conservative" ~ 6,
                     Ideology == "Conservative" ~ 7,
                     Ideology == "Extremely Conservative" ~ 8),
  Party = case_when(V201231x == 1 ~ "Strong Democrat",
                    V201231x == 2 ~ "Not very strong Democrat",
                    V201231x == 3 ~ "Independent-Democrat",
                    V201231x == 4 ~ "Independent",
                    V201231x == 5 ~ "Independent-Republican",
                    V201231x == 6 ~ "Not very strong Republican",
                    V201231x == 7 ~ "Strong Republican"),
  Party_Cont = case_when(Party == "Strong Democrat" ~ 1,
                         Party == "Not very strong Democrat" ~ 2,
                         Party == "Independent-Democrat" ~ 3,
                         Party == "Independent" ~ 4,
                         Party == "Independent-Republican" ~ 5,
                         Party == "Not very strong Republican" ~ 6,
                         Party == "Strong Republican" ~ 7),
  Economy_Current = case_when(V201324 == 1 ~ "Very good",
                              V201324 == 2 ~ "Good",
                              V201324 == 3 ~ "Neither",
                              V201324 == 4 ~ "Bad",
                              V201324 == 5 ~ "Very bad"),
  Economy_Past = case_when(V201325 == 1 ~ "Gotten better",
                           V201325 == 2 ~ "Stayed about the same",
                           V201325 == 3 ~ "Gotten worse"),
  Economy_Past_Extended = case_when(V201327x == 1 ~ "Gotten much better",
                                    V201327x == 2 ~ "Gotten somewhat better",
                                    V201327x == 3 ~ "Stayed about the same",
                                    V201327x == 4 ~ "Gotten somewhat worse",
                                    V201327x == 5 ~ "Gotten much worse"),
  Marital = case_when(V201508 == 1 ~ "Married",
                      V201508 == 2 ~ "Married",
                      V201508 == 3 ~ "Widowed",
                      V201508 == 4 ~ "Divorced",
                      V201508 == 5 ~ "Separated",
                      V201508 == 6 ~ "Never married"),
  Education = case_when(V201511x == 1 ~ "Less than HS",
                        V201511x == 2 ~ "HS",
                        V201511x == 3 ~ "Some post-HS",
                        V201511x == 4 ~ "Bachelor's",
                        V201511x == 5 ~ "Graduate degree"),
  Education_Cont = case_when(Education == "Less than HS" ~ 1,
                             Education == "HS" ~ 2,
                             Education == "Some post-HS" ~ 3,
                             Education == "Bachelor's" ~ 4,
                             Education == "Graduate degree" ~ 5),
  Employed = case_when(V201534x == 1 ~ "Employed",
                       V201534x == 2 ~ "Temp. Laid Off",
                       V201534x == 4 ~ "Unemployed",
                       V201534x == 5 ~ "Retired",
                       V201534x == 6 ~ "Disabled",
                       V201534x == 7 ~ "Homemaker",
                       V201534x == 8 ~ "Student"),
  Hispanic = case_when(V201546 == 1 ~ "Yes",
                       V201546 == 2 ~ "No"),
  Grandparents_Born = case_when(V201555 == 0 ~ 5,                                #None
                                V201555 == 1 ~ 4,                                #1
                                V201555 == 2 ~ 3,                                #2
                                V201555 == 3 ~ 2,                                #3
                                V201555 == 4 ~ 1),                               #4
  Hispanic_Ancestry = case_when(V201558x == 1 ~ "Mexican",
                                V201558x == 2 ~ "Puerto Rican",
                                V201558x == 3 ~ "Other/More than One",
                                V201558x == 4 ~ "Hispanic, undetermined"),
  Parents_Born = case_when(V201553 == 1 ~ 3,                                     # Both in 
                           V201553 == 2 ~ 2,                                     # One in 
                           V201553 == 3 ~ 1 ),                                   # Both out
  Migration_Dist = Grandparents_Born + Parents_Born,
  Language = case_when(V201562 == 1 ~ "Only English",
                       V201562 == 2 ~ "Mostly English",
                       V201562 == 3 ~ "Both languages equally",
                       V201562 == 4 ~ "Mostly Spanish",
                       V201562 == 5 ~ "Only Spanish"),
  Language_Cont = case_when(V201562 == 1 ~ 5,
                            V201562 == 2 ~ 4,
                            V201562 == 3 ~ 3,
                            V201562 == 4 ~ 2,
                            V201562 == 5 ~ 1),
  Border_Wall = case_when(V201426x == 1 ~ 7,                                     # Originally 1 - Favor a great deal, 7 - Oppose a great deal
                          V201426x == 2 ~ 6,
                          V201426x == 3 ~ 5,
                          V201426x == 4 ~ 4,
                          V201426x == 5 ~ 3,
                          V201426x == 6 ~ 2,
                          V201426x == 7 ~ 1),                                     # reversed so decrease means oppose
  
  Hispanic_Candidate = case_when(V202220 == 1 ~ 1,                               # 1 - Extremely Imp, 5 - Not at all 
                                 V202220 == 2 ~ 2, 
                                 V202220 == 3 ~ 3,
                                 V202220 == 4 ~ 4,
                                 V202220 == 5 ~ 5),
  UnfairLaws_Hisp = case_when(V202485 == 1 ~ 1,                                 # 1 - Extremely Imp, 5 - Not at all 
                          V202485 == 2 ~ 2, 
                          V202485 == 3 ~ 3,
                          V202485 == 4 ~ 4,
                          V202485 == 5 ~ 5),
  American_Identity = case_when(V202504 == 1 ~ 1,                               # 1 - Extremely Imp, 5 - Not at all 
                                V202504 == 2 ~ 2, 
                                V202504 == 3 ~ 3,
                                V202504 == 4 ~ 4,
                                V202504 == 5 ~ 5),
  Linked_Fate = case_when(V202506 == 1 ~ 1,                                      # 1 - A lot, 5 - Not at all 
                          V202506 == 2 ~ 2, 
                          V202506 == 3 ~ 3,
                          V202506 == 4 ~ 4),
  Tighten_Border = case_when(V201306 == 1 ~ 1,                                   # 1 - Increased, 0 - Kept the same, -1 Decreased
                             V201306 == 3 ~ 0,
                             V201306 == 2 ~ -1),
  Latino_Identity = Hispanic_Candidate + UnfairLaws_Hisp + Linked_Fate,
  Age = ifelse(V201507x == -9, NA, V201507x)
)

# Subsetting to latinos only
latinos20 <- anes20[anes20$Hispanic == "Yes",]

# Age - NAs


## testing out how many NAs in 2020 
latinos20 %>% summarise_all(~ sum(is.na(.)))
latinos20_clean <- latinos20[!is.na(latinos20$V200001),]

latinos20_clean %>% summarise_all(~ sum(is.na(.)))
# saving 
# write.csv(latinos20_clean, "latinos20_clean.csv")
