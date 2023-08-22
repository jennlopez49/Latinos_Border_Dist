# ANES DATA IMPORT - 2016 

# anes_12 <- read_dta("anes_12/anes_timeseries_2012_Stata12.dta")
anes_16 <- read_dta("anes_16/anes_timeseries_2016_Stata12.dta")


# getting rid of mode, etc to use lasso 
# 
# anes12 <- anes_12[,-c(1,3:13)]

# lasso on all possible variables 
# 2012:
# hisp: language (dem_hisplang), 

# 2016:
# V161196 is oppose/favor wall, V161196a is degree of opposition/favoring

# Controls: Age (V161267), gender (V161342), education (V161270 or V165741), party (V161158x),
# marital (V165739), 

# Hispanic vars: language (V161323), arrival (V161321a), become us cit (V161322a),
# mex (V161320), hispanic ancestry country (V161319x)

# 2020 

# hisp: language (V201562), 
latinos16_short <- anes_16 %>% select(V160001, V161003, V161309, V161004, V161009, 
                                              V161127, V161140x, V161141x, 
                                              V161158x, V161310x,
                                              V161315, V161317, V161319x,
                                              V161323, V162326, V161196x, 
                                      V161342, V160101, V161270, V161126, V160102,
                                      V160201, V160202, V162268, V162269, V162270,
                                      V162224, V162222, V162332, V162221, V161196)
latinos16_short <- latinos16_short[latinos16_short$V161309 == 1,]
# saving latinos only 2016, csv --------

write.csv(latinos16_short, "latinos16.csv")

# loading ---------------
latinos16 <- read.csv("latinos16.csv")
latinos16 <- na.omit(latinos16)

# getting rid of restricted, if needed 
#latinos16_clean <- latinos16 %>% replace_with_na_all(condition = ~.x == -3)
#latinos16_clean <-  latinos16_clean[, unlist(lapply(latinos16_clean, function(x) !all(is.na(x))))]

#### --- adding in factor labels -------------

latinos_16 <- latinos16 %>% mutate(
    # 162171a -- alt 3 category one
    Ideology = case_when(V161126 == 1 ~ "Extremely Liberal",
                         V161126 == 2 ~ "Liberal",
                         V161126 == 3 ~ "Slightly Liberal",
                         V161126 == 5 ~ "Slightly Conservative",
                         V161126 == 6 ~ "Conservative",
                         V161126 == 7 ~ "Extremely Conservative",
                         V161126 == 99 ~ "Don't Know",
                         V161127 == 1 ~ "Liberal",
                         V161127 == 2 ~ "Conservative",
                         V161127 == 3 ~ "Moderate"),
    Ideo_8 = case_when(Ideology == "Extremely Liberal" ~ 1,
                       Ideology == "Liberal" ~ 2,
                       Ideology == "Slightly Liberal" ~ 3,
                       Ideology == "Moderate" ~ 4,
                       Ideology == "Don't Know" ~ 5,
                       Ideology == "Slightly Conservative" ~ 6,
                       Ideology == "Conservative" ~ 7,
                       Ideology == "Extremely Conservative" ~ 8),
    Attention_to_Politics = case_when(V161003 == 1 ~ "Always",
                                      V161003 == 2 ~ "Most of the time",
                                      V161003 == 3 ~ "About half of the time",
                                      V161003 == 4 ~ "Some of the time",
                                      V161003 == 5 ~ "Never"),
    Interest_Campaigns = case_when(V161004 == 1 ~ "Very much interested",
                                   V161004 == 2 ~ "Somewhat interested",
                                   V161004 == 3 ~ "Not much interested"),
    Attention_NewsMedia = case_when(V161009 == 1 ~ "A great deal",
                                    V161009 == 2 ~ "A lot",
                                    V161009 == 3 ~ "A moderate amount",
                                    V161009 == 4 ~ "A little",
                                    V161009 == 5 ~ "None at all"),
    Economy_PastYear = case_when(V161140x == 1 ~ "Much better",
                                 V161140x == 2 ~ "Somewhat better",
                                 V161140x == 3 ~ "About the same",
                                 V161140x == 4 ~ "Somewhat worse",
                                 V161140x == 5 ~ "Much worse"),
    Economy_Future = case_when(V161141x == 1 ~ "Get much better",
                               V161141x == 2 ~ "Get somewhat better",
                               V161141x == 3 ~ "About the same",
                               V161141x == 4 ~ "Get somewhat worse",
                               V161141x == 5 ~ "Get much worse"),
    Party = case_when(V161158x == 1 ~ "Strong Democrat",
                      V161158x == 2 ~ "Not very strong Democrat",
                      V161158x == 3 ~ "Independent-Democrat",
                      V161158x == 4 ~ "Independent",
                      V161158x == 5 ~ "Independent-Republican",
                      V161158x == 6 ~ "Not very strong Republican",
                      V161158x == 7 ~ "Strong Republican"),
    Race = case_when(V161310x == 1 ~ "White, Non-Hisp",
                     V161310x == 2 ~ "Black, Non-Hisp",
                     V161310x == 3 ~ "Asian",
                     V161310x == 4 ~ "Native American",
                     V161310x == 5 ~ "Hispanic",
                     V161310x == 6 ~ "Other"),
    Parents_Born = case_when(V161315 == 1 ~ "Both born in the US",
                             V161315 == 2 ~ "One parent born in the US",
                             V161315 == 3 ~ "Both born outside the US"),
    Grandparents_Born = case_when(V161317 == 0 ~ "None",
                                  V161317 == 1 ~ "One",
                                  V161317 == 2 ~ "Two",
                                  V161317 == 3 ~ "Three",
                                  V161317 == 4 ~ "All"),
    Latin_Country = case_when(V161319x == 1 ~ "Mexico",
                              V161319x == 2 ~ "Puerto Rican",
                              V161319x == 3 ~ "Other/More than one",
                              V161319x == 4 ~ "Hispanic, undisclosed"),
    Language = case_when(V161323 == 1 ~ "Only English",
                         V161323 == 2 ~ "Mostly English",
                         V161323 == 3 ~ "Both languages equally",
                         V161323 == 4 ~ "Mostly Spanish",
                         V161323 == 5 ~ "Only Spanish"),
    Language_Cont = case_when(V161323 == 1 ~ 5,
                              V161323 == 2 ~ 4,
                              V161323 == 3 ~ 3,
                              V161323 == 4 ~ 2,
                              V161323 == 5 ~ 1),
    Identity_Importance = case_when(V162326 == 1 ~ "Extremely Important",
                                    V162326 == 2 ~ "Very Important",
                                    V162326 == 3 ~ "Moderately Important",
                                    V162326 == 4 ~ "A little Important",
                                    V162326 == 5 ~ "Not at all Important"),
    Identity_Importance_Cont = case_when(V162326 == 1 ~ 1,
                                    V162326 == 2 ~ 2,
                                    V162326 == 3 ~ 3,
                                    V162326 == 4 ~ 4,
                                    V162326 == 5 ~ 5),
    Border_Wall = case_when(V161196x == 1 ~ "Favor a great deal",
                            V161196x == 2 ~ "Favor a moderate amount",
                            V161196x == 3 ~ "Favor a little",
                            V161196x == 4 ~ "Neither favor nor oppose",
                            V161196x == 5 ~ "Oppose a little",
                            V161196x == 6 ~ "Oppose a moderate amount",
                            V161196x == 7 ~ "Oppose a great deal"),
    Border_Reordered =  case_when(V161196x == 1 ~ 7,
                                  V161196x == 2 ~ 6,
                                  V161196x == 3 ~ 5,
                                  V161196x == 4 ~ 4,
                                  V161196x == 5 ~ 3,
                                  V161196x == 6 ~ 2,
                                  V161196x == 7 ~ 1),
    Border_Short = case_when(V161196x == 1 ~ 5,                                  # 1 - Favor a lot, 2 - Favor, 3 - Neither, 4 - Oppose, 5 - Oppose a lot
                             V161196x == 2 | V161196x == 3  ~ 4,
                             V161196x == 4 ~ 3,
                             V161196x == 5 | V161196x == 6  ~ 2,
                             V161196x == 7 ~ 1),
    # Border_Bin = case_when(V161196x == 1 ~ 1,
    #                        V161196x == 2 ~ 1,
    #                        V161196x == 3 ~ 1,
    #                        V161196x == 5 ~ 0,
    #                        V161196x == 6 ~ 0,
    #                        V161196x == 7 ~ 0),
    Gender = case_when(V161342 == 1 ~ "Male",
                       V161342 == 2 ~ "Female"),
    Age = V161342,
    Education = case_when(V161270 < 9 ~ "Less than HS",
                          V161270 == 9 ~ "HS",
                          V161270 == 10 ~ "Some College",
                          V161270 == 11 ~ "Some College",
                          V161270 == 12 ~ "Some College",
                          V161270 == 13 ~ "Bachelor's",
                          V161270 == 14 ~ "Post-Graduate",
                          V161270 == 15 ~ "Post-Graduate",
                          V161270 == 16 ~ "Post-Graduate"),
    Immigrants_Economy = case_when(V162268 == 1 ~ 1,                             # 1 agree strongly that they are good for econ, 5 disagree strongly 
                                   V162268 == 2 ~ 2,
                                   V162268 == 3 ~ 3,
                                   V162268 == 4 ~ 4,
                                   V162268 == 5 ~ 5),
    Immigrants_HarmCulture = case_when(V162269 == 1 ~ 5,                         # 1 - disagree strongly that imm harm culture, 5 agree strongly 
                                       V162269 == 2 ~ 4,
                                       V162269 == 3 ~ 3,
                                       V162269 == 4 ~ 2,
                                       V162269 == 5 ~ 1),
    Immigrants_Crime = case_when(V162270 == 1 ~ 5,                               # 1 - disagree strongly that imms increase crime, 5 agree strongly 
                                 V162270 == 2 ~ 4,
                                 V162270 == 3 ~ 3,
                                 V162270 == 4 ~ 2,
                                 V162270 == 5 ~ 1),
    Linked_Fate = case_when(V162224 == 1 ~ 1,                                    # 1 - life affected a lot, 4 - not at all 
                            V162224 == 2 ~ 2,
                            V162224 == 3 ~ 3,
                            V162224 == 4 ~ 4),
    News_Language = case_when(V162222 == 1 ~ 1,                                 # 1 - English More, 3- Spanish More, 2 - Equally
                              V162222 == 3 ~ 2,
                              V162222 == 2 ~ 3),
    Hispanic_Candidates = case_when(V162221 == 1 ~ 1,                           # 1 - Extremely Important, 5 - Not at all important
                                    V162221 == 2 ~ 2,
                                    V162221 == 3 ~ 3,
                                    V162221 == 4 ~ 4,
                                    V162221 == 5 ~ 5),
    American_Identity = case_when(V162332 == 1 ~ 1,                             # 1 - Extremely Important, 5 - Not at all important
                                  V162332 == 2 ~ 2,
                                  V162332 == 3 ~ 3,
                                  V162332 == 4 ~ 4,
                                  V162332 == 5 ~ 5),
    Latino_Identity = Linked_Fate + Hispanic_Candidates,
    Immigrant_Attitudes = Immigrants_Economy + Immigrants_HarmCulture + Immigrants_Crime
    
)

# how many non-responses
latinos_16 %>% summarise_all(~ sum(is.na(.)))

# testing out cleaning
latinos_16_clean <- na.omit(latinos_16)

## getting rid of non-controls 

latinos_withoutideo <- latinos16[,-6]
reg <- lm(V161196 ~ ., data = latinos_withoutideo) 
x <- model.matrix(reg)

x <- x[, -1]
dim(x)

y <- latinos_withoutideo$V161196

set.seed(123)
lr_cv <- cv.glmnet(x, y)

# seeing the "best" coefs (though using the regularized model produces 0 coefs, using min instead):
coef(lr_cv) 




