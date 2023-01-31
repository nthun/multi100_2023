#---------------------------------------------------------
#-------Source Data Reproduction of Huijts_OY3B----------
#------------------SCORE Phase 2--------------------------
#--------------Dataset preparation code----------------------
#--------------Analysis Attempt 1----------------------



#OSF Project page:


# Load packages and ESS data --------------------------------------------------------

if(!require(rio)){install.packages("rio")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(car)){install.packages("car")}
if(!require(haven)){install.packages("haven")}
if(!require(sjlabelled)){install.packages("sjlabelled")}
if(!require(tidyr)){install.packages("tidyr")}

#set working directory
setwd("C:/Users/admin/Dropbox/OSF/Bushel Repro Huijts/")

#load datasets including the adjacent Latvia and Romania datasets
ESS3 <- rio::import("ESS3e03_7.sav", user_na = TRUE) 
dfLV <- rio::import("ESS3LV.sav", user_na = TRUE) 
dfRO <- rio::import("ESS3RO.sav", user_na = TRUE) 

df <- rbind(ESS3, dfLV, dfRO) %>% #merge the datasets
  filter(!cntry == c("CY")) #exclude Cyprus

df <- df %>%
  filter(agea > 40) %>% #include individuals over 40
  select(idno, cntry, gndr, pspwght, bthcld, chldhhe, #select variables that are required for the analysis
         rshipa2, rshipa3, rshipa4, rshipa5, rshipa6, rshipa7, rshipa8, rshipa9, rshipa10, rshipa11,
         rshipa12, rshipa13, rshipa14, rshipa15,
         fltdpr, flteeff, slprl, wrhpp, fltlnl, enjlf, fltsd, cldgng, #wellbeing items
         agea, maritala, lvghwa, lvgptna, edulvla, edulvlfa, edulvlma, 
         rlgatnd, pdwrk, brncntr, anvcld, sclmeet)

nrow(df) #check number of observations to check if it matches 27,182
#note this is a deviation as the article reports 27,182 but the actual number is 27,812 (630 more)


# Calculate parental category ---------------------------------------------

#first compute if the respondent lives with a child
df <- mutate(df, live_child = as.numeric(if_else(rshipa2 == 2, 1, #score of 2 in rshipa2-rshipa15 stands for daughter/son
                                                 if_else (rshipa3 == 2, 1, 
                                                          if_else(rshipa4 == 2, 1, 
                                                                  if_else(rshipa5 ==2, 1, 
                                                                          if_else(rshipa6 ==2, 1, 
                                                                                  if_else(rshipa7 == 2, 1, 
                                                                                          if_else(rshipa8 ==2, 1, 
                                                                                                  ifelse(rshipa9 == 2, 1,
                                                                                                         if_else(rshipa10 == 2, 1, 
                                                                                                                 if_else(rshipa11 == 2, 1, 
                                                                                                                         if_else (rshipa12 ==2, 1, 
                                                                                                                                  if_else(rshipa13 ==2, 1, 
                                                                                                                                          if_else (rshipa14 ==2, 1, 
                                                                                                                                                   if_else (rshipa15 ==2, 1, 0))))))))))))))))

#recode live_child NAs to 0
df$live_child <- car::recode(df$live_child, "NA=0")

#three parenting categories: currently living with children
#---(1) lives with children and biological parent 
#---(2) children have left the parental home after reaching adulthood (empty nest), 
#---(3) never had children (childless). 

df <- df %>%
  mutate(par_stat = ifelse(live_child ==1 & bthcld == 1, 1, #lives with a child and gave bith to/fathered a child
                           ifelse(live_child ==0 & bthcld == 1 & chldhhe == 1, 2, #doesn't live with a child but gave bith to/fathered a child
                                  ifelse(live_child == 0 & bthcld==2, 3, NA)))) #doesn't live with a child and didn't give birth to/fathered a child

###calculate country-level childlessness with sample aged > 40
#This was done by aggregating the percentage of people in the childless
#category of the parental status variable for each country
#separately. 

childless <- df %>%
  group_by(cntry, par_stat) %>%
  summarise(n = n()) %>%
  mutate(childless_perc = n / sum(n)*100) %>%
  ungroup() %>%
  filter(par_stat==3) %>%
  select(cntry, childless_perc)
             
#add these norms to the dataset
df <- left_join(df, childless, c("cntry"))


#remove NAs from the parent status
df <- df %>%
  filter(par_stat %in% 1:3) %>%
  filter(gndr %in% 1:2) #keep only males and females


#childless people being the reference group - create factor variable
df$par_stat <- factor(df$par_stat, labels=c("Living w children", "Empty nest", "Childless"))
df <- df %>%
  mutate(par_stat = relevel(par_stat, ref = "Childless")) #make childless the baseline


# Prepare individual-level variables --------------------------------------

#psychological wellbeing
#recode variables 

#Respondents who had missing information on five or more wellbeing items were excluded
#count NAs in wellbeing items
df$miss1 <- car::recode(df$fltsd, 'NA=1; else = 0')
df$miss2 <- car::recode(df$fltdpr, 'NA=1; else = 0')
df$miss3 <- car::recode(df$slprl, 'NA=1; else = 0')
df$miss4 <- car::recode(df$wrhpp, 'NA=1; else = 0')
df$miss5 <- car::recode(df$fltlnl, 'NA=1; else = 0')
df$miss6 <- car::recode(df$enjlf, 'NA=1; else = 0')
df$miss7 <- car::recode(df$cldgng, 'NA=1; else = 0')
df$miss8 <- car::recode(df$flteeff, 'NA=1; else = 0')

df <- df %>%
  mutate(missing = miss1+ miss2+miss3+miss4+miss5+miss6+miss7+miss8) %>% #sum missing values
  filter(missing < 4) #respondents with 5 or more missing values removed

#reverse-code negatively worded items from 1-4 to 3-0 as per the original article coding
df <- df %>% mutate_at(vars(fltdpr, flteeff, slprl, fltlnl, fltsd, cldgng),
                       funs(r = plyr::mapvalues(., 1:4, 3:0))) %>%
  mutate_at(vars(wrhpp, enjlf),
                       funs(r = plyr::mapvalues(., 1:4, 0:3)))

df$wellbeing <- rowMeans(df[c("fltdpr_r", "flteeff_r", "slprl_r", "fltlnl_r", "fltsd_r", "cldgng_r",
                        "wrhpp_r", "enjlf_r")], na.rm=TRUE)


#age - subtract the minimum age (41 years) from the actual age for all respondents
df$age <- df$age - 41

#age-squared
df$age2 <- (df$age)^2


#married
#four groups: married or cohabiting, separated or divorced, widowed, and never married. 
#Cohabiting people were included in the married group
df <- df %>%
  mutate(married = case_when(maritala %in% 1:2 ~ 1,
                             maritala %in% c(4:5, 7) ~ 2,
                             maritala %in% c(6, 8) ~ 3,
                             maritala == 9 ~ 4,
                             lvghwa == 1 ~ 1, ##cohabiting with civil partner also included in the married categor
                             lvgptna == 1 ~ 1)) #cohabiting with a partner also included in the married category
  
df$married <- factor(df$married, labels=c("Married or cohabitating", "Separated or divorced", "Widowed", "Never married"))
df <- df %>%
  mutate(married = relevel(married, ref = "Married or cohabitating"))

#education
df <- df %>%
  mutate(edu = case_when(edulvla == 1 ~ 1,
                         edulvla == 2 ~ 2,
                         edulvla == 3 ~ 3,
                         edulvla == 4 ~ 4,
                         edulvla == 5 ~ 4))

df$edu <- factor(df$edu, labels=c("Primary", "Lower secondary", "Upper secondary", "Teritary"))
df <- df %>%
  mutate(edu = relevel(edu, ref = "Primary")) #primary education as baseline    


#parental education
#the maximum of the father's and the mother's education to create this variable
#include missing data
df$paredu <- pmax(df$edulvlfa, df$edulvlma)
df <- df %>%
  mutate(paredu = case_when(paredu == 1 ~ 1,
                         paredu == 2 ~ 2,
                         paredu == 3 ~ 3,
                         paredu == 4 ~ 4,
                         paredu == 5 ~ 4,
                         paredu == 55 ~ 5,
                         is.na(paredu) ~ 5))

df$paredu <- factor(df$paredu, labels=c("Primary", "Lower secondary", "Upper secondary", "Teritary", "Missing"))
df <- df %>%
  mutate(paredu = relevel(paredu, ref = "Primary"))


#religious attendance
df <- df %>%
  mutate(rel = plyr::mapvalues(rlgatnd, c(1, 2, 3, 4, 5, 6, 7), 
                      c(6, 5, 4, 3, 2, 1, 0)))

#recode individual disapproval towards childlessness
df$childless_disapp <- car::recode(df$anvcld, '5=0; 4=1; 3=2; 2=3; 1=4; else=NA')

df$brncntr <- car::recode(df$brncntr, '1=1; 2=0')

df$sclmeet <- sclmeet - 1 

#paid employment (pdwrk) does not need any transformations


# Prepare country-level norms -----------------------------------------

#childessness disapproval - country level with the full dataset (all age groups)
norm_childless <- rbind(ESS3, dfLV, dfRO) %>% #take the original datasets
  filter(!cntry == c("CY")) %>% #exclude Cyprus
  mutate(childless_disapp_cat = if_else(anvcld == 1 | anvcld ==2, 1, #1 = disapproval
                                  ifelse(anvcld %in% 3:5, 0, NA))) %>% #0 = neither or approval
  group_by(cntry, childless_disapp_cat) %>%
  summarise(
    n = n()) %>%
    mutate(perc_childless_disapp = n / sum(n)*100) %>% #calculate percentage
  ungroup() %>%
  filter(childless_disapp_cat==1) %>% #only preserve frequencies for those disapproving
  mutate(childless_disapp_std = scale(perc_childless_disapp)) %>% #grand mean centre
  select(c("cntry", "childless_disapp_std"))

#add these norms to the dataset
df <- left_join(df, norm_childless, c("cntry"))

#social contact norms - country level with the full dataset (all age groups)
#note: I don't recode this variable from 1-5 to 0-4 as per the original analysis because the final score is mean centered anyway
norm_contact <- rbind(ESS3, dfLV, dfRO) %>% #take the original datasets
  filter(!cntry == c("CY")) %>% #exclude Cyprus
  group_by(cntry) %>%
  summarise(
    sclmeet=mean(sclmeet, na.rm=T)) %>% #calculate country-level mean
  ungroup()  %>%
  mutate(sclmeet_std = scale(sclmeet)) %>% #grand mean centre
  select(c("cntry", "sclmeet_std"))


#add these norms to the dataset
df <- left_join(df, norm_contact, c("cntry"))

#remove labels etc
df <- df %>% 
  remove_all_labels() %>%
  zap_formats() %>%
  zap_widths()

#variables needed for the analysis - save in a different dataframe
df2 <- df %>%
  select(wellbeing, par_stat,
         gndr, age, age2, married, edu, paredu, rel, pdwrk, brncntr,
         childless_disapp_std, sclmeet_std) %>%
  drop_na()

#summarise the constructed dataset with only the key variables
summary(df2)


#save the dataset as ready for the analysis
write.csv(df, "SRD Huijts data final.csv")



###CONCLUSION: Everything looks in order in terms of variable summaries.
#The numbers are slightly different to the original paper which stated
#"24,195 respondents remain available to comprise our final sample (10,477 men and 13,718 women)"
#The sample in this reproduction dataset is larger than that (25,082) 
#The original authors did not specify which variables were used for listwise deletion




