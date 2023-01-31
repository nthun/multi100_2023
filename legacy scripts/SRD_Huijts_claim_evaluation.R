#---------------------------------------------------------
#-------Source Data Reproduction of Huijts_OY3B----------
#------------------SCORE Phase 2--------------------------
#--------------Claim evaluation code----------------------


#Reproduction analyst: Karolina Urbanska


# Load packages and ESS data --------------------------------------------------------

if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}
if(!require(dplyr)){install.packages("dplyr")}


options(scipen=999) #turn-off scientific notation


#set working directory
setwd("C:/Users/admin/Dropbox/OSF/Bushel Repro Huijts")


#load cleaned data
df <- read.csv("SRD Huijts data final.csv")




#Claim ID: ywpnj6 -------------------------------------------------------

#Analysis Attempt 1
#To evaluate this claim, 24 linear regression models corresponding to female samples in the 24 countries were run to predict the effect of parental status on wellbeing. 

#Results are controlled for age, age squared, marital status, educational level, religious attendance, 
#parents education, paid employment, and whether they were born in the country of residence

#Coefficients for women living with children (compared to childless women) and women with an empty nest (compared to childless women) were inspected. 
#Countries for which both coefficients were of a negative value were considered as a country where childless women reported better wellbeing. 
#These countries were summed to evaluate the claim.

df_f <- filter(df, gndr == 2) #females only dataframe
nrow(df_f) #sample size

#create dataframes for females in each country
df_f_AT <- filter(df_f, cntry == "AT")
df_f_BE <- filter(df_f, cntry == "BE")
df_f_BG <- filter(df_f, cntry == "BG")
df_f_DK <- filter(df_f, cntry == "DK")
df_f_EE <- filter(df_f, cntry == "EE")
df_f_FI <- filter(df_f, cntry == "FI")
df_f_FR <- filter(df_f, cntry == "FR")
df_f_DE <- filter(df_f, cntry == "DE")
df_f_HU <- filter(df_f, cntry == "HU")
df_f_IE <- filter(df_f, cntry == "IE")
df_f_LV <- filter(df_f, cntry == "LV")
df_f_NL <- filter(df_f, cntry == "NL")
df_f_NO <- filter(df_f, cntry == "NO")
df_f_PL <- filter(df_f, cntry == "PL")
df_f_PT <- filter(df_f, cntry == "PT")
df_f_RO <- filter(df_f, cntry == "RO")
df_f_RU <- filter(df_f, cntry == "RU")
df_f_SK <- filter(df_f, cntry == "SK")
df_f_SI <- filter(df_f, cntry == "SI")
df_f_ES <- filter(df_f, cntry == "ES")
df_f_SE <- filter(df_f, cntry == "SE")
df_f_CH <- filter(df_f, cntry == "CH")
df_f_UA <- filter(df_f, cntry == "UA")
df_f_GB <- filter(df_f, cntry == "GB")

###For each of these dataframes, the same simple regression model is constructed
m_AT <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
         data = df_f_AT)
m_BE <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_BE)
m_BG <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_BG)
m_CH <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_CH)
m_DE <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_DE)
m_DK <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_DK)
m_EE <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_EE)
m_ES <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_ES)
m_FI <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_FI)
m_FR <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_FR)
m_HU <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_HU)
m_IE <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_IE)
m_LV <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_LV)
m_NL <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_NL)
m_NO <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_NO)
m_PL <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_PL)
m_PT <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_PT)
m_RO <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_RO)
m_RU <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_RU)
m_SE <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_SE)
m_SI <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_SI)
m_SK <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_SK)
m_UA <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_UA)
m_GB <- lm(wellbeing ~ par_stat + age + age2 +married+ edu + paredu + rel + pdwrk + brncntr,
           data = df_f_GB)

###Coefficients for living with children and empty nest are pulled
##and evaluated country-by country

#Austria
m_AT[["coefficients"]][["par_statLiving w children"]]
m_AT[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are negative - country 1/24 with better wellbeing among childless

#Belgium
m_BE[["coefficients"]][["par_statLiving w children"]]
m_BE[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are positive

#Bulgaria
m_BG[["coefficients"]][["par_statLiving w children"]]
m_BG[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are positive

#Denmark
m_DK[["coefficients"]][["par_statLiving w children"]]
m_DK[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are negative - country 2/24 with better wellbeing among childless

#Estonia
m_EE[["coefficients"]][["par_statLiving w children"]]
m_EE[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are positive

#Finland
m_FI[["coefficients"]][["par_statLiving w children"]]
m_FI[["coefficients"]][["par_statEmpty nest"]]
#One coefficient negative and one positive

#France
m_FR[["coefficients"]][["par_statLiving w children"]]
m_FR[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are negative - country 3/24 with better wellbeing among childless

#Germany
m_DE[["coefficients"]][["par_statLiving w children"]]
m_DE[["coefficients"]][["par_statEmpty nest"]]
#One coefficient positive, one negative

#Hungary
m_HU[["coefficients"]][["par_statLiving w children"]]
m_HU[["coefficients"]][["par_statEmpty nest"]]
#Both coefficient positive

#Ireland
m_IE[["coefficients"]][["par_statLiving w children"]]
m_IE[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are negative - country 4/24 with better wellbeing among childless

#Latvia
m_LV[["coefficients"]][["par_statLiving w children"]]
m_LV[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are negative - country 5/24 with better wellbeing among childless

#Netherlands
m_NL[["coefficients"]][["par_statLiving w children"]]
m_NL[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are negative - country 6/24 with better wellbeing among childless

#Norway
m_NO[["coefficients"]][["par_statLiving w children"]]
m_NO[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are negative - country 7/24 with better wellbeing among childless

#Poland
m_PL[["coefficients"]][["par_statLiving w children"]]
m_PL[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are negative - country 8/24 with better wellbeing among childless

#Portugal
m_PT[["coefficients"]][["par_statLiving w children"]]
m_PT[["coefficients"]][["par_statEmpty nest"]]
#One coefficient positive, one negative

#Romania
m_RO[["coefficients"]][["par_statLiving w children"]]
m_RO[["coefficients"]][["par_statEmpty nest"]]
#One coefficient positive, one negative

#Russia
m_RU[["coefficients"]][["par_statLiving w children"]]
m_RU[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are positive

#Slovakia
m_SK[["coefficients"]][["par_statLiving w children"]]
m_SK[["coefficients"]][["par_statEmpty nest"]]
#One coefficient positive, one negative

#Slovenia
m_SI[["coefficients"]][["par_statLiving w children"]]
m_SI[["coefficients"]][["par_statEmpty nest"]]
#One coefficient positive, one negative

#Spain
m_ES[["coefficients"]][["par_statLiving w children"]]
m_ES[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are negative - country 9/24 with better wellbeing among childless

#Switzerland
m_CH[["coefficients"]][["par_statLiving w children"]]
m_CH[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are negative - country 10/24 with better wellbeing among childless

#Ukraine
m_UA[["coefficients"]][["par_statLiving w children"]]
m_UA[["coefficients"]][["par_statEmpty nest"]]
#Both coefficients are positive

#Great Britain
m_GB[["coefficients"]][["par_statLiving w children"]]
m_GB[["coefficients"]][["par_statEmpty nest"]]
#One coefficient is positive, one negative


# In 10 out of 24 countries, childless women reported better wellbeing than women with children. 

# CONCLUSION: The claim was reproduced successfully. The analysis concludes here.



# Claim ID: 21drv2 -------------------------------------------------------

#Analysis Attempt 1
# A multilevel model predicting wellbeing was constructed, nesting individuals within countries. 
# The model used the data from male respondents only.
# An interaction term of parental status and social norms related to childlessness disapproval was entered as the key predictor. 

# The model controlled for age, age squared, marital status, education, parents' education, religious attendance...
# ...paid work, whether the individual was born in the country of residence, own disapproval of childlessness and own social contacts. 
# The slopes for parental status were allowed to vary (one for living with children and one for empty nest). 


df_m <- filter(df, gndr == 1)

m1 <- lmer(wellbeing~par_stat*childless_disapp_std + #key interaction
             age +age2 +married+ edu + paredu + rel + pdwrk + brncntr + 
             childless_disapp + sclmeet + (1 + par_stat| cntry), 
           data = df_m) #males only
summary(m1)

# For men, the Lives with children x Percent (strongly) disapproves interaction on wellbeing was significant.
# However, the coefficient range did not reproduce within the approximate range of the originally reported coefficient.

# CONCLUSION: The claim was not reproduced successfully. The analysis concludes here.


# Claim ID: 8r3dlz -------------------------------------------------------
# Analysis Attempt 1

# A multilevel model predicting wellbeing was constructed, nesting individuals within countries. 
# The model used the data from female respondents only. 
# An interaction term of parental status and social norms related to childlessness disapproval was entered as the key predictor. 
# The model controlled for age, age squared, marital status, education, parents' education, religious attendance...
# ...paid work, whether the individual was born in the country of residence, own disapproval of childlessness and own social contacts. 
# The slopes for parental status were allowed to vary (one for living with children and one for empty nest). 

m2 <- lmer(wellbeing~par_stat*childless_disapp_std + #key interaction
             age +age2 +married+ edu + paredu + rel + pdwrk + brncntr + 
             childless_disapp + sclmeet + (1 + par_stat| cntry), 
           data = df_f) #females only
summary(m2)


# For women, the Lives with children x Percent (strongly) disapproves interaction on wellbeing was significant.
# However, the coefficient range did not reproduce within the approximate range of the originally reported coefficient.

# CONCLUSION: The claim was not reproduced successfully. The analysis concludes here.



#Claim ID: oko481 and n38dk7
# A multilevel model predicting wellbeing was constructed, nesting individuals within countries. 
# The model used the data from female respondents only. 
# An interaction term of parental status and social norms related to social contacts was entered as the key predictor. 
# The model controlled for age, age squared, marital status, education, parents' education, religious attendance...
# ...paid work, whether the individual was born in the country of residence, own disapproval of childlessness and own social contacts. 
# The slopes for parental status were allowed to vary (one for living with children and one for empty nest). 

# Claims are evaluated by considering the Living with Children x Social Contact interaction and the Empty Nest x Social Contact interaction

m3 <- lmer(wellbeing~par_stat*sclmeet_std + #key interaction
             age +age2 +married+ edu + paredu + rel + pdwrk + brncntr + 
             childless_disapp + sclmeet + (1 + par_stat| cntry), 
           data = df_f) #females only
summary(m3)


# Claim oko481
# For women, the Lives with children x Level of social contacts interaction on wellbeing was significant.
# However, the coefficient range did not reproduce within the approximate range of the originally reported coefficient.

# CONCLUSION: The claim was not reproduced successfully. The analysis concludes here.

# Claim n38dk7
# For women, the Women, Empty nest x Level of social contacts interaction on wellbeing was significant.
# However, the coefficient range did not reproduce within the approximate range of the originally reported coefficient.

# CONCLUSION: The claim was not reproduced successfully. The analysis concludes here.