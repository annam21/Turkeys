# Data organization 
# Anna Moeller 
# 8/27/2021

# To do list 
# X Turn mortality rates into survival 
# Turn mortality error into survival error 
# X Make sure all age classes are correct 
# X Make sure Sex has 2 values only 
# Turn SD into SE 
# Fix: "nest suvival", "nesting rates"
# probably ignore short season DSRs, recruitment rate
# Are any survival rates censoring harvested individuals? 
# What is brood success? Different from nest success? 
# X STANDARD ERROR = 0!!!
# Turn all survival rates into annual survival 
# Combine seasonal Vital Rate with period and parameter comments

library(tidyverse)

# tur_raw <- read_csv("data/turkey_demographics.csv")
tur_raw <- read_csv("data/ranking data.csv")
tur <- tur_raw %>% 
  rename(citation = "Strata", 
         vitalrate = "Vital Rate",
         lifestage = "Life Stage",
         error = "Error Measurment",
         # errortype = "Error measurment type", 
         comments = "Parameter Comments",
         treatment = "treatments ?",
         tier = "Vital Rate Tier",
         usefulness = "Usefulness tier") %>% 
  select(vitalrate, period, lifestage, Sex, Parameter, SE, SD, CIwidth, error,
         LCL, UCL, Year, n,  treatment, tier, usefulness, citation) %>% 
  mutate(lifestage = replace(lifestage, 
                             lifestage == "Adult" | lifestage == "adults",
                             "adult")
  ) %>% 
  # Calculate SEs from CIs
  mutate(tmp = 
           case_when(
             CIwidth == 0.95 ~ 3.92,
             CIwidth == 0.9 ~ 3.29
            ),
         SEfromCI = (UCL-LCL)/tmp,
         SE = replace(SE, !is.na(SEfromCI), SEfromCI[!is.na(SEfromCI)])
         ) %>% 
  # Calculate SE from SD 
  # I don't care enough right now. 
  # mutate(
  #   SE = replace(SE, 
  #                citation == 4 & vitalrate == "clutch size",
  #                SD[citation == 4 & vitalrate == "clutch size"]/sqrt(n[citation == 4 & vitalrate == "clutch size"]))
  #   ) %>% 
  select(-SEfromCI, -CIwidth, -LCL, -UCL, -error) %>% 
  # Turn mortality into survival 
  mutate(Parameter = replace(Parameter, vitalrate == "annual mortality", 1-Parameter[vitalrate == "annual mortality"]),
         vitalrate = replace(vitalrate, vitalrate == "annual mortality" | vitalrate == "annual survival", "survival"),
  ) 
  
# Data statistics 
length(unique(tur$citation))
dim(tur)
# Good everything
tur %>% 
  filter(lifestage %in% c("adult", "subadult")) %>% 
  filter(Sex %in% c("F")) %>% 
  # any measure of uncertainty
  filter((!is.na(SE) & SE != 0) | !is.na(SD)) %>% 
  filter(tier == 1) %>%
  distinct(citation) %>%
  dim()
  

# Data exploration
table(tur$vitalrate)
# DSR means "daily survival rate"

# Sex
table(tur$Sex)
table(is.na(tur$Sex))

#Age
table(tur$lifestage)

# Pick out the things I definitely want 
tier1 <- tur %>% 
  filter(lifestage %in% c("adult", "subadult")) %>% 
  filter(Sex %in% c("F")) %>% 
  # any measure of uncertainty
  filter((!is.na(SE) & SE != 0) | !is.na(SD)) %>% 
  filter(tier == 1)
# going to need to dip to tier 2

# questions about model 
# Need Shat[age,area,yr] and Sse[age,area,yr]

tier1 %>% 
  filter(vitalrate=="survival", 
         period == "annual",
         lifestage == "adult")
