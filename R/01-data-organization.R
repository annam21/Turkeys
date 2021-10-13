# Data organization 
# Anna Moeller 
# 8/27/2021

# To do list 
# X Turn mortality rates into survival 
# X Turn mortality error into survival error 
# X Make sure all age classes are correct 
# X Make sure Sex has 2 values only 
# X Turn SD into SE 
# If it's a rate, I think I can always calculate SE...? 
# Some of my "rates" aren't rates (nesting rate, nest success)
# X Fix: "nest suvival", "nesting rates"
# probably ignore short season DSRs, recruitment rate
# Are any survival rates censoring harvested individuals? 
# What is brood success? Different from nest success? 
# X Remove/fix standard error = 0
# Turn all survival rates into annual survival 
# X Combine seasonal Vital Rate with period and parameter comments

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
                             "adult"),
         vitalrate = replace(vitalrate, vitalrate == "nest suvival", "nest survival")
  ) %>%
  # Turn mortality into survival 
  mutate(Parameter = replace(Parameter, vitalrate == "annual mortality", 1-Parameter[vitalrate == "annual mortality"]),
         vitalrate = replace(vitalrate, vitalrate == "annual mortality", "annual survival"),
  ) %>%
  # Rename my vital rates and create appropriate periods
  mutate(
    periodinfo = period,
    period = case_when(
      vitalrate == "annual survival" ~ "annual",
      vitalrate == "seasonal survival" ~ "seasonal",
      vitalrate == "Nesting Season survival (DSR)" ~ "daily",
      vitalrate == "post-nesting survival (DSR)" ~ "daily",
      vitalrate == "poult survival" ~ "seasonal",
      vitalrate == "poult survival to November (DSR)" ~ "daily",
      vitalrate %in% 
        c("brood success", "clutch size", "hatching rate", 
          "natality rate", "nest DSR", "nest success", "nest survival",
          "nesting rate") ~ "first nest",
      vitalrate %in% 
        c("renest clutch size", "renest DSR", "renest hatching rate", 
          "renest success", "renest survival", "renesting rate") ~ "second nest", 
      vitalrate %in% c("third nest rate", "third nest success")~ "third nest",
      vitalrate == "recruitment rate" ~ "annual"
    ),
    vitalrate = case_when(
      vitalrate == "annual survival" ~ "survival",
      vitalrate == "seasonal survival"~ "survival",
      vitalrate == "Nesting Season survival (DSR)" ~ "survival",
      vitalrate == "post-nesting survival (DSR)" ~ "survival",
      vitalrate == "poult survival to November (DSR)" ~ "poult survival",
      vitalrate == "renest clutch size" ~ "clutch size",
      vitalrate == "renest DSR" ~ "nest DSR",
      vitalrate == "renest hatching rate" ~ "hatching rate",
      vitalrate == "renest success" ~ "nest success",
      vitalrate == "renest survival" ~ 'nest survival',
      vitalrate == "renesting rate" ~ "nesting rate",
      vitalrate == "third nest rate" ~ "nesting rate",
      vitalrate == "third nest success" ~ "nest success",
      # Then all the others I don't want to change 
      vitalrate == "brood success" ~ "brood success",
      vitalrate == "clutch size" ~ "clutch size",
      vitalrate == "hatching rate" ~ "hatching rate",
      vitalrate == "natality rate" ~ "natality rate",
      vitalrate == "nest DSR" ~ "nest DSR",
      vitalrate == "nest success" ~ "nest success",
      vitalrate == "nest survival" ~ "nest survival",
      vitalrate == "nesting rate" ~ "nesting rate",
      vitalrate == "poult survival" ~ "poult survival",
      vitalrate == "recruitment rate" ~ "recruitment rate"
    )
  ) 

# Calculate missing SEs for rates
# This only excludes clutch size and recruitment rate (which might be a rate?)
rateparams <- c("brood success", "hatching rate", "natality rate", "nest DSR", 
                "nest success", "nest survival", "nesting rate", "poult survival", 
                "survival"
                )
# No SE but yes sample size
tur %>%
  filter(is.na(SE),
         !is.na(n),
         vitalrate %in% rateparams) %>%
  mutate(SE = sqrt(Parameter * (1-Parameter) / n)) %>% 
  filter(is.na(SE))

# Calculate SEs from CIs for population means
citose <- tur %>%
  filter(is.na(SE), !is.na(CIwidth)) %>%
  mutate(tmp = 
           case_when(
             CIwidth == 0.95 ~ 3.92,
             CIwidth == 0.9 ~ 3.29
            ),
         SE = (UCL-LCL)/tmp,
         ) %>% 
    select(-tmp)

# Calculate SE from SD 
sdtose1 <- tur %>% 
  filter(!is.na(SD),
         vitalrate %in% c("clutch size", "renest clutch size")) %>% 
  mutate(SE = SD/sqrt(n))
sdtose2<- tur %>% 
  filter(!is.na(SD),
         vitalrate %in% c("hatching rate", "renest hatching rate", "poult survival")) %>%
  mutate(SE = sqrt(Parameter * (1-Parameter) / n))

# Put it all back together
tur <- bind_rows(tur, sdtose1, sdtose2) %>% 
  select(-tmp, -SEfromCI, -CIwidth, -LCL, -UCL, -error) 
 
  
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
goodstuff <- tur %>% 
  filter(lifestage %in% c("adult", "subadult")) %>% 
  filter(Sex %in% c("F")) %>% 
  # any measure of uncertainty
  filter((!is.na(SE) & SE != 0) | !is.na(SD)) %>%
  mutate(tau = 1/SE,
         age = case_when(lifestage == "subadult" ~ 1,
                         lifestage == "adult" ~ 2)) 












# tur %>%
#   # Rename my vital rates 
#   mutate(
#     periodinfo = period,
#     # Annual survival
#     period = replace(period, vitalrate == "annual survival", "annual"),
#     vitalrate = replace(vitalrate, vitalrate == "annual survival", "survival"),
#     
#     # Seasonal survival 
#     period = replace(period, vitalrate == "seasonal survival", "seasonal"),
#     vitalrate = replace(vitalrate, vitalrate == "seasonal survival", "survival"),
#     period = replace(period, vitalrate == "Nesting Season survival (DSR)", "daily"),
#     vitalrate = replace(vitalrate, vitalrate == "Nesting Season survival (DSR)", "survival"),
#     period = replace(period, vitalrate == "post-nesting survival (DSR)", "daily"),
#     vitalrate = replace(vitalrate, vitalrate == "post-nesting survival (DSR)", "survival"),
#       
#     # Poults
#     period = replace(period, vitalrate == "poult survival", "seasonal"),
#     period = replace(period, vitalrate == "poult survival to November (DSR)", "daily"),
#     vitalrate = replace(vitalrate, vitalrate == "poult survival to November (DSR)", "poult survival"),
#     
#     # Recruitment 
#     
#     # I'm interpreting "first nest" if it's not specified
#     period = replace(
#       period, 
#       vitalrate %in% c("brood success", "clutch size", "hatching rate", 
#                        "natality rate", "nest DSR", "nest success", "nest survival",
#                        "nesting rate"),
#       "first nest"
#     ),
#     # Read "renest" as second nest 
#     period = replace(
#       period, 
#       vitalrate %in% c("renest clutch size", "renest DSR", "renest hatching rate", 
#                        "renest success", "renest survival", "renesting rate"),
#       "second nest"
#     ),
#     vitalrate = replace(vitalrate, vitalrate == "renest clutch size", "clutch size"),
#     # Fix all here 
#     
#     # Third nest 
#     period = replace(
#       period, 
#       vitalrate %in% c("third nest rate", "third nest success"),
#       "third nest"
#     ),
#     # Fix all these 
#     # vitalrate = replace(vitalrate, )
#   ) %>% 
#   select(vitalrate, period, periodinfo)