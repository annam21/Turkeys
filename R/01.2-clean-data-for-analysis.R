# Organize data for analyses
# Anna Moeller 
# 5/5/22

library(tidyverse)

# Read in cleaned and joined data
tur_all <- readRDS("data/alldata_cleaned.rds")

#~~~~~~~~~~~~~~~~~~~~
# Make plot year ####

# If there's a single year associated with the estimate 
oneyr <- tur_all %>% 
  filter(!is.na(EstimateYear),
         !(grepl("-", .$EstimateYear))) %>% 
  mutate(plotyr = as.numeric(EstimateYear))

# If there's a range of years associated with the estimate
mr_est <- tur_all %>% 
  filter(!is.na(EstimateYear),
         grepl("-", .$EstimateYear)) %>% 
  separate(StudyYears, into = c("begin_yr", "end_yr"), sep = "-", remove = F) 

# If there are no years with the estimate or study, use publication year 
pubyr <- tur_all %>% 
  filter(is.na(EstimateYear), is.na(StudyYears)) %>% 
  mutate(plotyr = PublicationYear)

# If there's a single range with the study 
oner <- tur_all %>%
  filter(is.na(EstimateYear)) %>% 
  filter(grepl("-", .$StudyYears), !grepl(";", .$StudyYears)) %>%
  separate(StudyYears, into = c("begin_yr", "end_yr"), sep = "-", remove = F) 
# Where there's multiple ranges with the study 
mr <- tur_all %>%
  filter(is.na(EstimateYear)) %>% 
  filter(grepl(";", .$StudyYears), grepl("-", .$StudyYears)) %>%
  separate(StudyYears, into = c("firstint", "secint"), sep = ";", remove = F) %>% 
  separate(firstint, into = c("begin_yr", "tmp"), sep = "-") %>% 
  separate(secint, into = c("tmp", "end_yr"), sep = "-") %>% 
  select(-tmp) 
# Just years separated by ; (Just Porter)
sr <- tur_all %>% 
  filter(is.na(EstimateYear)) %>% 
  filter(grepl(";", .$StudyYears), !grepl("-", .$StudyYears)) %>%
  separate(StudyYears, into = c("begin_yr", "trash", "end_yr"), sep = ";", remove = F) %>% 
  select(-trash)

# Put ranges back together 
tur_all <- bind_rows(mr_est, oner, mr, sr) %>%
  mutate(plotyr = as.numeric(end_yr)) %>% # chose end yr to be conservative 
  select(-begin_yr, -end_yr) %>% 
  bind_rows(., oneyr, pubyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Select columns I need
tur <- tur_all %>% 
  select(Strata, vitalrate, period, periodinfo, LifeStage, Sex, Parameter, SE, SD, CIwidth,
         LCL, UCL, n, Subspecies, StateProvince, Citation, plotyr) %>% 
         # Year, Years, pubyr,
  # Turn mortality into survival
  mutate(Parameter = replace(Parameter, vitalrate == "annual mortality", 1-Parameter[vitalrate == "annual mortality"]),
         vitalrate = replace(vitalrate, vitalrate == "annual mortality", "annual survival"),
  ) %>%  
  # Only Easterns
  filter(Subspecies == "Eastern")

# Calculate SE from SD (yay, normal distribution!)
sdtose <- tur %>% 
  filter(!is.na(SD)) %>% 
  mutate(SE = SD/sqrt(n))
tur <- tur %>% 
  filter(
    (is.na(SD))
  ) %>%
  bind_rows(., sdtose)

# Calculate SE if it has a CI 
citose <- tur %>%
  filter(
    is.na(SE), 
    !is.na(CIwidth)
  ) %>%
  mutate(tmp = 
           case_when(
             CIwidth == 0.95 ~ 3.92,
             CIwidth == 0.9 ~ 3.29
           ),
         SE = (UCL-LCL)/tmp,
  ) %>% 
  select(-tmp)
tur <- tur %>% 
  filter(
    !(is.na(SE) & 
        !is.na(CIwidth))
  ) %>% 
  bind_rows(., citose)

# Calculate missing SEs for rates
# No SE but yes sample size
# survival, nest survival, brood success, nesting rate, hatching rate, 
#   poult survival, nest success 
rateparams <- c("nesting rate", "nest success", "survival"
                # "poult survival", # n = # broods, so can't do binomial
                # "nest survival", "brood success" # don't use these in model
                
)
seforrate <- tur %>%
  filter(is.na(SE),
         !is.na(n),
         vitalrate %in% rateparams) %>%
  mutate(SE = sqrt(Parameter * (1-Parameter) / n)) 
tur <- tur %>% 
  filter(
    !(is.na(SE) &
        !is.na(n) & 
        vitalrate %in% rateparams)
  ) %>% 
  bind_rows(., seforrate)

# Select only the vital rates I'll use 
tur <- tur %>%
  filter((vitalrate == "survival" & period == "annual")|
           vitalrate == "nesting rate" |
           vitalrate == "hatching rate"|
           vitalrate == "nest success"|
           vitalrate == "clutch size"|
           vitalrate == "poult survival"
  ) %>% 
  # Get rid of poult survival in summer
  filter(!(vitalrate == "poult survival" & periodinfo == "summer")) %>% # also gets rid of NAs
  # the study with repetitive estimates 
  filter(!(vitalrate == "poult survival" & Strata == 19 & periodinfo == "0-14 days post hatch"),
         !(vitalrate == "poult survival" & Strata == 19 & periodinfo == "14-28 days post hatch") )

# After all that, remove if no SE
tur <- tur %>%
  filter(!is.na(SE), SE > 0) %>%
  select(-CIwidth, -SD, -LCL, -UCL)

# Keep only useful data 
tur <- tur %>% 
  filter(LifeStage %in% c("adult", "subadult"),
         Sex == "F") %>%
  mutate(tau = 1/SE,
         age = case_when(LifeStage == "subadult" ~ 1,
                         LifeStage == "adult" ~ 2)) 

# Look at states, remove some if out of native range 
tur %>% 
  distinct(StateProvince)

# Look at impossible values 
tur %>% 
  arrange(n, vitalrate) %>%
  select(-Citation) %>% 
  filter(n<5) %>%
  as.data.frame()

# Number of studies our estimates come from 
tur %>% count(vitalrate, Strata) %>% count(vitalrate)
tur %>% filter(vitalrate=="survival", period == "annual") %>% count(vitalrate, Strata) %>% count(vitalrate)
  
# saveRDS(tur, "data/Eastern_tier1_withsymposium.rds")


