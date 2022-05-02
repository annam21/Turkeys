# Data organization 
# Anna Moeller 
# 8/27/2021

# To do list 
# Bring in symposium data, put through all the same checks/mutations, add to old data
# X Turn mortality rates into survival 
# X Turn mortality error into survival error 
# X Make sure all age classes are correct 
# X Make sure Sex has 2 values only 
# X Turn SD into SE 
# X If it's a rate, I think I can always calculate SE...? 
# X Some of my "rates" aren't rates (nesting rate, nest success)
# X Fix: "nest suvival", "nesting rates"
# X probably ignore short season DSRs, recruitment rate
# X Are any survival rates censoring harvested individuals? 
# X What is brood success? Different from nest success? 
# X Remove/fix standard error = 0
# X Turn all survival rates into annual survival 
# X Combine seasonal Vital Rate with period and parameter comments

library(tidyverse)

# tur_raw <- read_csv("data/turkey_demographics.csv")
# tur_raw <- read_csv("data/ranking data.csv")

# Connecting the 2 data sheets 
tur_raw <- read_csv("data/ranking data.csv") %>% 
  select(Strata:`Usefulness tier`)
tur_over <- read_csv("data/turkey demographics paper overview.csv") %>%
  select(ID, Strata, "Publication Year", Years, Subspecies, Season, State)

# Break up the year ranges ####
# Single range
oner <- tur_over %>%
  filter(grepl("-", .$Years), !grepl(";", .$Years)) %>%
  separate(Years, into = c("begin_yr", "end_yr"), sep = "-", remove = F) 
# Where there's multiple ranges, pick 1st and last year
mr <- tur_over %>%
  filter(grepl(";", .$Years), grepl("-", .$Years)) %>%
  separate(Years, into = c("firstint", "secint"), sep = ";", remove = F) %>% 
  separate(firstint, into = c("begin_yr", "tmp"), sep = "-") %>% 
  separate(secint, into = c("tmp", "end_yr"), sep = "-") %>% 
  select(-tmp) 
# Just years separated by ; (Just Porter)
sr <- tur_over %>% 
  filter(grepl(";", .$Years), !grepl("-", .$Years)) %>%
  separate(Years, into = c("begin_yr", "trash", "end_yr"), sep = ";", remove = F) %>% 
  select(-trash)
# Put them back together 
tur_over_yrs <- tur_over %>% 
  filter(!grepl("-", .$Years), 
         !grepl(";", .$Years)) %>% 
  bind_rows(., oner, mr, sr) 
# Pick out the year I want to keep 
tur_yrs <- left_join(tur_raw, tur_over_yrs, by = "Strata") %>% 
  mutate(
    gyvr = !grepl("-", .$Year) & !is.na(Year),
    # gyover = !is.na(begin_yr), # first or last year of range
    gyover = !is.na(end_yr),
    plotyr = case_when(
      gyvr == T ~ Year,
      # gyvr == F & gyover == T ~ begin_yr 
      gyvr == F & gyover == T ~ end_yr 
      # otherwise NA
    )
  ) %>%
  mutate(plotyr = as.numeric(plotyr)) %>% 
  select(-gyvr, -gyover, -Years)

# Clean up columns, data
tur <- tur_yrs %>% 
  rename(citation = "Strata", 
         vitalrate = "Vital Rate",
         lifestage = "Life Stage",
         error = "Error Measurment",
         # errortype = "Error measurment type", 
         comments = "Parameter Comments",
         treatment = "treatments ?",
         # tier = "Vital Rate Tier",
         # usefulness = "Usefulness tier",
         pubyr = "Publication Year") %>% 
  select(vitalrate, period, lifestage, Sex, Parameter, SE, SD, CIwidth, error,
         LCL, UCL, n,  treatment, #tier, usefulness, 
         citation, pubyr, Year, plotyr, Subspecies, State) %>% 
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
  ) %>% 
  # Remove any missing Parameter values and n = 0
  filter(!is.na(Parameter)) %>% 
         # is.na(n) | n > 0) %>%
  # Only Easterns
  filter(Subspecies == "Eastern")

# Fix rates recorded as percentages 
# "Rates" only excludes clutch size, natality rate and recruitment rate (which might be a rate?)
rateparams <- c("brood success", "hatching rate", "nest DSR", 
                "nest success", "nest survival", "nesting rate", "poult survival", 
                "survival"
                )
p1 <- tur %>% 
  filter(vitalrate %in% rateparams & 
         (Parameter > 1)) %>% 
  mutate(Parameter = Parameter/100,
         SE = SE/100) # And CIs but there aren't any so that's nice
 
tur <- tur %>% 
  filter(!(vitalrate %in% rateparams & 
           (Parameter > 1)) ) %>% 
  bind_rows(., p1)

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
# Get down to the estimates I used only 
# tur <- tur %>%
#   filter((vitalrate == "survival" & period == "annual")|
#            vitalrate == "nesting rate" |
#            vitalrate == "hatching rate"|
#            vitalrate == "nest success"|
#            vitalrate == "clutch size"|
#            (vitalrate == "poult survival" &  periodinfo %in% c("0-28 days post hatch"))
#   )

###############
  
# After all that, remove if no SE
tur <- tur %>%
  filter(!is.na(SE), SE > 0) %>%
  select(-CIwidth, -SD, -LCL, -UCL, -error)

# Keep only useful data 
tier1 <- tur %>% 
  filter(lifestage %in% c("adult", "subadult"),
         Sex == "F") %>%
  mutate(tau = 1/SE,
         age = case_when(lifestage == "subadult" ~ 1,
                         lifestage == "adult" ~ 2)) 

# saveRDS(tier1, "data/tier1.rds")
# saveRDS(tier1, "data/Eastern_tier1.rds")

# Look at impossible values 
tier1 %>% 
  arrange(n, vitalrate) %>%
  filter(n<5) %>%
  as.data.frame()

# How many studies...? 
tur %>% 
  filter(Sex == "F") %>% 
  filter(lifestage %in% c("adult", "subadult"))

