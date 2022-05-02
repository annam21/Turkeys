# Symposium data
# Anna Moeller 
# 3/4/2022

library(tidyverse)

symp <- read_csv("data/symposium/symposium_vitalrates.csv")
sym_over <- read_csv("data/symposium/symposium_overview.csv")

# Break up the year ranges ####
# Single range
oner <- sym_over %>%
  filter(grepl("-", .$Years), !grepl(";", .$Years)) %>%
  separate(Years, into = c("begin_yr", "end_yr"), sep = "-", remove = F) 
# Where there's multiple ranges, pick 1st and last year
mr <- sym_over %>%
  filter(grepl(";", .$Years), grepl("-", .$Years)) %>%
  separate(Years, into = c("firstint", "secint"), sep = ";", remove = F) %>% 
  separate(firstint, into = c("begin_yr", "tmp"), sep = "-") %>% 
  separate(secint, into = c("tmp", "end_yr"), sep = "-") %>% 
  select(-tmp) 
# Just years separated by ; (Just Porter)
sr <- sym_over %>% 
  filter(grepl(";", .$Years), !grepl("-", .$Years)) %>%
  separate(Years, into = c("begin_yr", "trash", "end_yr"), sep = ";", remove = F) %>% 
  select(-trash)
# If there's only one year
oney <- sym_over %>%
  filter(!grepl(";", .$Years), !grepl("-", .$Years)) %>%
  mutate(begin_yr = Years,
         end_yr = Years)
# Put them back together 
sym_over_yrs <- sym_over %>% 
  # Get rid of everything with ranges, bring them back in
  filter(!grepl("-", .$Years), 
         !grepl(";", .$Years),
         is.na(Years)) %>% 
  bind_rows(., oner, mr, sr, oney) 
# Pick out the year I want to keep 
tur_yrs <- left_join(symp, sym_over_yrs, by = "Strata") %>% 
  mutate(
    gyvr = !grepl("-", .$Year) & !is.na(Year),
    # gyover = !is.na(begin_yr), # first or last year of range
    gyover = !is.na(Years),
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
         errortype = "Error measurment type",
         comments = "Parameter Comments",
         pubyr = "Publication Year") %>% 
  select(vitalrate, period, lifestage, Sex, Parameter, SE, SD, CIwidth, error,
         LCL, UCL, n,  treatment, 
         citation, pubyr, Year, plotyr, Subspecies, State) %>% 
  mutate(lifestage = replace(lifestage, 
                             lifestage == "Adult" | lifestage == "adults",
                             "adult"),
         lifestage = replace(lifestage, 
                             lifestage == "subadults" | lifestage == "subaudults",
                             "subadult"),
         vitalrate = replace(vitalrate, vitalrate == "Annual survival", "annual survival")
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
