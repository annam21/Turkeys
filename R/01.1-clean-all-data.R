# Join and clean all data 
# Anna Moeller 
# 5/5/22

library(tidyverse)

#~~~~~~~~~~~~~~~~~~~~~~~
# First round of data ####

tur_raw <- read_csv("data/ranking data.csv") %>% 
  select(
    # These were just for me and useless now 
    -`Usefulness tier`, -`Vital Rate Tier`,
    # redundant to overview but would cause problems in join if not always exactly the same 
    -ID, -Authors, -`Publication Year`, -`Publication title`, -`Begin Page`, 
    -`End Page`, -Journal, -Volume, 
  )
tur_over <- read_csv("data/turkey demographics paper overview.csv") %>%
  select(-`...30`) %>% 
  rename(lat = x, 
         lon = y,
         Comments = `Variable comments`) 

# Clean up columns, data
tur_orig <- left_join(tur_raw, tur_over, by = "Strata") 
  
#~~~~~~~~~~~~~~~~~~~~~~~
# Symposium data ####

symp <- read_csv("data/symposium/symposium_vitalrates.csv") %>% 
  select(-Authors, -`Publication title`, -Journal)
sym_over <- read_csv("data/symposium/symposium_overview.csv")

# Clean up columns, data
tur_sym <- left_join(symp, sym_over,
                     by = c("Strata")) %>% 
  select(-begin_yr, -end_yr, -plotyr) %>% 
  rename(Comments = `Parameter Comments`) %>% 
  # Create ID with full citation info 
  unite(ID, Authors, `Publication Year`, `Publication title`, Journal, sep = ". ", remove = F) %>% 
  unite(ID, ID, Volume, sep = " ") %>% 
  unite(ID, ID, `Begin Page`, sep = ": ") %>% 
  unite(ID, ID, `End Page`, sep = "-")

#~~~~~~~~~~~~~~~~~~~~~~~
# Put both together ####

tur <- bind_rows(tur_orig, tur_sym) %>% 
  select(-`Error Measurment`, -`Error measurment type`) %>% 
  rename(Citation = ID,
         vitalrate = "Vital Rate",
         LifeStage = "Life Stage",
         # Error = "Error Measurment",
         # ErrorType = "Error measurment type",
         StudyContext = "Study Context",
         Treatment = "treatments ?",
         PublicationYear = "Publication Year",
         # Title = `Publication title`,
         # BeginPage = `Begin Page`,
         # EndPage = `End Page`,
         EstimateYear = Year,
         StudyYears = Years,
         CoverVegetation = `Cover-vegatation`,
         CoverLandscape = `Cover-landscape`,
         TranslocationStatus = `Translocation status`,
         LandscapeContext = `Landscape context`,
         EstimationMethod = `method of estimation`,
         StateProvince = State
         ) %>%
  select(Strata, Citation, Authors, PublicationYear, 
         # Title, Volume, BeginPage, EndPage, 
         Journal, EstimateYear, StudyYears, 
         vitalrate, period, Subspecies, LifeStage, Sex, Parameter, SE, 
         SD, CIwidth, LCL, UCL, n, 
         EstimationMethod, Comments, StateProvince, Location, lat, lon, 
         Season, Treatment, StudyContext, Harvest, Poaching, Predation, LandscapeContext,
         CoverVegetation, CoverLandscape, 
         Management, Weather, Disease, Other, TranslocationStatus
         ) %>% 
  mutate(LifeStage = replace(LifeStage,
                             LifeStage == "Adult" | LifeStage == "adults",
                             "adult"),
         LifeStage = replace(LifeStage, 
                             LifeStage == "subadults" | LifeStage == "subaudults",
                             "subadult"),
         vitalrate = replace(vitalrate, vitalrate == "nest suvival", "nest survival"),
         vitalrate = replace(vitalrate, vitalrate == "Annual survival", "annual survival"),
         Subspecies = replace(Subspecies, Subspecies == "Mirriam's", "Merriam's")
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
          "renest success", "renest survival", "renesting rate", "renest rate") ~ "second nest",
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
      vitalrate == "renest rate" ~ "nesting rate",
      vitalrate == "third nest rate" ~ "nesting rate",
      vitalrate == "third nest success" ~ "nest success",
      vitalrate == "Production" ~ "poults per hen",
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
      vitalrate == "recruitment rate" ~ "recruitment rate",
      vitalrate == "female success" ~ "female success",
      vitalrate == "annual mortality" ~ "annual mortality"
    ),
    StateProvince = replace(StateProvince, StateProvince == "Wisconson", "Wisconsin"),
    StateProvince = replace(StateProvince, StateProvince == "Conneticut", "Connecticut"),
    StateProvince = replace(StateProvince, StateProvince == "Minnisota", "Minnesota"),
    
    # Clean up poult survival period
    periodinfo = replace(periodinfo, periodinfo == "0-28 days", "0-28 days post hatch"),
    periodinfo = replace(periodinfo, periodinfo == "0-28days", "0-28 days post hatch"),
    periodinfo = replace(periodinfo, periodinfo == "0-14 days", "0-14 days post hatch"),
    periodinfo = replace(periodinfo, periodinfo == "14-28 days", "14-28 days post hatch")
  ) %>%
  # Remove any missing Parameter values 
  filter(!is.na(Parameter)) 

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
         SE = SE/100)
tur <- tur %>% 
  filter(!(vitalrate %in% rateparams & 
             (Parameter > 1)) ) %>% 
  bind_rows(., p1)

# Save data for use in analyses 
saveRDS(tur, "data/alldata_cleaned.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Final touches appendix ####

appnd <- tur %>%
  rename(VitalRate = vitalrate) %>% 
  select(-Strata) %>% 
  arrange(Citation) %>% 
  # For display purposes, recode sex 
  mutate(Sex = case_when(
    Sex == "F" ~ "female",
    Sex == "M" ~ "male",
    Sex == "B" ~ "both"
  ))

# save for appendix 
write_csv(appnd, "results/Appendix 1_all vital rates_withsymposium.csv")
