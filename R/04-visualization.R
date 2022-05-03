# Data visualization 
# Anna Moeller 
# 11/10/2021

library(tidyverse)

# turdat <- readRDS("data/Eastern_tier1.rds") 
turdat <- readRDS("data/Eastern_tier1_withsymposium.rds") 

# By publication year or study year-ish
tst <- turdat %>% 
  # rename(pubyr = `Publication Year`) %>% 
  filter(
    (vitalrate == "survival" & period == "annual")|
      vitalrate == "nesting rate" | 
      vitalrate == "hatching rate" | 
      vitalrate == "nest success" |
      vitalrate == "clutch size" | 
      (vitalrate == "poult survival" &
         periodinfo %in% c("0-28 days post hatch"
                           # "0-14 days post hatch",
                           # "14-28 days post hatch"
                           ))
      # vitalrate == "poult survival" # For everything graph, instead of above lines
  ) %>% 
  mutate(vitalrate = replace(vitalrate, vitalrate == "survival", "annual survival")) %>% 
  group_by(vitalrate, 
           # pubyr
           plotyr
           ) %>% 
  mutate(n = n()) %>%
  # add in publication year if there's still no year
  mutate(plotyr = replace(plotyr, is.na(plotyr) & !is.na(pubyr), pubyr))

xaxis <- seq(1970, 2020, by = 10)
# ggplot(tst, aes(pubyr, n) ) + 
ggplot(tst, aes(plotyr, n)) + 
  geom_col(aes(fill = vitalrate), position = "dodge") + 
  geom_text(aes(label = n, y = n+0.7), size = 2) + 
  facet_wrap(~vitalrate) + 
  theme_classic() + 
  scale_x_continuous(breaks = xaxis) + 
  theme(legend.position = "none") + 
  # xlab("Publication year") + 
  xlab("Study year") +
  ylab("Estimates used")
ggsave("results/vitalrate_year_withsymposium.jpg")

########################################
# All vital rates from review 

# Connecting the 2 data sheets 
tur_raw <- read_csv("data/ranking data.csv") %>% 
  select(Strata:`Usefulness tier`)
tur_over <- read_csv("data/turkey demographics paper overview.csv") %>%
  select(ID, Strata, "Publication Year", Years, Subspecies, Season, State)

# Combine year columns 
tur_orig <- left_join(tur_raw, tur_over, by = "Strata") %>% 
  rename(studyyears = Years) %>% 
  rename(citation = "Strata", 
         vitalrate = "Vital Rate",
         lifestage = "Life Stage",
         error = "Error Measurment", 
         # pubyr = "Publication Year",
         # treatment = "treatments ?",
         # comments = "Parameter Comments",
         ) %>% 
  select(vitalrate, period, lifestage, Sex, Parameter, SE, SD, CIwidth, error,
         LCL, UCL, n,  citation, 
         # pubyr, treatment, 
         Year, Subspecies, State, studyyears, ID) 

# Symposium data
symp <- read_csv("data/symposium/symposium_vitalrates.csv") %>% 
  select(-Authors, -`Publication title`, -Journal)
sym_over <- read_csv("data/symposium/symposium_overview.csv") %>% 
  # Create ID with full citation info 
  unite(ID, Authors, `Publication Year`, `Publication title`, Journal, sep = ". ") %>% 
  unite(ID, ID, Volume, sep = " ") %>% 
  unite(ID, ID, `Begin Page`, sep = ": ") %>% 
  unite(ID, ID, `End Page`, sep = "-")

tur_symp <- left_join(symp, sym_over, by = c("Strata")) %>% 
  rename(citation = "Strata", 
         vitalrate = "Vital Rate",
         lifestage = "Life Stage",
         error = "Error Measurment",
         errortype = "Error measurment type",
         # pubyr = "Publication Year",
         comments = "Parameter Comments"
         ) %>% 
  select(vitalrate, period, lifestage, Sex, Parameter, SE, SD, CIwidth, error,
         LCL, UCL, n, citation, 
         # pubyr, plotyr, 
         Year, Subspecies, State, ID) 

tur_all <- bind_rows(tur_orig, tur_symp)

# Clean up columns, data
tur <- tur_all %>% 
  mutate(lifestage = replace(lifestage, 
                             lifestage == "Adult" | lifestage == "adults",
                             "adult"),
         lifestage = replace(lifestage, 
                             lifestage == "subadults" | lifestage == "subaudults",
                             "subadult"),
         vitalrate = replace(vitalrate, vitalrate == "Annual survival", "annual survival"),
         vitalrate = replace(vitalrate, vitalrate == "nest suvival", "nest survival"),
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
          "renest success", "renest survival", "renesting rate", 
          "renest rate") ~ "second nest", 
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
      vitalrate == "annual mortality" ~ "annual mortality",
      vitalrate == "female success" ~ "female success"
    ),
    State = replace(State, State == "Wisonson", "Wisconsin"),
    State = replace(State, State == "Conneticut", "Connecticut"),
    State = replace(State, State == "Minnisota", "Minnesota")
  ) %>% 
  # For display purposes, recode sex 
  mutate(Sex = case_when(
    Sex == "F" ~ "female",
    Sex == "M" ~ "male",
    Sex == "B" ~ "both"
  ))
  
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

# For nice clean table
appnd <- tur %>% 
  select(ID, vitalrate, period, lifestage, Sex, Subspecies, Parameter, SE,
         SD, CIwidth, LCL, UCL, n, Year, studyyears, State, periodinfo) %>%
  rename(citation = ID) %>% 
  arrange(citation)

# write_csv(appnd, "results/Appendix 1_all vital rates_withsymposium.csv")

# why are we missing some citation numbers? 
# 28, 33, 45, 62

# We had originally planned on using citation number in the table and a list of citations 
# below it 



#####################################################################
# Table 1. vital rate distributions 
# res <- readRDS("results/Eastern_parameterestimates11122021.rds")
# Rmu <- res$BUGSoutput$median$R
# Rsd <- res$BUGSoutput$sd$R
# 
# bind_rows(
#   purrr::map_df(res$BUGSoutput$median, 1),
#   purrr::map_df(res$BUGSoutput$sd, 1),
# 
#   purrr::map_df(res$BUGSoutput$median, 2),
#   purrr::map_df(res$BUGSoutput$sd, 2),
#   
#   purrr::map_df(res$BUGSoutput$median, 3),
#   purrr::map_df(res$BUGSoutput$sd, 3),
#   
#   purrr::map_df(res$BUGSoutput$median, 4),
#   purrr::map_df(res$BUGSoutput$sd, 4),
# ) %>%
#   mutate(metric = rep(c('median', 'sd'), 4),
#          age = rep(c('subadult', 'subadult', 'adult', 'adult'), 2),
#          nestattempt = rep(1:2, each = 4) ) %>%
#   select(-PSD, -deviance) %>%
#   pivot_longer(cols = C:S, names_to = "vitalrate", values_to = "estimate") %>%
#   pivot_wider(names_from = metric, values_from = estimate) %>%
#   arrange(vitalrate, age, nestattempt) %>%
#   as.data.frame()%>%
#   #organization
#   select(vitalrate, age, nestattempt, median, sd) %>%
#   filter(!is.na(median))

