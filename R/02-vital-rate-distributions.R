# Creating vital rate distributions 
# Anna Moeller 
# 9/28/2021

# library and data
library(tidyverse)
library(R2jags)
# turdat <- readRDS("data/tier1.rds") 
turdat <- readRDS("data/Eastern_tier1.rds") 
# turdat <- tier1 %>% # From other script
#   filter(`Publication Year` > 2007)

# Survival 
sdat <- turdat %>% 
  filter(vitalrate == "survival", 
         period == "annual"
         ) %>% 
  select(Parameter, tau, age)

# Nest initiation ("nesting rate", "renesting rate")
# "third nest rate" doesn't have any uncertainty so got dumped
nidat <- turdat %>% 
  filter(vitalrate == "nesting rate") %>% 
  select(Parameter, tau, period, age) %>% 
  mutate(period = case_when(
    period == "first nest" ~ 1,
    period == "second nest" ~ 2
  )) 

# Hatching rate 
# We only have first nest 
hdat <- turdat %>% 
  filter(vitalrate == "hatching rate") %>% 
  select(Parameter, tau, period, age) %>% 
  mutate(period = case_when(
    period == "first nest" ~ 1,
    period == "second nest" ~ 2
  )) 

# Nest success 
nsdat <- turdat %>% 
  filter(vitalrate == "nest success") %>% 
  select(Parameter, tau, period, age) %>% 
  mutate(period = case_when(
    period == "first nest" ~ 1,
    period == "second nest" ~ 2
  )) 

# Clutch size 
cdat <- turdat %>% 
  filter(vitalrate == "clutch size") %>%
  select(Parameter, tau, period, age) %>% 
  mutate(period = case_when(
    period == "first nest" ~ 1,
    period == "second nest" ~ 2
  )) 

# Poult survival 
# # 0-28 days only
# psdat <- turdat %>% 
#   filter(vitalrate == "poult survival",
#          periodinfo == "0-28 days post hatch"
#   ) %>% 
#   select(Parameter, tau, age)

# assuming each day has equal survival
# Make sure to only use "0-28 days post hatch" from Strata 19 !!!!!!!!!!!!!!!!!!!
psdat <- turdat %>% 
  filter(vitalrate == "poult survival",
         # periodinfo %in% c("0-14 days post hatch", "0-28 days post hatch",
         #                   "14-28 days post hatch")
         periodinfo %in% c("0-28 days post hatch")
  ) %>% 
  mutate(periodlength = case_when(
    periodinfo == "0-28 days post hatch" ~ 28,
    # periodinfo == "0-14 days post hatch" ~ 14,
    # periodinfo == "14-28 days post hatch" ~ 14
  ) ) %>% 
  select(Parameter, tau, periodlength, age) 

# Other vital rates available... 
# Brood success 
# Nest survival
# Seasonal survival 
# Other lengths of poult survival

# JAGS data
jdata <- list(
  nage = 2,
  
  # Survival 
  sdat = sdat, 
  nS = dim(sdat)[1],
  
  # Nest initiation
  nidat = nidat,
  nNI = dim(nidat)[1],
  nattempt = max(nidat$period),
  
  # Hatching rate 
  hdat = hdat, 
  nH = nrow(hdat),
  nhp = max(hdat$period),
  
  # Nest success
  nsdat = nsdat,
  nNS = nrow(nsdat),
  nnsp = max(nsdat$period),
  
  # Clutch size 
  cdat = cdat,
  nC = nrow(cdat),
  ncp = max(cdat$period),
  meanc = mean(cdat$Parameter),
  tauc = 1/sd(cdat$Parameter),
  
  # Poult survival
  psdat = psdat,
  nPS = nrow(psdat)
)

jinits <- function(){
  list(
    S = runif(2, 0, 1),
    NI = matrix(runif(4, 0, 1), nrow = 2),
    H = matrix(runif(2, 0, 1), nrow = 2),
    NS = matrix(runif(4, 0, 1), nrow = 2),
    # NS = runif(2, 0, 1), # For 2007
    C = matrix(runif(4, 0, 1), nrow = 2), 
    # C = runif(2, 0, 1), # For 2007
    PSD = runif(2, 0, 1) 
  )
}

jparams = c("S", "NI", "H", "NS", "C", "PSD", "PS", "R")

ni <- 30000
nt <- 1
nb <- 10000
nc <- 3

res <- jags( 
  jdata, 
  jinits,
  jparams,
  here::here("models/vital rates process vs sampling error.txt"),
  n.chains = nc, 
  n.iter = ni, 
  n.burnin = nb,
  n.thin = nt
)
res
mcmcplots::mcmcplot(res)

# saveRDS(res, "results/Eastern_parameterestimates11122021.rds")
# saveRDS(res, "results/Eastern_parameterestimates01142022.rds")



# Create a table with sample sizes of estimates actually used 
sampsz <- sdat %>%
  mutate(vitalrate = "annual survival") %>%
  bind_rows(nidat %>% mutate(vitalrate = "nest initiation")) %>%
  bind_rows(hdat %>% mutate(vitalrate = "hatching rate")) %>%
  bind_rows(nsdat %>%  mutate(vitalrate = "nest success")) %>%
  bind_rows(cdat %>%  mutate(vitalrate = "clutch size")) %>%
  bind_rows(psdat %>% mutate(vitalrate = "poult survival to 28 days")) %>%
  rename(nestattempt = period) %>%
  count(vitalrate, age, nestattempt) 
# write_csv(sampsz, "results/samplesizefordistributions.csv")


# Taylor table 1
tab1 <- res$BUGSoutput$summary %>% 
  as.data.frame() %>%
  select(mean, sd, "2.5%", "97.5%") %>% 
  mutate(param = rownames(.)) %>% 
  filter(param != "deviance") %>% 
  separate(param, c("vitalrate", "Age", "Nest Attempt")) %>% 
  mutate("Vital Rate" = case_when(
    vitalrate == "C" ~ "Clutch Size",
    vitalrate == "H" ~ "Hatching Rate",
    vitalrate == "NI"~ "Nest Initiation",
    vitalrate == "NS" ~ "Nest Survival",
    vitalrate == "PS" ~ "Poult Survival to 28 days",
    vitalrate == "S" ~ "Annual Survival",
    vitalrate == "R" ~ "Recruitment"
  )) %>%
  select("Vital Rate", "Age", "Nest Attempt", "mean", "sd", "2.5%", "97.5%")
# write_csv(tab1, "results/Table 1.csv")

# Another way to split out age and nest attempt 
# mutate(age = case_when(
#   grepl("*\\[1", param) ~ 1,
#   grepl("*\\[2", param) ~ 2
#   ),
#   nestattempt = case_when(
#     grepl("*\\[.\\,1", param) ~ 1,
#     grepl("*\\[.\\,2", param) ~ 2
#   )
# )