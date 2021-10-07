# Creating vital rate distributions 
# Anna Moeller 
# 9/28/2021

library(R2jags)

# Survival 
sdat <- tier1 %>% 
  filter(vitalrate == "survival", 
         period == "annual",
         # lifestage == "adult"
         ) %>% 
  mutate(tau = 1/SE,
         age = case_when(lifestage == "subadult" ~ 1,
                         lifestage == "adult" ~ 2)) %>% 
  select(Parameter, tau, age)

# JAGS data
jdata <- list(
  nage = 2,
  # Survival 
  sdat = sdat, 
  nS = dim(sdat)[1]
  # age = Shat$age,
  # Shat = Shat$Parameter,
  # Stau = Shat$tau,
  # nS = length(Shat$Parameter)
)

jinits <- function(){
  list(
    S = runif(2, 0, 1)
  )
}

jparams = c("S")

ni <- 10000
nt <- 1
nb <- 5000
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


# monitor 
S