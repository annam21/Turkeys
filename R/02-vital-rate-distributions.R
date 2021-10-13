# Creating vital rate distributions 
# Anna Moeller 
# 9/28/2021

library(R2jags)

# Survival 
sdat <- goodstuff %>% 
  filter(vitalrate == "survival", 
         period == "annual"
         ) %>% 
  select(Parameter, tau, age)

# Nest initiation ("nesting rate", "renesting rate")
# "third nest rate" doesn't have any uncertainty 
nestdat <- goodstuff %>% 
  filter(vitalrate == "nesting rate") %>% 
  select(Parameter, tau, age)
  
renestdat <- goodstuff %>% 
  filter(vitalrate == "renesting rate")

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
  
  # Nest initiation
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