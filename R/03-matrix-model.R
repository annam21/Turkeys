# Turkey matrix model 
# Anna Moeller 
# 9/3/2021

library(msm) # rtnorm

# Use distributions we got for each vital rate 
# load data 
# res <- readRDS("results/parameterestimates10192021.rds")
# res <- readRDS("results/Eastern_parameterestimates11122021.rds")
res <- readRDS("results/Eastern_parameterestimates01142022.rds")
Rmu <- res$BUGSoutput$median$R
Rsd <- res$BUGSoutput$sd$R
Smu <- res$BUGSoutput$median$S
Ssd <- res$BUGSoutput$sd$S

# Regression and lambda
nrep <- 1000

# pre-birth matrix (only 1yo and 2yo exist. Initiate,succeed,etc the next day)
lambda <- rep(NA, nrep)
S <- matrix(NA, nrow = nrep, ncol = 2)
R <- matrix(NA, nrow = nrep, ncol = 2)
for(i in 1:nrep){
  # S[i,] <- rnorm(2, Smu, Ssd) # fine but I only use adult survival
  S[i,] <- rtnorm(2, Smu, Ssd, lower = 0, upper = 1)
  # R[i,] <- rnorm(2, Rmu, Rsd)
  R[i,] <- rtnorm(2, Rmu, Rsd, lower = 0, upper = 1)
  mat <- matrix(
    c(
      R[i,1], R[i,2],
      S[i,2], S[i,2] # 1yo and 2yo live at adult survival 
    ),
    nrow = 2,
    byrow = TRUE
  )
  lambda[i] <- eigen(mat)$values[1]
}

# Visualize 
mnl <- mean(lambda)
hist(lambda)
abline(v = mnl, col = "red", lty = "dashed", lwd = 2)
text(x = mnl+0.2, y = 190, labels = paste0("lambda ", round(mnl, 2)), col = "red")

par(mfrow = c(1,1))
s1 <- lm(lambda ~ S[,1])
plot(S[,1], lambda)
abline(s1)

s2 <- lm(lambda ~ S[,2])
plot(S[,2], lambda)
abline(s2)

r1 <- lm(lambda ~ R[,1])
plot(R[,1], lambda)
abline(r1)

r2 <- lm(lambda ~ R[,2])
plot(R[,2], lambda)
abline(r2)

summary(s1)$r.squared
summary(s2)$r.squared
summary(r1)$r.squared
summary(r2)$r.squared

# Full matrix - all vital rates ####
# curve(dnorm(x, res$BUGSoutput$median$NI[2,2], res$BUGSoutput$sd$NI[2,2]))
# hist(rnorm(1000, res$BUGSoutput$median$NI[2,1], res$BUGSoutput$sd$NI[2,1]))
# All have 2 columns - age 1 and age 2

# truncate normal (doesn't change much but ok)
nrep <- 1000
lambda <- rep(NA, nrep)
NI1 <- NI2 <- NS1 <- NS2 <- C1 <- C2 <- H <- PS <- S <- R<- matrix(NA, nrow = nrep, ncol = 2)
for(i in 1:nrep){
  # NI1[i,] <- rnorm(2, res$BUGSoutput$median$NI[,1], res$BUGSoutput$sd$NI[,1])
  # NI2[i,] <- rnorm(2, res$BUGSoutput$median$NI[,2], res$BUGSoutput$sd$NI[,2])
  # NS1[i,] <- rnorm(2, res$BUGSoutput$median$NS[,1], res$BUGSoutput$sd$NS[,1])
  # NS2[i,] <- rnorm(2, res$BUGSoutput$median$NS[,2], res$BUGSoutput$sd$NS[,2])
  # C1[i,] <- rnorm(2, res$BUGSoutput$median$C[,1], res$BUGSoutput$sd$C[,1])
  # C2[i,] <- rnorm(2, res$BUGSoutput$median$C[,2], res$BUGSoutput$sd$C[,2])
  # H[i,] <- rnorm(2, res$BUGSoutput$median$H[,1], res$BUGSoutput$sd$H[,1])
  # PS[i,] <- rnorm(2, res$BUGSoutput$median$PS, res$BUGSoutput$sd$PS)
  # S[i,] <- rnorm(2, res$BUGSoutput$median$S, res$BUGSoutput$sd$S)
  
  NI1[i,] <- rtnorm(2, res$BUGSoutput$median$NI[,1], res$BUGSoutput$sd$NI[,1],
                    lower = 0, upper = 1)
  NI2[i,] <- rtnorm(2, res$BUGSoutput$median$NI[,2], res$BUGSoutput$sd$NI[,2],
                    lower = 0, upper = 1)
  NS1[i,] <- rtnorm(2, res$BUGSoutput$median$NS[,1], res$BUGSoutput$sd$NS[,1],
                    lower = 0, upper = 1)
  NS2[i,] <- rtnorm(2, res$BUGSoutput$median$NS[,2], res$BUGSoutput$sd$NS[,2],
                    lower = 0, upper = 1)
  C1[i,] <- rnorm(2, res$BUGSoutput$median$C[,1], res$BUGSoutput$sd$C[,1])
  C2[i,] <- rnorm(2, res$BUGSoutput$median$C[,2], res$BUGSoutput$sd$C[,2])
  H[i,] <- rtnorm(2, res$BUGSoutput$median$H[,1], res$BUGSoutput$sd$H[,1],
                  lower = 0, upper = 1)
  PS[i,] <- rtnorm(2, res$BUGSoutput$median$PS, res$BUGSoutput$sd$PS,
                   lower = 0, upper = 1)
  S[i,] <- rtnorm(2, res$BUGSoutput$median$S, res$BUGSoutput$sd$S,
                  lower = 0, upper = 1)
  
  R[i,] <- NI1[i,] * NS1[i,] * C1[i,] * 0.5 * H[i,] * PS[i,] * S[i,1]^(48/52) + 
    NI1[i,] * (1-NS1[i,]) * NI2[i,] * NS2[i,] * C2[i,] * 0.5 * H[i,] * PS[i,] *
    S[i,1]^(48/52)
  
  mat <- matrix(
    c(
      R[i,1], R[i,2],
      S[i,2], S[i,2]
    ),
    nrow = 2,
    byrow = TRUE
  )
  lambda[i] <- eigen(mat)$values[1]
}

# Visualize 
mnl <- mean(lambda)
hist(lambda)
abline(v = mnl, col = "red", lty = "dashed", lwd = 2)
text(x = mnl+0.2, y = 150, labels = paste0("lambda ", round(mnl, 2)), col = "red")

# Regression by age
ni1 <- apply(NI1, 2, function(x) lm(lambda~x))
lapply(ni1, function(x) summary(x)$r.squared)

ns1 <- apply(NS1, 2, function(x) lm(lambda~x))
lapply(ns1, function(x) summary(x)$r.squared)

c1 <- apply(C1, 2, function(x) lm(lambda~x))
lapply(c1, function(x) summary(x)$r.squared)

h <- apply(H, 2, function(x) lm(lambda~x))
lapply(h, function(x) summary(x)$r.squared)

ps <- apply(PS, 2, function(x) lm(lambda~x))# Not sure this will work 
lapply(ps, function(x) summary(x)$r.squared)

s1 <- apply(S, 2, function(x) lm(lambda~x)) # actually just s
lapply(s1, function(x) summary(x)$r.squared)

ni2 <- apply(NI2, 2, function(x) lm(lambda~x))
lapply(ni2, function(x) summary(x)$r.squared)

ns2 <- apply(NS2, 2, function(x) lm(lambda~x))
lapply(ns2, function(x) summary(x)$r.squared)

c2 <- apply(C2, 2, function(x) lm(lambda~x))
lapply(c2, function(x) summary(x)$r.squared)

# Stack these
library(dplyr)
getr2 <- function(mod){
  tmp <- lapply(mod, function(x) summary(x)$r.squared)
  tmp2 <- as.data.frame(tmp)
  names(tmp2) <- c("age1", "age2")
  dat2 <- data.frame(vr = deparse(substitute(mod)) ) # name of vital rate
  cbind(tmp2, dat2)
}
# getr2(ni1)
repro <- bind_rows(getr2(ni1), getr2(ns1), getr2(c1), getr2(h), getr2(ps), 
          getr2(ni2), getr2(ns2), getr2(c2)) 
# survival is sort of its own thing
getr2(s1)

apply(repro[,1:2], 2, sum) # total variation explained by these pieces 



# Plot a few of them
# ni1 <- lm(lambda ~ NI1[,1])
# plot(NI1[,1], lambda)
# abline(ni1)
# summary(ni1)$r.squared

plot(NI1[,2], lambda, 
     pch = 16, 
     col = scales::alpha("black", 0.2),
     xlab = "NI1 adults")
abline(ni1[[2]], col = "red")

plot(PS[,2], lambda, 
     pch = 16, 
     col = scales::alpha("black", 0.2),
     xlab = "PS from adults")
abline(ps[[2]], col = "red")

###########################################
# Beta distribution instead of normal 
# estBetaParams <- function(mu, var) {
#   alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
#   beta <- alpha * (1 / mu - 1)
#   return(params = list(alpha = alpha, beta = beta))
# }
# 
# NIbeta <- estBetaParams(res$BUGSoutput$median$NI, (res$BUGSoutput$sd$NI)^2)
# # hist(rbeta(10000, NItst$alpha, NItst$beta))


############################################33
# Elasticity 
popbio::elasticity(mat)

# Or in ggplot 
library(tidyverse)
res1 <- tibble::tibble(
  S1 = S[,1],
  S2 = S[,2],
  R1 = R[,1],
  R2 = R[,2],
  lambda = lambda
)
# tst <- res1 %>%
#   pivot_longer(1:4, names_to = "vitalrate", values_to = "RV") %>%
#   group_by(vitalrate) %>%
#   do(mod = lm(lambda ~ RV, data = .))
# ggplotRegression <- function (fit) {
#   ggplot(fit$model, aes_string(x = names(fit$model)[2],
#                                y = names(fit$model)[1])) +
#     geom_point() +
#     stat_smooth(method = "lm", col = "red") +
#     labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
#                        "Intercept =",signif(fit$coef[[1]],5 ),
#                        " Slope =",signif(fit$coef[[2]], 5),
#                        " P =",signif(summary(fit)$coef[2,4], 5))) +
#     theme_classic()
# }
# ggplotRegression(r1)



# For fun ####
# Simulate a different value each year 
# this is probably unreasonable because the distribution is temporal AND spatial
#  variation, and a particular population would only experience temporal variation

N0 <- matrix(c(100, 100), ncol = 1)

# Multiple time steps
project <- function(N0, matr, nyears){
  N.projected <- matrix(0, nrow = nrow(matr), ncol = nyears+1)
  N.projected[, 1] <- N0

  for (i in 1:nyears){
    N.projected[, i + 1] <- matr %*% N.projected[,i]
  }
  return(N.projected)
}


# # Just lambda ####
# nrep <- 1000
# lambda <- rep(NA, nrep)
# for(i in 1:nrep){
#   S <- rnorm(2, Smu, Ssd)
#   R <- rnorm(2, Rmu, Rsd)
#   mat <- matrix(
#     c(
#       S[1]*R[1], S[2]*R[2],
#       S[1],    S[2]
#     ),
#     nrow = 2,
#     byrow = TRUE
#   )
#   lambda[i] <- eigen(mat)$values[1]
# }
# 
# # Visualize 
# hist(lambda)


# # Verify with simulation 
# N0 <- matrix(c(100, 100), ncol = 1)
# 
# # Multiple time steps
# project <- function(N0, matr, nyears){
#   N.projected <- matrix(0, nrow = nrow(matr), ncol = nyears+1)
#   N.projected[, 1] <- N0
#   
#   for (i in 1:nyears){
#     N.projected[, i + 1] <- matr %*% N.projected[,i]
#   }
#   return(N.projected)
# }
# 
# years <- 100
# N <- project(N0, mat, years)
# # Sum up by year
# Ntot <- apply(N, 2, sum)
# lam <- Ntot[2:(years+1)] / Ntot[1:years]
# mlam <- mean(lam[10:years])
# mlam
# 
# # Plot
# plot(1:(years+1), Ntot, type = "l")

