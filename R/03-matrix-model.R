# Turkey matrix model 
# Anna Moeller 
# 9/3/2021

# Making up values for now
Sp <- 0.3
Sy <- 0.4
Sa <- 0.6
Rp <- 0.1 * 2
Ry <- 0.4 * 4
Ra <- 0.6 * 6

mat <- matrix(
  c(
    Sp*Rp, Sy*Ry, Sa*Ra,
    Sp, 0, 0, 
    0, Sy, Sa
  ),
  nrow = 3,
  byrow = TRUE
)

N0 <- matrix(c(100, 250, 50), ncol = 1)

# Multiple time steps
project <- function(N0, matr, nyears){
  N.projected <- matrix(0, nrow = nrow(matr), ncol = nyears+1)
  N.projected[, 1] <- N0
  
  for (i in 1:nyears){
    N.projected[, i + 1] <- matr %*% N.projected[,i]
  }
  return(N.projected)
}

years <- 100
N <- project(N0, mat, years)
# Sum up by year
Ntot <- apply(N, 2, sum)
lam <- Ntot[2:(years+1)] / Ntot[1:years]
mlam <- mean(lam[10:years])
mlam

# Plot 
plot(1:(years+1), Ntot, type = "l")
