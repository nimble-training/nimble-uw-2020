set.seed(1)                   # So we all get same data set
M <- 100                      # Number of sites
J <- 3                        # Number of presence/absence measurements
y <- matrix(NA, nrow = M, ncol = J) # to contain the obs. data

# Create a covariate called vegHt
vegHt <- sort(runif(M, -1, 1)) # sort for graphical convenience

# Choose parameter values for occupancy model and compute occupancy
beta0 <- 0                    # Logit-scale intercept
beta1 <- 3                    # Logit-scale slope for vegHt
psi <- plogis(beta0 + beta1 * vegHt) # Occupancy probability
# plot(vegHt, psi, ylim = c(0,1), type = "l", lwd = 3) # Plot psi relationship

# Now visit each site and observe presence/absence perfectly
z <- rbinom(M, 1, psi)        # True presence/absence

wind <- array(runif(M * J, -1, 1), dim = c(M, J))

# Choose parameter values for measurement error model and compute detectability
alpha0 <- -2                        # Logit-scale intercept
alpha1 <- -3                        # Logit-scale slope for wind
p <- plogis(alpha0 + alpha1 * wind) # Detection probability
# plot(p ~ wind, ylim = c(0,1))     # Look at relationship

# Take J = 3 presence/absence measurements at each site
for(j in 1:J) {
  y[,j] <- rbinom(M, z, p[,j])
}

time <- matrix(rep(as.character(1:J), M), ncol = J, byrow = TRUE)
hab <- c(rep("A", 33), rep("B", 33), rep("C", 34))  # Must have M = 100

# Bundle and summarize data set
occupancy_data <- list(y = y, 
                            vegHt = vegHt,
                            wind = wind,
                            M = nrow(y),
                            J = ncol(y),
                            XvegHt = seq(-1, 1, length.out=100),
                            Xwind = seq(-1, 1, length.out=100)) 

# Initial values: best to give for same quantities as priors given !
zst <- apply(y, 1, max)        # Avoid data/model/inits conflict
occupancy_inits <- function(){
  list(z = zst, 
       mean.p = runif(1), 
       alpha1 = runif(1), 
       mean.psi = runif(1), 
       beta1 = runif(1))
}

inits_saved <- occupancy_inits()

occ_code <- nimbleCode({
  # Priors
  mean.p ~ dunif(0, 1)         # Detection intercept on prob. scale
  alpha0 <- logit(mean.p)      # Detection intercept
  alpha1 ~ dunif(-20, 20)      # Detection slope on wind
  mean.psi ~ dunif(0, 1)       # Occupancy intercept on prob. scale
  beta0 <- logit(mean.psi)     # Occupancy intercept
  beta1 ~ dunif(-20, 20)       # Occupancy slope on vegHt
  
  # Likelihood
  for (i in 1:M) {
    # True state model for the partially observed true state
    z[i] ~ dbern(psi[i])      # True occupancy z at site i
    logit(psi[i]) <- beta0 + beta1 * vegHt[i]
    for (j in 1:J) {
      # Observation model for the actual observations
      y[i,j] ~ dbern(p.eff[i,j])    # Detection-nondetection at i and j
      p.eff[i,j] <- z[i] * p[i,j]   # 'straw man' for WinBUGS
      logit(p[i,j]) <- alpha0 + alpha1 * wind[i,j]
    }
  }
}
)

occ_model <- nimbleModel(occ_code,
                        constants = occupancy_data,
                        inits = occupancy_inits())
