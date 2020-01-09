set.seed(1)
nsim <- 100000

# data
m1 <- 2500 # counterparties tranche A
m2 <- 5000 # counterparties tranche B
m3 <- 2500 # counterparties tranche C

p1 <- 0.0004 # prob of default 
rho1 <- 0.0004 # correlation within the tranche

p2 <- 0.0097 
rho2 <- 0.0044

p3 <- 0.0503  
rho3 <- 0.01328

# exposures
e1 <- 80
e2 <- 25 
e3 <- 10 

# loss given default
LGD1 <- 0.25
LGD2 <- 0.375
LGD3 <- 0.5

# beta-binomial model with copula 

# beta parameters: matching tranches default probabilities and correlation
alpha1 <- p1 * (1 / rho1 - 1)
beta1 <- alpha1 * (1 / p1 - 1)

alpha2 <- p2 * (1 / rho2 - 1)
beta2 <- alpha2 * (1 / p2 - 1)

alpha3 <- p3 * (1 / rho3 - 1)
beta3 <- alpha3 * (1 / p3 - 1)

# correlations between sub-portfolios
cor12 <- 0.3
cor13 <- 0.1
cor23 <- 0.4

# Gaussian copula structure
myCop <- copula::normalCopula(param = c(cor12, cor13, cor23), dim = 3, dispstr = "un")

# define multivariate beta with given copula
myMvd <- copula::mvdc(copula = myCop,
              margins = c("beta", "beta", "beta"),
              paramMargins = list(list(alpha1, beta1),
                                  list(alpha2, beta2),
                                  list(alpha3, beta3)))

# simulation from the chosen copula
H <- copula::rMvdc(nsim, myMvd)

# simulate number of default per tranches (binomial distributions)
M1 <- rbinom(n = nsim, size = m1, prob = H[, 1])
M2 <- rbinom(n = nsim, size = m2, prob = H[, 2])
M3 <- rbinom(n = nsim, size = m3, prob = H[, 3])

# total loss per sub-portfolio
L1 <- M1 * e1 * LGD1
L2 <- M2 * e2 * LGD2
L3 <- M3 * e3 * LGD3

# aggregate portfolio loss
L <- L1 + L2 + L3

# DB for SWIM
credit_data <- cbind(L, L1, L2, L3, H)
colnames(credit_data) <- c("L", "L1", "L2", "L3", "H1", "H2", "H3")
