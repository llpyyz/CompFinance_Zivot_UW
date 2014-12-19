##################################################
#David Schonberger
#Computational Finance and Financial Econometrics
#Coursera/UW, Eric Zivot instructor
#Lab 2: Random Variables and Probability Distributions
#Completed: 12/13/2014
##################################################

# X ~ N(0.05, (0.10)^2)
mu_x = .05
sigma_x = .1

# Pr(X > 0.10)
1 - pnorm(.1,mu_x, sigma_x)

# Pr(X < -0.10)
pnorm(-.1,mu_x, sigma_x)

# Pr(-0.05 < X < 0.15)
pnorm(.15, mu_x, sigma_x) - pnorm(-.05,mu_x, sigma_x)

###

# 1%, 5%, 95% and 99% quantile
qnorm(c(.01, .05, .95, .99), mu_x, sigma_x)

###

# Normally distributed monthly returns
x_vals = seq(-0.25, 0.35, length.out = 100)
MSFT = dnorm(x_vals, .05, .1)
SBUX = dnorm(x_vals, .025, .05)


###

# Normal curve for MSFT
plot(x_vals, MSFT, type = "l", col = "blue", ylab = "Normal curves", 
     ylim = c(0, 8))
# Add a normal curve for SBUX
lines(x_vals, SBUX, col = "red")

# Add a plot legend
legend("topleft", legend = c("Microsoft", "Starbucks"), 
       col = c("blue", "red"), lty = 1)

###
	   
# R ~ N(0.04, (0.09)^2) 
mu_R = .04
sigma_R = .09
# Initial wealth W0 = $100,000
W0 = 100000
# The 1% value-at-risk
W0 * (mu_R + sigma_R * qnorm(.01))
# The 5% value-at-risk
W0 * (mu_R + sigma_R * qnorm(.05))

###

# r ~ N(0.04, (0.09)^2) 
mu_r = .04
sigma_r = .09
# Initial wealth W0 = $100,000
W0 = 100000
# The 1% value-at-risk
W0 *(exp(mu_r + sigma_r * qnorm(.01)) - 1)
# The 5% value-at-risk
W0 *(exp(mu_r + sigma_r * qnorm(.05)) - 1)

###

# Vectors of prices
PA = c(38.23, 41.29)
PC = c(41.11, 41.74)

# Simple monthly returns
RA = PA[2]/PA[1] - 1
RC = PC[2]/PC[1] - 1

###

# Continuously compounded returns
rA = log(1 + RA)
rC = log(1 + RC)

###
# Cash dividend per share
DA = 0.1
# Simple total return
RA_total = (PA[2] + DA - PA[1])/PA[1]
# Dividend yield
DY = DA / PA[1]

###

# Simple annual return
RA_annual = (1 + RA)^12 - 1
# Continuously compounded annual return
rA_annual = 12 * rA
rA_annual = 12 * log(1 + RA)

###

