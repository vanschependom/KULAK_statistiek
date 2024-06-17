# OEFENING 1
rm(list=ls())


?seq
x = seq(0,5,length=200)
xd = 0:5


# 1 / 1 000 000 vliegtuigen geeft probleem
p = 1 / 1000000
# 250 000 vliegtuigen per jaar
n = 250000

# X = aantal vliegtuigongevallen per jaar
# S_X = {0,..., 250 000}

# X ~ bin(250 000, 1 / 1 000 000)
dbinom(2, n, p)
plot(xd,dbinom(xd,n,p),type="h")



# Benadering met poisson ( np>5 en n>100 )

# S_X2 = N

# lambda is de verwachtingswaarde
lambda = n*p

# X2 ~ pois(np) = pois(0.25)
dpois(2, lambda)

lines(xd+0.05, dpois(xd,lambda), type="h", col="green")



# Benadering met normale verdeling

# S_X3 = R

# X3 ~ N(mu, sigma^2)
# E(X3) = np = mu = 0.25
mu = n*p
# sqrt(Var(X2)) = 0.5 = sigma
sd = sqrt( n*p*(1-p) )

lines(x,dnorm(x,mu,sd),type="l",col="red")

# we integreren van 1.5 tot 2.5
pnorm(2.5,mu,sd) - pnorm(1.5,mu,sd)

# 0 benaderen van -oneindig tot 0.5
# 250 000 benaderen van 249999.5 tot plus oneindig
# de kansen in de staarten zijn zodanig klein

