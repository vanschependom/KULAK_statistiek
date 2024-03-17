# OEFENING 2
rm(list=ls())


# X1 = aantal personen die voor Obama kiezen
n = 1000
p = 0.51
# X1 ~ bin(n,p)

# kans dat minder dan de helft voor Obama stemt
pbinom(499,n,p)



# benadering met Poisson
n*p*(1*p) # groter dan 5

# X2 ~ pois(lambda)
lambda = n*p

ppois(499,lambda) # slechte benadering



# benadering met normale verdeling
mu = n*p
sd = sqrt( n*p*(1-p) )

# X3 ~ N(mu,sd^2)
pnorm(499.5,mu,sd)  # komma 5 !!
# de binomiale is een 'staaf' dus we doen een continuiteitscorrectie !!


x = seq(mu-3*sd,mu+3*sd,length=200)
xd = round(mu-3*sd):round(mu+3*sd)
plot(xd,dbinom(xd,n,p),type="h")
lines(xd+1, dpois(xd,lambda), type="h", col="green")
lines(x,dnorm(x,mu,sd),type="l",col="red")