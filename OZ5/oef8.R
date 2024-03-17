# OEFENING 8
rm(list=ls())

load("~/Documents/Code/KULAK_statistiek/OZ5/cars.RData")

attach(cars)

# LENGTH NORMAAL

# dichtheidshistrogram met freq=FALSE
hist(length, freq=FALSE)

summary(length)
mu = mean(length)
sd = sd(length)

x = seq(3.5,5.563,length=100)
lines(x,dnorm(x,mu,sd),type="l",lwd=1,col="red")

# MAXPRICE LOGNORMAAL

x = seq(0,40,length=1000)
hist(log(maxpr), freq=FALSE, breaks=12)
mu=mean(log(maxpr))
sd=sd(log(maxpr))
lines(x,dnorm(x,mu,sd),type="l",lwd=1,col="red")

# OPTIES EXPONENTIEEL

hist(opties, breaks=12)
# voor de lognormaal zetten we logx op de x-as