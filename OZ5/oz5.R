# DISCRETE VERDELINGEN

dbinom(1,10,1/6)
# kans (dichtheid) om 0:10 keer een 6 te gooien
dbinom(0:10,10,1/6)

x = 0:10
y = dbinom(x,10,1/6)
# histogram van de binomiale dichtheid met p=1/6
plot(x,y,type="h")

# cumulatieve kansen
pbinom(1,10,1/6)
dbinom(1,10,1/6)+dbinom(0,10,1/6)

y = pbinom(x,10,1/6)
# trapfunctie van de cumulatieve verdeling
plot(x,y,type="s")

# derde kwartiel
qbinom(.75, 10, 1/6)

# makkelijkste manier om kwantielfunctie te plotten is (y,x) plotten ipv (x,y)
# want het is ook een trapfunctie met sprongetjes op rare momenten
plot(y,x,type="s")

# ---------------------- #
# CONTINUE VERDELINGEN
# ! We werken met standaarddeviaties en dus niet variantie !

dnorm(2,1.75,0.1) # !! GEEN KANS !! want P(X=x)=0 !!
# we gebruiken dnorm enkel voor de Guasscurve te tekenen

x = seq(1.45,2.05,length=200) # 3 s.a. aan elke kant
y = dnorm(x,1.75,0.1)
plot(x,y,type="l") # l van lijn want anders puntenplot
# om een nieuwe plot toe te voegen aan een bestaande grafiek gebruiken we lines
lines(x,dnorm(x,1.65,0.1),col="red",type="l")
lines(x,dnorm(x,1.75,0.15),col="green",type="l")
