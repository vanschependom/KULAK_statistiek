## Vraag 1: Binomiaal/Poisson/Normaal
# http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
# Binomiaal
n = 250000 # Aantal vliegbewegingen in Zaventem in een jaar
p = 1/1000000 # Kans op ongeval
mu = n*p # Verwachte waarde van het aantal ongevallen in Zaventem in een jaar.
sigma = sqrt(n*p*(1-p)) # Standaardafwijking voor de binomiale verdeling
dbinom(2,n,p) # Kans op exact 2 ongevallen in een jaar. 
# d staat steeds voor dichtheidsfunctie (dbinom, dpois, dnorm ...)

# Poissonbenadering
lambda = n*p # Parameter lambda voor Poissonverdeling (EX Poisson = lambda)
dpois(2,lambda) # Kans op exact 2 ongevallen in een jaar voor Poissonbenadering.

# Normale benadering
pnorm(2.5,mu,sigma)-pnorm(1.5,mu,sigma) # Continu?teitscorrectie!
# Breedte van een balkje is 1, dus er 
# wordt 0.5 links en rechts van middenwaarde (2.0) gekeken bij de continue benadering
# (Dit is de derde formule van 5.12 op p242 met a=b)
n*p
n*(1-p)
# De binomiale verdeling is exact,
# Omdat n groot is en p klein, is Poisson 
# een zeer goede benadering (n>100, np<5)
# Omdat np niet groter is dan 5 (p240), is 
# de normale benadering niet goed. (n(1-p) is wel groter dan 5)

# Grafische vergelijking
x1 = 0:5
y1 = dbinom(x1,n,p) 
plot(x1,y1,pch=16,col='blue') # Binomaal in blauwe stippen
y2 = dpois(x1,lambda)
points(x1,y2,pch=0,col='red')  # Poisson in rode vierkanten
x2 = seq(0,5,by=0.01) 
y3 = dnorm(x2,mu,sigma) 
lines(x2,y3,col='green') # Normaal in groene lijn (is continu)
abline(v=2,col='pink',lwd=2) # We willen kans op 2 ongevallen kennen

## Vraag 2: Binomiaal/Poisson/Normaal

# Binomiaal
# s.v. X = aantal personen in steekproef van 1000 die voor Obama kiezen
# X ~ B(n,p)
n = 1000 # Duizend kiezers
p = .51 # Kans dat kiezer op Obama stemt
mu = n*p # Verwacht aantal Obamastemmers
sigma = sqrt(n*p*(1-p)) # Standaardafwijking voor de binomiale verdeling
pbinom(499,n,p) # Kans op <500 Obamastemmers
# p staat steeds voor verdelingsfunctie (pbinom, ppois, pnorm ...)

# Poissonbenadering
lambda = n*p # Parameter lambda voor Poissonverdeling (EX Poisson = lambda)
ppois(499,lambda) # Kans op m<500 Obamastemmers bij Poissonbenadering

# Normale benadering
pnorm(499.5,mu,sigma) # Kans op <500 Obamastemmers bij normale benadering
# Opnieuw continu?teitscorrectie (Tweede formule van 5.12 op p242)
n*p
n*(1-p)
# De binomiale verdeling is exact,
# Omdat p groot is, is Poisson geen goede benadering (np is niet kleiner dan 5)
# Omdat zowel np als n(1-p) veel groter zijn dan 5, 
# is de normale verdeling een zeer goede benadering.

# Grafische vergelijking
x1 = seq(450,550,by=1)
y1 = pbinom(x1,n,p) 
plot(x1,y1,pch=16,col='blue') # Binomaal in blauwe stippen
y2 = ppois(x1,lambda)
points(x1,y2,pch=0,col='red')  # Poisson in rode vierkanten
x2 = seq(450,550,by=0.01) 
y3 = pnorm(x2,mu,sigma) 
lines(x2,y3,col='green') # Normaal in groene lijn (is continu)
abline(v=499,col='pink',lwd=4) # We willen kans op minder dan 500 stemmers kennen


## Vraag 3: Normale verdeling
# X = loon ~ N(2837,sigma^2) => (loon-2837)/sigma ~ N(0,1)
# P(X < 1807) = F(1807) = 0.1 => PHI((1807-2837)/sigma) = 0.1
# Noem q = PHI^(-1)(0.1), dan
# (1807-2837)/sigma = q => sigma = (1807-2837)/q

mu=2837 # Verwachte waarde
q = qnorm(.1) # We bepalen PHI^(-1)(0.1)
sigma = (1807-mu)/q # Hieruit kunnen we sigma halen
# Controle
pnorm(1807,mu,sigma) # PHI(1807-mu)/sigma moet gelijk zijn aan 0.1
# Kwartielen en interkwartielafstand
qnorm(.75,mu,sigma)-qnorm(.25,mu,sigma) # Interkwartielafstand
# q staat steeds voor kwantielfunctie (qbinom, qpois, qnorm ...)
qnorm(.9,mu,sigma) # Loon zodat maar 10% meer verdient
# Of, omdat de normale verdeling symmetrisch is,
# is het loonsverschil tussen gemiddelde en hoogste 10% gelijk 
# aan dat tussen gemiddelde en laagste 10% (en dat kennen we uit gegeven)
2837+(2837-1807) # Gemiddelde + loonsverschil tussen gemiddelde en minste 10%

## Vraag 4: Exponenti?le verdeling
# s.v. X = aantal minuten vertraging
# P(X<6) = 88.9% met P(X<x) = 1-exp(-lambda*x)
# 0.889 = 1-exp(-lambda*6) => lambda = -log(1-0.889)/6
lambda = -log(1-0.889)/6
# Controle:
pexp(6,lambda) # F(6) moet gelijk zijn aan 0.889 voor deze lambda
# Mediaan
qexp(.5,lambda) # Mediaan is 0.5 kwantiel
# Meer dan 15': P(X>15)=1-F(15)
p = 1-pexp(15,lambda) # Kans op meer dan 15min vertraging
# s.v. Y = aantal keer meer dan 15' tijdens half jaar ~ bin(250,p)
# P(Y>=20)
1-pbinom(19,250,p) # Kans op 20 of meer vertragingen = P(X>=20)=1-F(19)

## Vraag 5: Populatieparameters berekenen
# Dobbelsteen
# Somcommando!
EX = sum(1:6)/6 # Verwachte waarde
VarX = sum((1:6-EX)^2/6) # Variantie

# Aantal kinderen
x = 0:4 # Aantal kinderen
f = c(.46,.21,.22,.08,.03) # Kans op dit aantal kinderen
EX = sum(x*f) # Herinner: * is elementsgewijs product
VarX = sum((0:4-EX)^2*f)

## Vraag 6: Grafieken van binomiale verdelingen
x = 0:100
f02 = dbinom(x,100,.02)
f04 = dbinom(x,100,.04)
f08 = dbinom(x,100,.08)
f16 = dbinom(x,100,.16)
f32 = dbinom(x,100,.32)
f50 = dbinom(x,100,.5)
f64 = dbinom(x,100,.64)
f96 = dbinom(x,100,.96)
plot(x,f02,type="h",col="red")
points(x,f04,type="h",col="orange")
points(x,f08,type="h",col="yellow")
points(x,f16,type="h",col="green")
points(x,f32,type="h",col="blue")
points(x,f50,type="h",col="pink")
points(x,f64,type="h",col="purple")
points(x,f96,type="h",col="brown")
# Duidelijk dat normale benadering beter is naargelang p dichter bij 0.5 ligt.

## Vraag 7: De geometrische verdeling
# Hoeveel pogingen verwachten we tot het eerste succes?
x = 1:100
f02 = dgeom(x,.02)
f04 = dgeom(x,.04)
f08 = dgeom(x,.08)
f16 = dgeom(x,.16)
f32 = dgeom(x,.32)
f64 = dgeom(x,.64)
plot(x,f64,type="h",col="red")
points(x,f32,type="h",col="orange")
points(x,f16,type="h",col="yellow")
points(x,f08,type="h",col="green")
points(x,f04,type="h",col="blue")
points(x,f02,type="h",col="purple")
# Hoe groter de succeskans, hoe minder pogingen we verwachten nodig te hebben
# Deze verdelingen zijn allen rechtsscheef
x=seq(0.01,0.99,by=0.01)
medgeom=qgeom(.5,x) # We bepalen de mediaan voor varierende slaagkans
plot(x,medgeom,type="s",col="red") # We plotten de medianen
lines(x,1/x,col="blue") # We plotten de gemiddelden ook
# De grote waarden trekken het gemiddelde naar omhoog,
# De mediaan is dus telkens kleiner dan het gemiddelde, 
# wat we verwachten voor rechtsscheve verdelingen

## Vraag 8: Vorm van verdelingen onderzoeken
load("cars.RData")
attach(cars)

# Lengte van de wagens
X = cars$length
hist(X,freq=F,breaks=12) # Plot histogram
?hist
mu=mean(X) # Bepaal gemiddelde van data
sigma=sd(X) # Bepaal standaardafwijking van data
x=seq(min(X),max(X),length=100)
lines(x,dnorm(x,mu,sigma),col='red') # Plot normale verdeling met zelfde mu en sigma
# Verdeling ziet er globaal ongeveer symmetrisch uit en
# wijkt op het zicht niet al te zeer erg af van de normale verdeling,
# op een piek rond 450cm na
# Kwantielplot (later meer!)
qqnorm(X) # Plot normale kwantielplot
qqline(X,col='red') # Plot er beste rechte door
# Lijkt een linear verband te zijn
shapiro.test(X)
# Test bevestigt normaliteit (p-waarde 0.78)


# Maximumprijs van de wagens
X = cars$maxpr
hist(X,freq=F,breaks=12) # Plot histogram
mu=mean(X) # Bepaal gemiddelde van data
sigma=sd(X) # Bepaal standaardafwijking van data
x=seq(min(X),max(X),length=100)
lines(x,dnorm(x,mu,sigma),col='red') # Plot normale verdeling met zelfde mu en sigma
lines(x,dexp(x,1/mu),col='blue') # Plot exp. verdeling met lamda=1/mu (EX(EXP)=1/lambda)
# Verdeling is sterk rechtsscheef, dus is niet normaal
# Maar de piek ligt niet t.h.v. het minimum, dus is evenmin exponentieel

X = log10(cars$maxpr) # Transformeer x logaritmisch
hist(X,freq=F,breaks=12)
mu=mean(X) # Bepaal gemiddelde van getransformeerde data
sigma=sd(X) # Bepaal standaardafwijking van getransformeerde data
x=seq(min(X),max(X),length=100)
lines(x,dnorm(x,mu,sigma),col='red') # Plot normale verdeling voor nieuwe mu en sigma
# Na logaritmische transformatie lijkt de verdeling beter te voldoen aan normaliteit

# Kwantielplot (later meer!)
qqnorm(X) # Plot normale kwantielplot
qqline(X,col='red') # Plot er beste rechte door 
# Lijkt een linear verband te zijn
shapiro.test(X)
# Test bevestigt lognormaliteit (p-waarde 0.388)!


# Opties
X = cars$opties
hist(X,freq=F,breaks=12) # Plot histogram
mu=mean(X) # Bepaal gemiddelde van data
sigma=sd(X) # Bepaal standaardafwijking van data
x=seq(min(X),max(X),length=100)
lines(x,dnorm(x,mu,sigma),col='red') # Plot normale verdeling met zelfde mu en sigma
lines(x,dexp(x,1/mu),col='blue') # Plot exp. verdeling met lamda=1/mu (EX(EXP)=1/lambda)
# Verdeling is volledig rechtsscheef, geen linkerstaart, dus zeker niet normaal
# De piek ligt t.h.v. het minimum, dus mogelijks exponentieel

# Kwantielplot (p.201 -- later meer)
n = length(X)
Q = -log(1-((1:n)-1/2)/n) # Bepaal theoretische kwantielen van exponentiele verdeling
plot(Q,sort(X)) # Plot data ten opzichte van theoretische kwantielen
abline(lm(sort(X)~Q),col='red') # Plot er beste rechte door
# Haal de outlier eruit (Dikke Mercedes)
Q = -log(1-((1:(n-1))-1/2)/(n-1))
abline(lm(sort(X[X!=max(X)])~Q),col='blue') # Plot opnieuw beste rechte
# Op de outlier na volgt de data inderdaad redelijk goed de exponentiele verdeling
