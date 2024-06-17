rm(list=ls())

setwd("~/Documents/Code/KULAK_statistiek/OZ11/")

load("cars.RData")

attach(cars)

# minpr moet in data.frame want anders miserie
car_cont <- data.frame(minpr,engines,horsepo,RPM,revolu,length,wheelbs,width,weight,city,hghwy)



#######
## 1 ##   correlatie
#######

cor(minpr,car_cont)
# we verwachten dat minstens enkele van de veranderlijken in het model
# gaan verschijnen
# alle veranderlijken meten in zekere maten 'hetzelfde' -> multicollineariteit

cor(car_cont)



#######
## 2 ##   achterwaardse regressie
#######

model = lm(minpr~., data=car_cont)

summary(model)

# hghwy is het minst significant
model = update(model, .~.-hghwy); summary(model);
# city en hghwy zijn quasi perfect gecorreleerd!
# allebei veel correlatie hierboven, maar dus eig hetz.

# weight is minst significant
model = update(model, .~.-weight); summary(model);

# RPM is minst significant
model = update(model, .~.-RPM); summary(model);

# wheelbs is minst significant
model = update(model, .~.-wheelbs); summary(model);

# revolu is minst significant
model = update(model, .~.-revolu); summary(model);

# engines is minst significant
model = update(model, .~.-engines); summary(model);

# We vinden dus als significante variabelen:
#   horsepo
#   length
#   width
#   city
# Repeated testing: hou u aan de 5%!


#######
## 3 ##   modelveronderstellingen nagaan
#######

par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

# links boven:
#   model onderschat eerst systematisch
#   daarna overschat het systematisch
#   daarna onderschat het terug systematisch
#     --> vorm is niet goed!
# links onder:
#   variatie is niet constant

# Dit kan te wijten zijn aan de scheefheid van minpr:
hist(minpr)
# Log transformatie
hist(log10(minpr))

# Update het model zodat er log10 wordt gebruikt
logmodel = update(model, log10(minpr)~.)
summary(logmodel)
# --> veel significantere waardes

par(mfrow=c(2,2))
plot(logmodel)
par(mfrow=c(1,1))

# Variatie is constant en normaliteit is in orde
# Nog een kleine onderschatting bij zeer kleine auto's
# Maar dat is eigenlijk al wat extrapoleren

# log10_p = ...
# p = 10^... + 10^... + ...
#   = ...

coef(model)
#       !!
10**(coef(logmodel)) # !!
#       !!



#######
## 4 ##   residuen tov de verschillende regressoren
#######

logmodel$model
attributes(logmodel)
?lm
# Regressoren:
#   horsepo   kolom 2
#   length    kolom 3
#   width     kolom 4
#   city      kolom 5

x_horsepo = logmodel$model[,2]
x_length = logmodel$model[,3]
x_width = logmodel$model[,4]
x_city = logmodel$model[,5]

residuen = logmodel$residuals

par(mfrow=c(2,2))
plot(x_horsepo, residuen); abline(h=0, col='red');
plot(x_length, residuen); abline(h=0, col='red');
plot(x_width, residuen); abline(h=0, col='red');
plot(x_city, residuen); abline(h=0, col='red');
par(mfrow=c(1,1))
# --> We zien geen regressoren die we kunnen kwadrateren
#     om het model te verbeteren! We kunnen dus niet verder

## OF AUTOMATISCH
par(mfrow=c(2,2))
# !!
termplot(logmodel,partial.resid=TRUE) # !!
# !!
par(mfrow=c(1,1))

# ONDERSTAANDE HEEFT EIG GEEN NUT MAAR IS VOOR SYNTAX
# !! machtsverheffing forceren --> I !!
summary(update(logmodel, .~.+I(length^2)))
summary(update(logmodel, .~.+I(width^2)))
summary(update(logmodel, .~.+I(city^2)))
summary(update(logmodel, .~.+I(horsepo^2)))
# --> net significant maar geen reden om het te houden want
#     we blijven maar proberen en proberen tot we iets significant krijgen


#######
## 5 ##   zijn er waarden die het model verstoren?
#######

par(mfrow=c(2,2))
plot(logmodel)
par(mfrow=c(1,1))

summary(logmodel)

# punten 58, 59 en 28 zijn uitschieters
cars[c(58,59),]   # de mercedes zijn veel duurder en we kunnen niet verklaren waarom
                  #   maar ze zijn eigenlijk helemaal niet zo afwijkend
cars[28,]         # de dodge ligt ver buiten de puntenwolk en heeft een even hoog residu als de mercedessen
                  #   maar veel meer leverage!

outmodel = update(logmodel, .~., data=car_cont[-28,])

# we zien geen effect! (kijk naar standaardfout!)
coef(logmodel)
coef(outmodel)



#######
## 6 ##   verschilt prijs van importwagens van die van Amerikaanse?
#######

model6 = update(model, .~.*domesti)
# !!!!
#   maalteken:  main effect en interactie
#   dubbelpunt: enkel interactie
# !!!!

summary(model6)

# !!
#   Hou de indicator steeds in het model zolang er interactietermen inzitten
#   ook al is de significantie van het hoofdeffect het kleinst!
# !!

model6 = update(model6, .~.-width:domesti); summary(model6);
model6 = update(model6, .~.-city:domesti); summary(model6);
model6 = update(model6, .~.-length:domesti); summary(model6);
# horsepower blijft staan
# extreem voorzichtige suggestie dat de prijs van importwagens
# en die van Amerikaanse productie verschilt

summary(model6)

# DUS
#   log10P_imp = ...
#   log10P_US = ...

# MAAR DAT IS VOOR DE LOG10 --> kijk naar de coef van gwn P
10**coef(model6)
# !! TOT DE MACHT en MAAL !!
#   P_imp = 2.9 + ...^HP + ...^... !!
# !!


detach(cars)
