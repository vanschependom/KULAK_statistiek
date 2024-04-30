load("pollutie-gegevens.RData")

attach(pollutie)

#### {Model opstellen}
model1 = lm(income~Education)
# Linear model
# We maken een regressiemodel van inkomen in functie van education
model1

plot(income~Education)       
abline(model1, col='red')      # regressielijn
# !! 'gemiddeld' !!
# Per jaar education gaat het inkomen met GEMIDDELD 2668 dollar naar omhoog

# Intercept is niet zo belangrijk:
# Wiskundige betekenis maar de interpretatie van het getal is vrij zinloos
# Er wordt ge-extrapoleerd: 0 is veeeeel kleiner dan 9 (minimum education)

summary(model1)
# 'intercept had evengoed nul kunnen zijn'
# Als intercept 0 is, is de kans dat we een intercept zien die meer dan 4000
# afwijkt van 0, 50%.

# Veranderlijken toevoegen zorgt ervoor dat de R^2 omhoog gaat
# --> kijk bij meervoudige regressie dus altijd naar ADJUSTED R^2

class(model1)
attributes(model1)

#### {Modelveronderstellingen}
head(model1$model)
# De gebruikte data voor het model, MAAR:
# 1. alleen de gebruikte veranderlijken
# 2. geen NA's
residuen = model1$residuals; residuen
y_hat = model1$fitted.values; y_hat
x_i = model1$model[,2]
y_i = model1$model[,1]

# De pars dienen om alle 4 de grafieken op 1 scherm te krijgen
par(mfrow=c(2,2))
plot(model1) # anders dan cursus!
par(mfrow=c(1,1))

# Bij meervoudige regressie kunnen we niet gewoon plot(education, income)
# bekijken want we zitten in hogere dimensies!

# Gauss-Markov voorwaarden:
# Residuen normaal verdeeld met gemiddelde 0 en constante variantie
# 1.
# Door de KKM zijn de gemiddelden sowieso globaal 0, MAAR
# we willen dat ter hoogte van elke x_i het gemiddelde nul is!
# Probleem: maar 1 observatie per x_i
# -> globaal: "in de buurt van x_i ongeveer gemiddeld nul"
# 2.
# Variantie is sws constant door KKM, maar we willen dat opnieuw, analoog
# voor elke x_i

# EERSTE GRAFIEK
# (y_i^, epsilon_i) -> 2D want y_i^ ipv meerdere x_i!
# Check of gemiddeld 0
# Eerst systematisch onderschat, dan systematisch overschat, dan goed
# Niet perfect  --> regressielijn niet proper door punten
#               --> vorm niet voldaan

# DERDE GRAFIEK (onder eerste)
# Zijn varianties in orde?
# We nemen de wortel van de absolute waarde
# Wortel niet belangrijk, absolute waarde wel
# --> we kijken enkel naar de grootte-orde van de afwijking 
# Quasi perfect

# TWEEEDE GRAFIEK (rechts van eerste)
# qqplot van de residuen
# Individuele afwijkingen (rechts) niet naar kijken (indvd. punten)
# In de linkerstaart meer afwijking
# Maar niet significant hier
# Goede qqplot: punten heel de tijd boven/onder regressielijn -> check!

#### {Opsporen van uitschieters}
#### {Voorspellingen en voorspellingsintervallen}
# Deze stukken uit de handleiding komen later aan bod

#### {Meervoudige regressie}
loon = lm(income~Education+Mortality+pop+pop.house)
summary(loon)
loon = update(loon, .~.-pop.house)
# We doen ACHTERWAARDSE REGRESSIE
# Eerst de minst significante variabele verwijderen
# Met update(model, ...)
# Hetgeen voor de tilde staat wordt niet aangepast
# Wat na de tilde staat wordt pop.house van afgetrokken

summary(loon)
# Door multicollineariteit veranderen de coefficienten!
loon = update(loon, .~.-Mortality)
summary(loon)
loon$model

#### {Transformaties van veranderlijken}
par(mfrow=c(2,2))
plot(loon)
par(mfrow=c(1,1))
# Modelvorm lijkt nog altijd niet correct
# Soms kan je dat gewoon niet oplossen
# ->  we concluderen dat er een significant effect is
#     maar we kunnen de vorm niet exact bepalen

# individuele regressoren op de x-as ipv de voorspelling
residuen = loon$residuals
# Partiele residualplot
x_pop = loon$model[,3]
# zeer scheve verdeling bij populatie
# --> scheve verdeling GEEN GOED IDEE!
#     want veel te veel impact!!
plot(x_pop, residuen); abline(h=0, col='red')     

par(mfrow=c(1,2))
boxplot(pop)
boxplot(log10(pop))
# Kies het 10-delig logaritme!!
par(mfrow=c(1,1))

logloon = update(loon, .~.-pop+log10(pop))
# Adj R squared is beetje verhoogd
# Significantie is beetje beter
# We willen niet overfitten om AdjR^2 of sign te verbeteren!
# --> niet per se beter model!
summary(logloon)

par(mfrow=c(2,2))
plot(logloon)
# nog altijd een paar punten met onder- of overschatting
# maar veel minder dan daarnet!
# dus veel beter gemiddeld 0
# de rest van de modelveronderstellingen zijn zo goed als onveranderd
# model duidelijk beter dan vorige
par(mfrow=c(1,1))

#### {Outlieronderzoek}
logloon = lm(income~Education+log10(pop))
summary(logloon)
pollutie[c(8,48),]

zonderoutliers = lm(income~Education+log10(pop),
                    data=pollutie[-c(8,48),])
summary(zonderoutliers)

#### {Voorspellingen en voorspellingsintervallen}
## eerst eenvoudig model, voor de prentjes
plot(income ~ Education)
abline(model1, col='red')
betrouwbh = predict(model1, interval = "confidence", level = 0.95) 
predictie = predict(model1, interval = "prediction", level = 0.95) 
lines(sort(x_i), betrouwbh[order(x_i),2], col='blue')
lines(sort(x_i), betrouwbh[order(x_i),3], col='blue')
lines(sort(x_i), predictie[order(x_i),2], col='green')
lines(sort(x_i), predictie[order(x_i),3], col='green')   
## voorspellingen met het meervoudige model
predict(logloon,data.frame(Education=10,pop=500000), level = 0.95)
# --> puntschatting, zeker fout!
predict(logloon,data.frame(Education=10,pop=500000),
        interval="confidence",level = 0.95)
# puntschatting is fout dus we willen een betrouwbaarheidsinterval
# --> dit is het interval voor de REGRESSIELIJN!!
predict(logloon,data.frame(Education=10,pop=500000),
        interval="prediction", level = 0.95)
# --> dit is het interval voor de INDIVIDUELE STAD!

#### {Indicatorvariabelen}
binair = arbeid=="laag"
table(binair)
loongroepen = update(logloon,.~.*binair)
summary(loongroepen)
loongroepen = update(loongroepen,.~.-Education:binair)
summary(loongroepen)
loongroepen = update(loongroepen,.~.-log10(pop):binair)
summary(loongroepen)
loongroepen = update(loongroepen,.~.-binair)
summary(loongroepen)

detach(pollutie)

