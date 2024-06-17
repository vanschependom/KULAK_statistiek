load("cars.RData")
attach(cars)

# Selectie van een aantal numerieke veranderlijken voor regressie
car_cont <- data.frame(minpr, engines,horsepo,RPM,revolu,length,
                       wheelbs,width,weight,city,hghwy)

## 11.1 Correlatie
cor(minpr,car_cont)
# Correlaties zijn voor de meeste maten positief
# (prijs stijgt gemiddeld met toenemende engines/horsepo/...)
# Behalve voor RPM en revolu die een negatieve associatie kennen
# (prijs daalt gemiddeld met toenemende RPM/revolu)
# Correlaties zijn redelijk uitgesproken (0.4 en hoger) behalve bij RPM waar die heel laag is
qqnorm(minpr); qqline(minpr)
# Formeel testen kan niet met Pearson-correlatie aangezien minpr zeer scheef is
# (verdeling van andere veranderlijken doet er dus al niet meer toe)
cor(minpr,car_cont,method="spearman")
# Bepalen vanaf welke waarde voor r de Spearmancorrelaties significant zijn (cfr. oef. 10.7)
# (Alternatief kan je natuurlijk ook één voor één cor.test uitvoeren)
t = qt(.975,91)
t/sqrt(91+t^2)
# Vanaf abs(r) = 0.20 is correlatie significant,
# dus enkel met  RPM is er géén associatie vast te stellen
# de andere verbanden zijn significant (zie eerder)

## Correlatie tussen de veranderlijken onderling
cor(car_cont, method="spearman")
# De veranderlijken zijn nagenoeg allemaal sterk onderling gecorreleerd.
# Dit betekent dat ze door multicollineariteit mogelijk niet allemaal significant zullen blijven in het regressiemodel

## 11.2 Regressiemodel opstellen
model <- lm(minpr ~ ., data=car_cont); summary(model)
# horsepo en width zijn sterk significant in dit model
# revolu, length, wheelbs en city neigen naar significantie
# dit wijkt wat af van de verwachtingen: horsepo en city waren immers sterkst gecorreleerd
# multicollineariteit maakt dat het model onvoorspelbaar wordt

# we gooien telkens de variabele met de grootste p-waarde uit het model (zolang die groter is dan 0.05)
model = update(model,.~.-hghwy); summary(model)
model = update(model,.~.-weight); summary(model)
model = update(model,.~.-RPM); summary(model)
model = update(model,.~.-wheelbs); summary(model)
model = update(model,.~.-revolu); summary(model)
model = update(model,.~.-engines); summary(model)
# Dit model verklaart een behoorlijk groot (R2_adj=0.72) en zeer significant deel van de variantie (p<2.2e-16)
# en elk van de coefficienten verschilt van nul (zeer significant, p telkens 0.0001 of kleiner)

## 11.3 Modelveronderstellingen
par(mfrow=c(2,2)) # dit werkt enkel op computerschermen met hoge resolutie (zoniet foutmelding: figure margins too large)
plot(model)
par(mfrow=c(1,1))
# Gauss-Markov-voorwaarden zijn niet voldaan:
# Plot 1 - residuplot suggereert kromlijnige trend (goedkope en dure wagens worden systematisch onderschat, gemiddelde wagens worden overschat)
# Plot 2 - residuen wijken af van normaliteit (eerder tangentiale vorm)
# Plot 3 - de variantie is niet constant, er is sterke heteroscedasticiteit, (vooral:) oplopende variantie bij duurdere wagens
# Plot 4 - outlier-onderzoek: zie verder
# Lineaire model is dus duidelijk niet correct: transformatie zoeken
boxplot(minpr)
boxplot(log10(minpr))
# Logaritmische transformatie is eerder symmetrisch en dus mogelijk meer geschikt voor lineair model
# (tiendelig wegens makkelijker te interpreteren dan natuurlijk)
logmodel = lm(log10(minpr)~., data=car_cont); summary(logmodel)
logmodel = update(logmodel,.~.-engines); summary(logmodel)
logmodel = update(logmodel,.~.-weight); summary(logmodel)
logmodel = update(logmodel,.~.-hghwy); summary(logmodel)
logmodel = update(logmodel,.~.-revolu); summary(logmodel)
logmodel = update(logmodel,.~.-RPM); summary(logmodel)
logmodel = update(logmodel,.~.-wheelbs); summary(logmodel)
# Dit model verklaart een iets groter (R2_adj=0.79) en opnieuw zeer significant deel van de variantie (p<2.2e-16)
# Model bevat (toevallig! zie volgorde van eliminatie) dezelfde termen, met nog steeds zeer significant van nul verschillende coefficienten
# Allemaal zeer bemoedigend maar motiveert de logaritmische transformatie op zich niet: bekijk daarvoor de modelveronderstellingen van het logaritmisch model
par(mfrow=c(2,2)) # dit werkt enkel op computerschermen met hoge resolutie (zoniet foutmelding: figure margins too large)
plot(logmodel)
par(mfrow=c(1,1))
# Gauss-Markov-voorwaarden zijn niet voldaan:
# Plot 1 - nog niet perfect maar een stuk beter dan het lineaire model (enkel nog systematische onderschatting van lage prijzen)
# Plot 2 - residuen lijken niet meer af te wijken van normaliteit
# Plot 3 - heteroscedasticiteit is verdwenen, variantie lijkt zo goed als constant
# Plot 4 - outlier-onderzoek: zie verder
# Logaritmische model is niet perfect maar een pak beter dan het lineaire model
# de transformatie is duidelijk zinvol!

## 11.4 Residuen t.o.v. regressoren apart
par(mfrow=c(2,2)) # dit werkt enkel op computerschermen met hoge resolutie (zoniet foutmelding: figure margins too large)
termplot(logmodel, partial.resid=TRUE)
par(mfrow=c(1,1))
# Een mogelijkheid om de afwijking van de residuen op te lossen is door transformatie van de onafhankelijke veranderlijken
# Uit de partiële residuplot blijken geen kromlijnige verbanden.
# Eerder ter illustratie dan uit overtuiging dat het zo zal worden opgelost, bekijken we of een kwadratische transformatie oplossing kan bieden:
summary(update(logmodel, .~.+I(length^2))) # niet significant
summary(update(logmodel, .~.+I(width^2))) # niet significant
summary(update(logmodel, .~.+I(city^2))) # niet significant
kwadmodel = update(logmodel, .~.+I(horsepo^2))
summary(kwadmodel) # net significant
# Enkel de coefficient van horsepo^2 is significant verschillend van nul (p=0.04)
# er is een voorzichtige suggestie dat de kwadratische term in horsepo verschilt van nul
# ook een lichte stijging van R^2
# modelveronderstellingen kwadratisch model nagaan
par(mfrow=c(2,2)) # dit werkt enkel op computerschermen met hoge resolutie (zoniet foutmelding: figure margins too large)
plot(kwadmodel)
par(mfrow=c(1,1))
# Plot 1: de residuplot VERSLECHT door toevoeging van de kwadratische term
# Aangezien het verbeteren van de residuplot het doel was van de kwadratische transformatie, biedt deze overduidelijk geen oplossing
# Bovendien is kwadratisch effect ook moeilijk verklaarbaar (bergparabool die dus voorschrijft dat prijs een maximum bereikt en daarna zal dalen bij toename van horsepo)

## 11.5 Outlieronderzoek
plot(logmodel, which=5)
# Plot 4 - Een punt kan op twee manieren "invloedrijk" zijn
# - verticaal door een groot residu (verschil tussen effectieve en voorspelde prijs)
# - horizontaal door een hoge "leverage" (afstand van observatie tot het centrum van de puntenwolk, op basis van de onafhankelijke veranderlijken)
# Enkel observaties die zich op beide manieren onderscheiden zullen typisch een grote invloed hebben op het model
cars[c(28,58,59),]
# Twee Mercedessen (58 en 59) kosten in werkelijk veel meer dan het model voorspelt. Deze hebben lage leverage dus waarschijnlijk geen grote impact op het regressiemodel.
# De Dodge Stealth (28) blijkt een pak minder te kosten dan het model voorschrijft. De wagen valt op doordat die de grootste horsepo in de dataset heeft -- maar blijkbaar geen navenante prijs.
# (VER buiten bestek van de cursus, zie vervolgvak SMDA: de leverage is zo groot omdat de wagen wel zeer hoge horsepo heeft, maar bescheiden afmetingen en verbruik, terwijl de correlatie tussen die veranderlijken net heel hoog is)
# Doordat de leverage én het residu van de Dodge Stealth groot zijn, oefent deze mogelijk invloed uit op de regressievergelijking.
outmodel = update(logmodel,.~.,data=car_cont[-28,])
summary(outmodel)
# Outliers worden NIET verwijderd uit de dataset maar
# WEL gerapporteerd, welke observaties, waarom ze bijzonder zijn
# eventueel welk effect ze op het model hebben (zie hieronder).

# Effect van Dodge Stealth op het model:
# Vergelijk coefficienten uit beide modellen rekening houdend met standaardfout
#           Volledig model   | Model zonder Dodge
# Intercept 1.1 (+/- 0.3)    |  1.1 (+/- 0.2)
# horsepo   0.0023  (0.0003) |  0.0026  (0.0003)
# length    0.22    (0.05)   |  0.20    (0.05)
# width    -1.0     (0.2)    | -0.9     (0.2)
# city      0.040   (0.008)  |  0.037   (0.008)
# De coefficienten veranderen nauwelijks:
# grootste verandering bij horsepo, en zelfs daar slechts 1 standaardfout,
# betrouwbaarheidsinterval is ongeveer 2 standaardfouten breed:
# dit is dus geen groot verschil.
# (het het geval een outlier WEL duidelijk effect heeft:
#  effect beschrijven en eventueel beide vergelijkingen rapporteren)

## Bijkomende bedenking bij het voorkomen van residuen:
# Grootste gestandaardiseerde residuen in deze dataset is ongeveer 3
z = 3
p=2*pnorm(-z); p
# Kans dat een observatie zo een residu heeft is klein: minder dan 3 op duizend
1-pbinom(0,93,p)
# Kans dat er in een dataset van 93 observaties GEEN zo een residu voorkomt is 22%.
# In de grote meerderheid van steekproeven van 93 observaties zal er dus WEL een observatie z=3 voorkomen.

## 11.6 Indicatorvariabele
# Voeg kruistermen aan model toe
domestimodel <- update(logmodel, .~.*domesti); summary(domestimodel)
## zolang er productveranderlijken in het model zijn
## mogen de "main effects" niet worden verwijderd
## enkel de productveranderlijken staan dus ter discussie
domestimodel <- update(domestimodel,.~.-city:domesti); summary(domestimodel)
domestimodel <- update(domestimodel,.~.-width:domesti); summary(domestimodel)
domestimodel <- update(domestimodel,.~.-length:domesti); summary(domestimodel)
# Nu is domesti niet significant,
# maar domesti moet in het model blijven omdat het voorkomt in de productveranderlijke!
# Er is een significant verschil tussen de prijszetting van in de US geproduceerde en geimporteerde wagens
# Amerikaanse wagens hebben een andere prijszetting naargelang horsepo
# De andere onafhankelijke veranderlijke lijken niet te verschillen tussen beide groepen producenten

## Coefficienten interpreteren in lineair model:
coef(domestimodel)
coef(domestimodel)[1]+coef(domestimodel)[6]
coef(domestimodel)[2]+coef(domestimodel)[7]
# log10(minpr)_non-US | log10(minpr)_US = 
# = 0.5               | = 0.5  [0.46+0.05]
# + 0.0025 horsepo    | + 0.0017 horsepo
# + 0.21 length       | + 0.21 length
# - 0.55 width        | - 0.55 width
# + 0.035 city        | + 0.035 city

# Betekenis INTERCEPT:
# Voor een auto met 0 horsepo, 0 afmetingen en 0 verbruik
# [en dus: geen auto, helemaal niet relevant]
# is log10(minpr) volgens dit model gemiddeld gelijk aan
# 0.46 (voor non-US wagens)
# 0.51 (voor US wagens)
# (beide intercepts niet significant verschillend)

# Betekenis SLOPE [1 als voorbeeld, rest als oefening]
# per extra eenheid horsepo
# zal log10(minpr) *volgens dit model* toenemen
# met gemiddeld 0.0025 eenheden (voor non-US)
# met gemiddeld 0.0017 eenheden (voor US)
# de gegevens suggereren dus (p=0.03) dat
# de prijs van importwagens sneller toeneemt naargelang toenemende horsepo
# dan die van in de US geproduceerde wagens,
# namelijk met gemiddeld 0.0009 eenheden per extra eenheid horsepo

## Coefficienten interpreteren van model voor minpr zelf
10**coef(domestimodel)
10**(coef(domestimodel)[1]+coef(domestimodel)[6])
10**(coef(domestimodel)[2]+coef(domestimodel)[7])
# minpr_non-US        | minpr_US
# = 2.9               | = 3.2
# * 1.006^horsepo     | * 1.004^horsepo
# * 1.6^length        | * 1.6^length
# * 0.3^width         | * 0.3^width
# * 1.08^city         | * 1.08^city

# Betekenis INTERCEPT:
# Voor een auto met 0 horsepo, 0 afmetingen en 0 verbruik
# [en dus: geen auto, helemaal niet relevant]
# is minpr volgens dit model gemiddeld gelijk aan
# 2.9 duizend dollar (voor non-US wagens)
# 3.2 duizend dollar (voor US wagens)
# (beide intercepts niet significant verschillend)

# Betekenis SLOPE [1 als voorbeeld, rest als oefening]
# per extra eenheid horsepo
# zal minpr *volgens dit model* toenemen
# met gemiddeld factor 1.006 of dus 0.6% (voor non-US)
# met gemiddeld factor 1.004 of dus 0.4% (voor US)
# de gegevens suggereren dus (p=0.03) dat
# de prijs van in de US geproduceerde wagens trager toeneemt naargelang
# toenemende horsepo dan die van geïmporterde wagens
# namelijk met gemiddeld FACTOR 0.998 per extra eenheid horsepo

## EXTRA. Voorspellingen (zie R Manual sectie 5.4)

# Nissan Almera uit 1993
# 114 pk, lengte = 412 cm, breedte = 169.5 cm, verbruik in stad = 8.87 l/100km, domesti = non-US.
almera <- data.frame(
  horsepo=114,
  length=4.12,
  width=1.695,
  city=8.87,
  domesti= "non-US")

# Betrouwbaarheidsinterval
CONF = predict(domestimodel, almera,
                interval = "confidence",
                level = 0.95)
# Voorspellingsinterval
PRED = predict(domestimodel, almera,
               interval = "prediction",
               level = 0.95)
# Voorspelde waarden zijn voor het logaritme van de prijs:
# intervallen nog transformeren
10^CONF
10^PRED
boxplot(minpr)
abline(h=10^(CONF),col='blue')
abline(h=10^(PRED),col='green')
# 1. Volgens ons model zou deze wagen in 1993 in Amerika
#    9936 dollar hebben gekost.
# 2. Een 95% betrouwbaarheidsinterval voor de gemiddelde prijs van auto's met
#    dezelfde specificaties loopt van 9036 tot 10925 dollar. 
# 3. Een 95% voorspellingsinterval voor de specifieke prijs van auto's met
#    dezelfde specificaties loopt van 6529 tot 15122 dollar.
# Ondanks de aanvaardbaar hoge R^2, blijkt de prijs op basis van de gebruikte
# veranderlijken toch nog zeer onvoorspelbaar (breed interval).
# Het model voorspelt niets veel specifieker dan dat dit een "relatief goedkope" zou moeten zijn
# (Ga na op de residuplot dat voor yhat=0.997 het model niet meer systematisch onderschat,
#  dit zouden dus allemaal relatief betrouwbare schattingen moeten zijn.)

