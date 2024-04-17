### Oefenzitting 1: Beschrijvende statistiek

## Gegevens inlezen

## Oefening 1
setwd("/Users/vincent/Documents/Code/KULAK_statistiek/OZ2") # werk-directory instellen, zodat je die in volgende commando's niet meer moet meegeven
cars = read.table(
	"cars.dat",
	header=TRUE,
	sep="\t",
	na.strings="*",
	dec=",") # Zie cars.txt. We hebben nog geen goede optie voor row.names.


## Oefening 2 (zeer technisch, buiten bestek van de cursus)
cars$model
# cars$model = as.character(cars$model) # In de recentste R-versies is cars-model al een character-variable
class(cars$model)
table(cars$model)[table(cars$model)!=1] # Er is een model dat dubbel voorkomt
cars[cars$model=="Lumina",] # De twee modellen zijn verschillend
cars[cars$model=="Lumina",2] = # Geef ze een verschillende naam
  c("LuminaM","LuminaV")
row.names(cars) = cars$model # Nu is er wel een goede keuze voor row.names
cars$model = NULL

## Oefening 3 en 5
# Volgende commando's passen alle veranderlijken aan naar SI etc en bestudeert meteen ook ver verdeling
# Er gebeurt dus meer dan in de opgave wordt gevraagd

class(cars$manufa)
cars$manufa
table(cars$manufa)
length(table(cars$manufa))
barplot(table(cars$manufa))
barplot(sort(table(cars$manufa)))
# elke van de 32 fabrikanten heeft 1 tot 8 modellen in de steekproef

class(cars$type)
cars$type = factor(cars$type)
table(cars$type)
barplot(sort(table(cars$type)))
levels(cars$type)
# categorische veranderlijke met 6 levels
# meest voorkomende zijn Small en Midsize, minst voorkomende Van en Large

class(cars$minpr)
# cars$minpr =  0.76 * cars$minpr # Gebruik een wisselkoers (dataset is eigenlijk van 1993, voor de euro bestond)
fivenum(cars$minpr)
boxplot(cars$minpr)
# minimumprijzen sterk rechtsscheef verdeeld, tussen 6.7 en 45 duizend dollar
# enkele extreem hoge waarden, maar niets fysiek onmogelijk

class(cars$midpr)
# cars$midpr =  0.76 * cars$midpr
fivenum(cars$midpr)
boxplot(cars$midpr)
# midpr sterk rechtsscheef verdeeld, tussen 7.4 en 62 duizend dollar
# enkele extreem hoge waarden, maar niets fysiek onmogelijk

class(cars$maxpr)
# cars$maxpr =  0.76 * cars$maxpr
fivenum(cars$maxpr)
boxplot(cars$maxpr)
# maxpr sterk rechtsscheef verdeeld, tussen 7.9 en 80 duizend dollar
# enkele extreem hoge waarden, maar niets fysiek onmogelijk

## Oefening 3b >>>
class(cars$cityMPG)
cars$city = 235.214583 / cars$cityMPG # Mijlen/gallon naar liter/100km. Nieuwe variabele maken.
cars$cityMPG = NULL # oude var weggooien om verwarring te vermijden (dit is een work-around om een naam te wijzigen)
mean(cars$city)
fivenum(cars$city)
boxplot(cars$city)
# licht linksscheef verdeeld, verbruik tussen 5.1 en 16 liter/100km
# aan de hoge kant: laag verbruik niet prioritair in 1993 & USA

class(cars$hghwyMP)
cars$hghwy = 235.214583 /  cars$hghwyMP
cars$hghwyMP = NULL
fivenum(cars$hghwy)
boxplot(cars$hghwy)
# eerder symmetrisch rond 8.4 l/100km, verbruik tussen 4.7 en 12 liter/100km
# aan de hoge kant: laag verbruik niet prioritair in 1993 & USA

# Belangrijke opmerking: 
# - Reken zo exact mogelijk (zie omzetting cars$city)
# - Maar rapporteer enkel relevante cijfers (zie opgave project of cursus fysica)
## <<< Oefening 3b

class(cars$airbags)
cars$airbags =
	factor(cars$airbags,
	labels=c("none","driver","both"))
table(cars$airbags)
table(cars$airbags)/length(cars$airbags)
barplot(table(cars$airbags))
# anno 1993 hebben de meeste wagens ??n of geen airbags, een minderheid 2


class(cars$cylin)
table(cars$cylin)
barplot(table(cars$cylin))
# meeste wagens hebben 4 of 6 cilinders, enkele 8, een uitzondering 3 of 5
# deze veranderlijke is in wezen numeriek maar gezien het beperkt aantal uitkomsten behandelen we ze beter als categorisch

class(cars$engines)
fivenum(cars$engines)
boxplot(cars$engines)
# engines licht rechtsscheef verdeeld, tussen 1.0 en 5.7

class(cars$horsepo)
fivenum(cars$horsepo)
boxplot(cars$horsepo)
# licht rechtsscheef tussen 55 en 300

class(cars$RPM)
fivenum(cars$RPM)
boxplot(cars$RPM)
# eerder symmetrisch tussen 3.8 en 6.5 duizend toeren per minuut

class(cars$revolu)
cars$revolu = cars$revolu / 1.609344 # /mijlen naar /km
fivenum(cars$revolu)
boxplot(cars$revolu)
# eerder symmetrisch tussen 820 en 2400

## Oefening 3c >>>
class(cars$drivetr)
cars$drivetr =
  factor(cars$drivetr,
         labels=c("rear","front","all"))
table(cars$drivetr)
table(cars$drivetr)/length(cars$drivetr)
barplot(table(cars$drivetr))
# grote meerderheid van de wagens heeft voorwielaandrijving, minderheid achter- of vierwielaandrijving

class(cars$transm)
cars$transm = 
	factor(cars$transm,labels=c("auto","manual"))
table(cars$transm)
barplot(table(cars$transm))
# (slechts) twee derde van de nieuwe wagens op de Amerikaanse markt heeft manuele transmissie
## <<< Oefening 3c

class(cars$fuelta)
cars$fuelta = 3.78541178 * cars$fuelta # Gallons naar liter
fivenum(cars$fuelta)
boxplot(cars$fuelta)
# eerder symmetrisch tussen 35 en 100 liter

class(cars$pass)
table(cars$pass)
table(cars$pass)/length(cars$pass) # Relatieve frequentietabel maken
barplot(table(cars$pass))
# tussen 2 en 8 passagiers, meestal 5

## Oefening 3a >>>
class(cars$length)
cars$length = 2.54 * cars$length / 100 # naar m
fivenum(cars$length)
boxplot(cars$length)
# eerder symmetrisch, wagens tussen 3.6 en 5.6m lang

class(cars$width)
cars$width = 2.54 * cars$width / 100
fivenum(cars$width)
boxplot(cars$width)
# eerder symmetrisch, wagens tussen 1.5 en 2.0m breedte

class(cars$weight)
cars$weight = 0.45359237 * cars$weight # Pound naar kg
fivenum(cars$weight)
boxplot(cars$weight)
# licht linksscheef, tussen 770 en 1900 kg
## <<< Oefening 3a

class(cars$wheelbs)
cars$wheelbs = 2.54 * cars$wheelbs / 100
fivenum(cars$wheelbs)
boxplot(cars$wheelbs)
# eerder symmetrisch, wagens tussen 2.3 en 3.0m wielbasis


class(cars$uturn)
cars$uturn = 0.3048 * cars$uturn 
fivenum(cars$uturn)
boxplot(cars$uturn)
# licht linksscheef, tussen 9.7 en 14 meter

class(cars$rearsea)
cars$rearsea = 2.54 * cars$rearsea / 100
fivenum(cars$rearsea)
boxplot(cars$rearsea)
# zitplaatsen tussen 48 en 91 cm breed, rechtsscheef maar met twee erg kleine waarden

class(cars$luggage)
cars$luggage = 0.0283168466 * cars$luggage  # Kubieke voet naar kubieke meter
fivenum(cars$luggage)
boxplot(cars$luggage)
# eerder symmetrisch tussen 0.17 en 0.62 kubieke meter

class(cars$domesti)
cars$domesti =
	factor(cars$domesti,
	labels=c("non-US","US"))
table(cars$domesti)
barplot(table(cars$domesti))
# ongeveer evenveel geimporteerde als eigen modellen

## Oefening 3d >>>
cars$opties = cars$maxpr - cars$minpr # Nieuwe variabele maken
class(cars$opties)
fivenum(cars$opties)
boxplot(cars$opties)
cars[cars$opties>20,] # Check de outlier
# rechtsscheve veranderlijke, tussen 0 en 36 duizend dollar, met een extreme outlier, Mercedes 300E
## <<< Oefening 3d

## Oefening 4
save(cars,file="cars.RData") # Klaar met manipulatie van de data: attach

## Oefening 5: Zie hoger
attach(cars)

# in de filmpjes worden de prijzen nog naar euro omgezet, dat gebeurt hier in het script niet... licht verschillende grafieken maar identieke conclusies.

## Oefening 6
plot(length,minpr)
plot(width,minpr)
plot(weight,minpr)
# prijs stijgt met afmetingen, grotere spreiding naarmate afmeting groter
# trend mogelijk kromlijnig

## Oefening 7
plot(minpr,opties,asp=1)
abline(a=0,b=1,col="red") # Twee wagens met duurdere opties dan de wagens
cars[opties > minpr,] # Bekijk deze twee wagens
# twee goedkope wagens hebben een relatief duur optiepakket
# extreem dure optiepakket hoort bij extreem dure wagen

## Oefening 8
mean(city)
plot(city,hghwy) # Linear verband?
cor(city,hghwy) # Hoge correlatie!
abline(a=0,b=1,col='red') # Niet hghwy=city
boxplot(data.frame(city,hghwy)) # Boxplot van twee verschillende variabelen: met data.frame().
mean(hghwy/city) # Gemiddeld verschil bepalen: 0.76
plot(city,hghwy)
abline(a=0,b=0.76,col='blue') # hghwy=0.76*city is een beter verband
tapply(hghwy/city,cylin,mean) # Bereken het gemiddeld meerverbruik in functie van het aantal cilinders.
boxplot(hghwy/city~cylin) # Boxplot van meerverbruik in functie van cilinders: voordeel is groter bij meer cilinders
# bijna perfecte correlatie tussen beide
# snelwegverbruik is ongeveer 3/4 van stadsverbruik
# verschil vergroot naarmate meer cilinders
# meer cilinders meer geschikt voor snelweg/minder voor stadsverkeer

## Oefening 9
tapply(horsepo,type,mean) # Bereken gemidddeld pk in functie van het type
sort(tapply(horsepo,type,mean)) # Sorteer de types volgens gemiddeld pk
plot(horsepo~type) # Boxplot per klasse (Merk op: gesorteerd op pk)

## Oefening 10
table(type,cylin) # Hoe kleiner de wagen, hoe minder cilinders. Grote spreiding op sportwagens en midsize.