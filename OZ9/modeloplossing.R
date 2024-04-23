load("~/statistiek/cars.RData") # Aanpassen aan eigen bestandslocatie
attach(cars)

### Testen voor het gemiddelde
## Test voor één gemiddelde
# Steekproef is van 1993: was het verbruik toen verschillend dan in 2004?
n = length(na.omit(hghwy)); n # n opvragen
gem = mean(hghwy); gem # gemiddelde
mu0 = 8.7
sd = sd(hghwy); sd # sd
t = (gem-mu0)/(sd/sqrt(n)); t # tobs bepalen
qt(0.025,n-1) # ondergrens van het 95% betrouwbaarheidsinterval
# tobs ligt nog lager dan deze grens => niet in BI => we moeten H0 verwerpen op basis van deze steekproef op significantieniveau 5%
2*(1-pt(abs(t),n-1)) # p-waarde bepalen
# Bovenstaande berkening ter illustratie. Gebruik in de praktijk t.test:
t.test(hghwy, mu=mu0, alternative="two.sided") # Tweezijdige test met mu0 = 8.7
# tobs en p-waarde komen overeen met de eerder bepaalde waarden: conclusie is dezelfde:
# De gegevens suggereren dat het verbruik nog is toegenomen tussen 1993 en 2004

## Test voor twee gepaarde gemiddelden
# Is stadsverbruik significant verschillden van 4/3*snelwegverbruik?
x = hghwy
y = city
n = length(na.omit(y-4/3*x)); n
mean(y)/mean(x) # is deze verhouding significant verschillend van 4/3?
gem = mean(4/3*x-y); gem # gemiddelde van verschil
sd = sd(4/3*x-y); sd # sd van verschil
t = gem/(sd/sqrt(n)); t # tobs
# 1e optie: gepaarde test [aanbevolen]
t.test(4/3*hghwy,y=city,alternative="two.sided",paired=TRUE)
# 2e optie: verschil bekijken  [equivalent met vorige, ter illustratie dat dit equivalent is met test voor 1 gemiddelde]
t.test(4/3*hghwy-city,mu=0,alternative="two.sided")
# Beide testen zijn equivalent: in de gepaarde test zal R precies de verschilveranderlijke berekenen
# De steekproefgegevens wijken niet significant af van de hypothese.
# Het verschil in verbruik tussen stad en snelweg van de wagens in de steekproef, wijst niet op een afwijking van het vooropgestelde verband.

## Test voor twee ongepaarde gemiddelden
## a) Verschilt gemiddelde bochtbreedte voor sportwagens van andere wagens?
table(type) # 14 sportwagens, 79 andere
x = uturn[type=="Sporty"]; length(na.omit(x))
y = uturn[type!="Sporty"]; length(na.omit(y))
mean(y)-mean(x) # Is het verschil in bochtbreedte significant verschillend van nul?
# Eerst normalteit van x nagaan:
# Via histogram
hist(x,freq=FALSE) # histogram: zeer rudimentaire tool, illustratief maar behalve in extreme gevallen niet geschikt om een oordeel mee te vormen
lines(seq(min(x),max(x),0.1),dnorm(seq(min(x),max(x),0.1), mean (x), sd(x)), col='red ') # normale verdeling erbij geplot
# Via kwantielplot
qqnorm(x); qqline(x,col='red') # punten kronkelen mooi rond trendlijn, enkel afwijking in (rechter)staart, maar dat is geen probleem
# Via Shapiro-Wilktest
shapiro.test(x) # n = 14, zeer nipt aantal, p-waarde is relatief hoog: er kan geen afwijking worden vastgesteld
# Normalteit van y nagaan:
# Via histogram
hist(y,freq=FALSE) # histogram
lines(seq(min(y),max(y),0.1),dnorm(seq(min(y),max(y),0.1), mean (y), sd(y)), col='red ') # normale verdeling erbij geplot
# Via QQplot
qqnorm(y); qqline(y,col='red') # opnieuw enkel afwijkingen in de staarten
# Via Shapiro-Wilktest
shapiro.test(y) # n is zeker groot genoeg
# Conclusie: geen redenen om normaliteit te verwerpen.
# [Dit bewijst niet dat X of Y normaal verdeeld zijn,
# maar wel dat de gegevens niet wijzen op heel sterke afwijkingen
# waarbij we er dus van uit gaan dat de tests die volgen
# robuust genoeg zijn voor kleine afwijkingen]
var.test(x,y) # Verschil in varianties is niet significant
# Steekproefvarianties wijzen niet op systemetisch verschil, we kunnen dus t-test voor gelijke varianties uitvoeren
t.test(x,y,alternative="two.sided",paired = FALSE,var.equal=TRUE)
# Het geobserveerde verschil van 3.6 cm is niet significant
# Uit de gegevens blijkt helemaal niet dat draaicirkel van sportauto's anders zou zijn dan die van andere types.

## b) Verschilt gemiddelde bochtbreedte voor voortrekkers en achtertrekkers?
table(drivetr) # 16 achterwielaandrijving, 67 voorwielaandrijving
x = uturn[drivetr=="front"]; length(na.omit(x))
y = uturn[drivetr=="rear"]; length(na.omit(y))
mean(y)-mean(x) # is dit verschil van 57 cm significant?
# Kwantielplots
qqnorm(x); qqline(x)
qqnorm(y); qqline(y) # niet zo fantastisch, eerder sigmoïdale vorm, maar eigenlijk weinig waarden om op voort te gaan
# Shapiro-Wilktest
shapiro.test(x)
shapiro.test(y) # n maar net groot genoeg voor Shapiro-Wilktest, p-waarde relatief hoog
# Formeel moeten we normaliteit niet verwerpen, dus we kunnen een variantietest doen
# [In dit vak mag je shapiro formeel volgen op alpha=5%,
# hoewel dat eigenlijk even goed kan betekenen dat er te weinig gegevens zijn om echt een verschil te zien
# wat hier waarschijnlijk het geval is als je naar kwantielplot kijkt.
# In de praktijk wordt vaker een informele inschatting gemaakt op basis van kwantielplot]
var.test(x,y)
# We moeten gelijkheid van varianties niet verwerpen
t.test(x,y,alternative="two.sided",paired = FALSE,var.equal=TRUE)
# De gegevens geven de voorzichtige suggestie dat wagens met voorwielaandrijving een kleinere draaicirkel hebben dan wagens met achterwielaandrijving.

## Test voor twee ongepaarde gemiddelden
# Verschilt de gemiddelde minimumprijs van importwagens significant van deze van Amerikaanse wagens?
table(domesti) # 45 importwagens, 48 Amerikaanse wagens
tapply(minpr,domesti,mean)
# Importwagens zijn inderdaad gemiddeld duurder, maar is dit significant? => 1zijdige test (MO p-waarde is sowieso <50%)
x = minpr[domesti=="non-US"]; length(na.omit(x))
y = minpr[domesti=="US"]; length(na.omit(y))
# Normaliteit nagaan van beide groepen
qqnorm(x); qqline(x)
qqnorm(y); qqline(y)
shapiro.test(x)
shapiro.test(y)
# We moeten normaliteit verwerpen en kunnen dus geen variantiettest doen
# Er zijn wel voldoende observaties (45 en 48) in elke groep voor CLS
# => we doen t-test met verschillende varianties
# t-test met ongepaarde groepen en verschillende varianties
t.test(x,y,alternative="greater",paired = FALSE,var.equal=FALSE)
# We zien geen significant verschil tussen prijzen in beide groepen
# Op basis van deze steekproef kan niet worden besloten dat importwagens systematisch duurder zijn

## Test voor twee ongepaarde gemiddelden
# Verschilt de minimumprijs van monovolumes significant van die van minibusjes?
table(type) # 11 monovolumes, 9 minibusjes
x = minpr[type=="Large"&pass>5]; length(na.omit(x))
y = minpr[type=="Van"&pass>5]; length(na.omit(y))
mean(x)-mean(y) # monovolumes lijken duurder te zijn, maar is dit significant?
hist(x)
hist(y)
# We kunnen geen normaliteitstest of t-test doen, 
# want n is te klein voor Shapiro-Wilktest en 
# CLS is niet geldig voor deze kleine n! 
# x en y zijn wel niet al te scheef
# => niet-parametrische Wilcoxon-Rangsomtest
wilcox.test(x,y,alternative="two.sided",paired = FALSE) # p-waarde is niet exact als er gelijke waarden zijn
# Deze ziet significant verschil: monovolumes significant duurder dan minibusjes
