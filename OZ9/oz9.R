setwd("~/Documents/Code/KULAK_statistiek/OZ9/")

load("cars.RData")

attach(cars)



#########
## 9.1 ##
#########

# 1. TESTPROBLEEM
#   Test voor 1 gemiddelde
#   H0: mu = mu_0 = 8.7
#   H1: mu != mu_0 = 8.7

# 2. TESTSTATISTIEK
#   T ~ t_92
#   Voorwaarde: CLS (n groot genoeg)

# !! DE CLS IS GELDIG !!
# Niet gewoon 'enkele tientallen'
# --> "Er zijn 93 meetwaarden, dus de CLS is geldig"
length(na.omit(hghwy)) ## let op voor NA!
# !!

t.test(hghwy, alternative="two.sided", mu=8.7)

# 5. Besluit
#   De gegevens suggereren dat het verbruik is toegenomen.



#########
## 9.2 ##
#########

# 1. SOORT TEST
#   Test voor twee gemiddelden
#   GEPAARDE gegevens!
#   We bekijken dus V = X - 4/3Y = (city) - 4/3(hghwy)
#   H0 : mu_X = 4/3 * mu_Y
#   H1 : mu_X > mu_Y

mean(hghwy)
mean(city)

# VOORWAARDE CLS
length(na.omit(city-(4/3)*hghwy))

t.test(x=city, y=(4/3)*hghwy, alternative="greater", paired=TRUE)

# 5. BESLUIT
#   Er is geen reden om te twijfelen aan het feit dat
#   het stadsverkeer 1/3 meer verbruik per 100 km kent dan verkeer op de snelweg.
#   "De gegevens bevestigen het vooropgestelde verband"



#########
## 9.3 ##
#########

#######
## a ##
#######

# X = de ruimte voor een U-bocht van een sportieve auto
x = uturn[type=="Sporty"]

# Y = de ruimte voor een U-bocht van andere auto's
y = uturn[type!="Sporty"]

mean(x)
mean(y)

# We testen de normaliteit van de gegevens
# --> zowel Shapiro-Wilk als kwantielplot
# --> want !! POWER veel te laag in kleine datasets
#     en veel te hoog in grote datasets !! 
shapiro.test(x)
shapiro.test(y)
qqnorm(x); qqline(x)
qqnorm(y); qqline(y)
# --> "we zien geen afwijkingen van normaliteit"
# --> we testen de varianties als volgende

# 1. SOORT TEST
#   Test voor twee varianties
#   H0: sigma_X^2 = sigma_Y^2
#   H1: sigma_X^2 != sigma_Y^2

var.test(x,y)

# 5. BESLUIT
#   We hebben geen reden om te twijfelen aan het feit dat
#   beide varianties gelijk zijn.

# TWO-SAMPLE T-TEST test met GELIJKE varianties!!

# 1. SOORT TEST
#   t-test (Two-Sample T) voor twee gemiddelden, 
#   GELIJKE varianties!
#   ONGEPAARDE gegevens!
#   H0: mu_X = mu_Y
#   H1: mu_X != mu_Y

# Aangezien de varianties gelijk zijn kunnen we de gepoolde variantie gebruiken
t.test(x, y, alternative="two.sided", paired=FALSE, var.equal=TRUE)

# 5. BESLUIT
#   We hebben geen enkele reden om te twijfelen aan het feit
#   dat sportieve auto's en andere auto's gemiddelde dezelfde
#   ruimte nodig hebben om een U-bocht te maken

#######
## b ##
#######

# X = de plaats voor een U-bocht van auto's met voorwielaandrijving
x = uturn[drivetr == "front"]

# Y = de plaats voor een U-bocht van auto's met achterwielaandrijving
y = uturn[drivetr == "rear"]

# We willen opnieuw eerst weten of X en Y normaal verdeeld zijn,
# want we willen de Welch test met gelijke varianties gebruiken,
# aangezien deze accurater is. Deze test mogen we echter enkel
# gebruiken als beide variabelen normaal verdeeld zijn (voor F-test)

# Testen van normaliteit
shapiro.test(x)
shapiro.test(y)
qqnorm(x); qqline(x)
qqnorm(y); qqline(y)
# --> we hebben geen reden om de normaliteit in vraag te stellen.

# 1. SOORT TEST
#   Test voor twee varianties (normaal verdeelde X en Y)
#   F-test

var.test(x,y)
# --> we hebben geen reden om te twijfelen aan het feit
#     dat beide varianties gelijk zijn

# 1. SOORT TEST
#   Opnieuw test voor twee gemiddelden, ONGEPAARDE GEGEVENS (!)
#   En opnieuw GELIJKE VARIANTIES (!)
#   H0: mu_X = mu_Y
#   H1: mu_X != mu_Y
#   = TWO SAMPLE T TEST

t.test(x,y,alternative = "two.sided", paired=FALSE, var.equal = TRUE)

# TWO SAMPLE T  als gelijke varianties
# WELCH         als verschillende varianties

# 5. BESLUIT
#   De gegevens suggereren dat de gemiddelde plaats voor een U-bocht waarschijnlijk
#   verschilt tussen auto's met voorwielaandrijving en achterwielaandrijving.



#########
## 9.4 ##
#########

summary(domesti)

# X = minimumprijs voor eigen auto's (non-US)
x = minpr[domesti=="non-US"]

# Y = minimumprijs voor import auto's (US)
y = minpr[domesti=="US"]

# H0: mu_X = mu_Y
# H1: mu_X > mu_Y

# We moeten opnieuw weer weten of X en Y normaal verdeeld zijn:
shapiro.test(x); qqnorm(x); qqline(x)
shapiro.test(y); qqnorm(y); qqline(y)
# --> beiden zijn duidelijk niet normaal verdeeld

# We checken of de voorwaarden voor de CLS voldaan zijn:
length(na.omit(x))
length(na.omit(y))
# --> de grootte van de gegevens is voldoende groot om beroep te doen op de CLS

# 1. SOORT TEST
#   Test voor twee gemiddelden
#   Ongepaarde gegevens
#   Ongelijke varianties
#   = WELCH TEST want ongelijke varianties (>< two-sample t bij gelijke var's)

t.test(x,y,paired=FALSE,var.equal = FALSE, alternative="greater")

# 5. BESLUIT
#   De gegevens geven ons weinig reden om te twijfelen aan
#   het feit dat de twee gemiddelden gelijk zijn tussen
#   auto's gemaakt in de US en auto's gemaakt elders dan de US.



#########
## 9.5 ##
#########

# X = mid-prijs van monovolumes (Large) met minstens 6 passagiersplaatsen
x = midpr[type=="Large" & pass>=6]

# Y = mid-prijs van minibusjes (Van) met minstens 6 passagiersplaatsen
y = midpr[type=="Van" & pass>=6]

mean(x)
mean(y)
median(x)
median(y)
boxplot(x,y)

# 1. SOORT TEST
#   Test voor twee gemiddelden
#   Ongepaarde gegevens
#   We weten nog niet of de gegevens normaal verdeeld zijn

# Test de normaliteit van X en Y
shapiro.test(X); qqnorm(X); qqline(X);
shapiro.test(Y); qqnorm(Y); qqline(Y);
# --> de gegevens bezorgen ons een sterk vermoeden dat X en Y niet normaal verdeeld zijn.

# Aangezien X en Y niet normaal verdeeld zijn, kijken we of de voorwaarden
# voor de CLS voldaan zijn, om normaliteit van X_ en Y_ na te gaan.

# Voorwaarden CLS
length(na.omit(X))
length(na.omit(Y))
# --> We kunnen dus GEEN beroep doen op de CLS!!

# Aangezien X,Y ~ ? en X_, Y_ ~ ?, moeten we een NIET-PARAMETRISCHE TEST doen
# --> W.R.S.

# 1. SOORT TEST
#   Test voor twee gemiddelden
#   Ongepaarde gegevens
#   WILCOXON-RANGSOM TEST! --> niet-parametrisch

n1 = length(na.omit(X))
n2 = length(na.omit(Y))

# verwachte rang
n1 * (n1 + n2 + 1) / 2

wilcox.test(X,Y,paired=FALSE,alternative="two.sided")

# ! PAS OP MET TIES !
# bv. bij aantal cylinders opletten
# hier maakt het niet zo veel uit

# 5. BESLUIT
#   De gegevens suggereren dat beide gemiddelden verschillen,
#   en meer specifiek dat het gemiddelde van X groter is dan
#   het gemiddelde van Y (want de RS van X is kleiner dan verwacht)



detach(cars)
