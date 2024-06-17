rm(list=ls())

setwd("~/Documents/Code/KULAK_statistiek/OZ10/")

load("cars.RData")

attach(cars)


########
# 10.1 #
########
# Correlatie city <-> highway

# Numeriek: normaliteit?
#   H0: normaal verdeeld
#   H1: niet normaal verdeeld
shapiro.test(hghwy); qqnorm(hghwy); qqline(hghwy);
shapiro.test(city); qqnorm(city); qqline(city);
#   --> geen duidelijke afwijking van normaliteit

plot(hghwy,city)
# ongeveer elliptisch

# Beiden normaal: Pearson Correlatie test
#   H0: rho = 0
#   H1: rho != 0
cor.test(hghwy, city, method="pearson")
#   --> ! bijna perfecte positieve correlatie !
#       --> auto's met een hogere waarde voor X hebben gemiddeld ook een
#       --> hogere waarde voor Y
plot(hghwy, city)


########
# 10.2 #
########
# Meer airbags bij Amerikaanse constructeurs dan bij geimporteerde?

summary(airbags)
summary(domesti)

# !!
table(airbags, domesti)
# --> zijn ze gelijkaardig of zitten er verschillen in?
# --> test voor afhankelijkheid tussen twee kwalitatieve variabelen

test = chisq.test(airbags, domesti);

test$expected   # Cochran regel is voldaan

test
# --> p = 0.79

# Conclusie:  we hebben geen reden om te twijfelen aan de onafhankelijkheid
#             van het aantal airbags in een auto en het al dan niet gemaakt 
#             zijn in de US

test$residuals


########
# 10.3 #
########
# Aantal cylinders afhankelijk van het type wagen?

summary(cylin)
summary(type)

table(type, cylin)
# --> wijst op afhankelijkheid, maar we moeten uiteraard nog testen!

test = chisq.test(type, cylin);
test$expected # --> Cochranregel NIET voldaan!
              #       HERCODEREN
              #       -> weinig bij weinig, veel bij veel
              #       -> we gooien 3 en 4 samen     = WEINIG
              #       -> we gooien 5, 6 en 8 samen  = VEEL

####
# --- numerieke veranderlijke hercoderen --- #
####

# hercoderen met cut
cylinder2 = cut(cylin, c(0,5,Inf), labels=c("3-4","5,6,8"), include.lowest=TRUE, right=FALSE)
attributes(cylinder2)

table(type, cylinder2)

# Test omtrent afhankelijkheid
#   H0: onafhankelijk
#   H1: afhankelijk
test = chisq.test(cylinder2, type);
test$expected # --> Cochranregel nu WEL voldaan!
              #       Er zijn nog altijd kleine afwijkingen,
              #       maar daar kijen we niet naar, aangezien
              #       we hier een zeer uitgesproken p-waarde hebben.

# Nu pas test!
test

test$residuals
# !! alleen de waarden groter dan 1 hebben een 'betekenis' !!
#     --> de residuen worden gekwadrateerd, dus kleiner 
#         dan nul wordt gewoon nog nuller!

# BESLUIT:    De gegevens wijzen erop dat er meer cylinders zijn
#             naarmate de auto's groter zijn.


# Daarnet konden we cut() gebruiken, omdat de gegevens NUMERIEK
# waren, maar dat kunnen we nu niet doen, want we werken met LABELS
# --> Hoe hercoderen we een categorische veranderlijke?

####
# --- categorische veranderlijke hercoderen --- #
####

type2 = type
type2[type=="Van"] = "Van+Large" # ERROR
# we hebben een categorische veranderlijke toegevoegd, 
# waarvan er geen label is! 

# --> we moeten het eerst omzetten naar TEKST!
#     als het geen factor meer is, kunnen we ermee doen wat we willen
type2 = as.character(type)
type2[type=="Van"] = "Van+Large"
type2[type=="Large"] = "Van+Large"
type2=as.factor(type2)

test2 = chisq.test(cylinder2, type2); test
test2$expected


########
# 10.4 #
########
# Wijkt de proportie bestelwagens af van 11%?

x = sum(type=="Van")
n = length(type)

# geobserveerde proportie
x/n

# Test omtrent 1 proportie
#   H0: p = p0 = 0.11
#   H1: p != 0.11
binom.test(x, n, p=0.11)

# BESLUIT:
#   Er is niets dat erop wijst dat het aantal bestelwagens significant
#   verschilt van 11%. De gegevens lijken de hypothetische proportie van
#   11% goed te volgen.


########
# 10.5 #
########

###
# Zijn er minder Amerikaanse wagens met manuele transmissie?
###

table(transm, domesti)

?prop.test
test = prop.test(x=c(22,39), n=c(48,45), alternative="less")

# Test voor twee proporties
#   H0: gelijke proporties
#   H1: p_1 < p_2
test;
# --> we kijken hier niet naar de residuen maar naar de proporties

# Wat als Cochran niet in orde is en we 2x2 hebben?
# Als Cochran wel geldig is, altijd chisq, want dat is
# veel beter interpreteerbaar!!
# Wat als we niet kunnen hercoderen?
#   --> FISHER
fisher.test(table(transm,domesti))

###
# Verschilt de beschikbaarheid van manuele transmissie bij
# Amerikaanse producenten en importeurs binnen de groep van
# compacte wagens?
###

table(transm[type=="Compact"],domesti[type=="Compact"])
fisher.test(table(transm[type=="Compact"],domesti[type=="Compact"]))


detach(cars)
