load("cars.RData")
attach(cars)

## 10.1 Correlatietest
plot(city,hghwy)
cor(city,hghwy)
qqnorm(city); qqline(city)
qqnorm(hghwy); qqline(hghwy)
cor.test(city,hghwy,method="pearson")
# De grafiek toont een zeer uitgesproken lineaire trend
# De correlatie is enorm hoog (0.9)
# Beide veranderlijken wijken niet systematisch af van normaliteit, de Pearsontest is toegelaten
# De geobserveerde correlatie verschilt van 0 (zeer significant)
# De gegevens tonen een zeer sterke stijgende trend:
# wagens met hoger stadsverbruik hebben gemiddeld ook een hoger snelwegverbruik.

## 10.2 Afhankelijkheid in een kruistabel
table(airbags,domesti)
chisq.test(airbags,domesti)$expected 
chisq.test(airbags,domesti) 
# In de kruistabel lijkt het alsof US-producenten minder vaak geen airbags plaatsen en vaker wél.
# De chisq.test is geldig want Cochran-regel is voldaan (E_i>7)
# Het geobserveerde verband duidt echter niet op afhankelijkheid:
# Op basis van deze gegevens blijkt helemaal niet dat US-producenten vooruitstrevender zouden zijn geweest in het plaatsen van airbags.

## 10.3 Afhankelijkheid in een kruistabel
table(type,cylin)
# Het aantal cilinders is numeriek (weliswaar discreet en met heel weinig uitkomsten), 
# maar aangezien 'type' een niet-geordende categorische veranderlijke is, is een test op een kruistabel aangewezen.
# De vele nullen in de kruistabel wijzen op afhankelijkheid,
# verder lijken kleinere types auto's minder cilinders te hebben (correcter: "minder vaak veel cilinders") dan grotere types auto's.
# Een test moet uitsluitsel bieden of deze observaties wijzen op significante afhankelijkheid.
chisq.test(type,cylin)$expected
cylin2=cut(cylin,
           c(0,4,8),
           labels=c("3-4","5-6-8"),
           ordered_result=TRUE)
chisq.test(type,cylin2)$expected
# De verwachte celfrequenties zijn echter systematisch te laag,
# daarom moeten verschillende klassen gepoold worden (hercoderen).
# Types samenbrengen brengt weinig soelaas,
# maar het aantal cylinders kan teruggebracht worden tot "3-4" en "5-8"
chisq.test(type, cylin2)
chisq.test(type, cylin2)$residual
# In dat geval is E_i>5 grosso modo voldaan en is een chi2-test mogelijk
# (Zwakkere versies van de Cochran-regel zijn "E_i>3" of "E_i>1 maar gemiddeld > 5")
# De gegevens tonen afhankelijkheid tussen beide veranderlijken (zeer significant):
# - Grote auto's (large/van), hebben vaker meer cilinders
# - Kleinere auto's (small/compact/midsize), hebben vaker minder cilinders
# - Over het aantal cilinders van sportwagens is niets te zeggen
type2 = as.character(type) # probeer dit eens zonder as.character() om te zien waarom dit nodig is
type2[type2=="Van"]   = "Van-Large"
type2[type2=="Large"] = "Van-Large"
type2 = as.factor(type2)
chisq.test(type2,cylin2)$expected
chisq.test(type2,cylin2)
chisq.test(type2,cylin2)$residual
# Om ten volle aan de Cochran-regel te voldoen kunnen nog de types
# Van en Large (die niet alleen beide te lage aantallen hebben,
# maar ook qua grootte goed bij elkaar passen) worden samengevoegd.
# Significantie en interpretatie veranderen in dit geval niet.

## 10.4 Test voor 1 proportie
length(na.omit(type))
table(type!="Van")
binom.test(9, 93, p=.11, alternative="two.sided") # Exacte test
# andere syntax, identiek resultaat:
binom.test(table(type!="Van"), p=.11, alternative="two.sided") # Exacte test
dbinom(10,93,.11)
qbinom(.05,93,0.11)
qbinom(.95,93,0.11)
# De observatie (9/93) ligt dicht bij de hypothese (11%)
# de p-waarde is dan ook erg groot (87%), de nul-hypothese wordt aanvaard,
# de proportie bestelwagens in de steekproef wijkt niet significant af van 11%
# Merk op dat de p-waarde hier exact het complement is van precies 10 successen (10=mediaan onder H0)
p0 = 0.11
pobs = 9/93
zobs = (pobs-p0)/sqrt(p0*(1-p0)/93)
2*pnorm(zobs)
93*p0
# Idem via benaderende z-test voor 1 proportie

## 10.5 (a) Test voor twee proporties
table(domesti,transm)
# Merk op dat de observatie in overeenstemming is met de alternatieve hypothese
# de p-waarde van deze eenzijdige test zal dus kleiner moeten zijn dan 50% (controle!)
prop.test(c(6,26), c(45,48),alternative = "less")
# Alternatieve syntax, identieke test:
prop.test(table(domesti,transm),alternative = "less")
# De p-waarde van deze test is zeer klein
# Amerikaanse wagens hebben inderdaad minder vaak manuele transmissie beschikbaar
# Alternatief: z-test voor 2 proporties, zelf na te rekenen

## 10.5 (b) Test voor afhankelijkheid
x=cars[type=="Compact",]
table(x$domesti,x$transm)
prop.test(c(0,2), c(9,7),alternative = "less")
chisq.test(x$domesti,x$transm)$expected
# prop.test is blijkbaar eigenlijk chisq.test voor afh en Cochran is niet voldaan: alternatief zoeken!
# de exacte Fisher test is aangewezen
fisher.test(x$domesti,x$transm)
chisq.test(x$domesti,x$transm)$residual
# Het effect dat er relatief minder Amerikaanse wagens manuele transmissie hebben
# is niet significant binnen deze groep (en kan dus even goed te wijten zijn aan toeval)
# De observatie wijst wel in dezelfde richting als het effect in de volledige dataset
# mogelijks/vermoedelijk is de niet significantie dus eerder gevolg van de kleine aantallen
## Merk op dat de residuen uit de chi2 test wel degelijk interessant zijn,
## ze zijn enkel niet geschikt om er een test mee uit te voeren

## 10.6 (a)
fpk=cut(1000*engines,
       breaks=c(0,seq(751,2751,by=200),
                seq(3051,3651,by=200),
                3951,4151,1000*max(engines)),
       labels=c(4:20,"20+"),
       include.lowest=TRUE,
       ordered_result=TRUE)
cor.test(rank(fpk),horsepo,method="spearman")
# fpk is een ordinale veranderlijke, dus een pearson-test is niet toegelaten
# de spearmantest toont dat de data extreem significant wijzen op correlatie tussen beide
# de spearmancorrelatie duidt met 0.82 op hoge mate van positieve associatie
# hogere waarden van fpk betekent gemiddeld hogere waarde van effectieve pk

# Kan ook via chisq-test, hoewel hier veel info wordt weggegooid
# In dit geval blijft het zeer significant, maar in het algemeen kan dit te veel aan power inboeten
horsepo2 <- cut(horsepo, c(0,100,150,300), 
                labels = c("-100","101-150","150-300"), ordered_result = T)
fpk2 <- cut(as.numeric(fpk), c(0,8,13,Inf), labels = c("-8","9-13","14+"),
            ordered_result = TRUE)
table(fpk2, horsepo2)
chisq.test(fpk2, horsepo2)$expected
chisq.test(fpk2, horsepo2)

## 10.6 (b)
table(fpk)
table(drivetr)
table(fpk,drivetr)
chisq.test(fpk,drivetr)
chisq.test(fpk,drivetr)$expected
# deze test verwerpt net de nulhypothese (p=3.5%)
# MAAR is niet geldig omdat er absoluut niet aan Cochran is voldaan!
fpk3=cut(1000*engines,
        breaks=c(0,2351,1000*max(engines)),
        labels=c("-12","13+"),
        include.lowest=TRUE,
        ordered_result=TRUE)
chisq.test(fpk3,drivetr)$expected
chisq.test(fpk3,drivetr)
chisq.test(fpk3,drivetr)$residual
# De klassen van fpk moeten worden samengenomen om de Cochran-regel te halen.
# Daardoor wordt zo veel informatie weggegooid,
# dat de chi2-test net niet meer significant is.
# De enige conclusie is dat
# - de hypothese van onafhankelijkheid NIET kan worden verworpen,
# - de residuen suggereren dat wagens met achterwielaandrijving vaker meer fiscale PK's hebben
# - maar gezien de lage aantallen is dit verband niet significant en zou het even goed toeval kunnen zijn

## 10.7 Correlatie
n=seq(3,100)
t=qt(.975,n-2)
R=t/sqrt(n-2+t^2)
plot(n,R,type="l",col="blue",xlab="Steekproefgrootte",
     ylab="Significante |R|")
# Stel alpha = 0.05
# H0 verwerpen als abs( R*sqrt(n-2)/sqrt(1-R^2) ) > qt(0.975,n-2) = t
# dus als R^2*(n-2)/(1-R^2) > t^2
# dus als R^2*(n-2) > t^2 - t^2*R^2
# dus als R^2*(n-2+t^2) > t^2
# dus als R^2 > t^2 / (n-2+t^2)
# dus als R > t / sqrt(n-2+t^2) of R< -t / sqrt(n-2+t^2)