pollutie <- read.delim("~/Documents/Code/KULAK_statistiek/OZ2/pollutie.txt", na.strings="")
pollutie$JanTemp = 5/9 * (pollutie$JanTemp-32)    # omzetten van F naar C
pollutie$JulyTemp = 5/9 * (pollutie$JulyTemp-32)  # omzetten van F naar C
pollutie$Temp = (pollutie$JanTemp + pollutie$JulyTemp) / 2

pollutie[ order(pollutie$JanTemp) , 2 ]
pollutie[ order(0-pollutie$JanTemp) , c(1,2)]
pollutie[ order(pollutie$JanTemp,decreasing=TRUE) , c(1,2) ]
pollutie[ pollutie$JanTemp > 5 & pollutie$JulyTemp < 25 , c(1,2,3) ]

# laad de regiocodes in
load("~/Documents/Code/KULAK_statistiek/OZ2/pollutie-regiocodes.RData")

# frequentietabel
table(regiocodes)

# we kennen M niet, dus we maken er een C van
# (we gaan er vanuit dat dit een typo is, want het gaat maar over 1 observatie)
regiocodes[regiocodes == "M"] = "C"

# regiocodes is nog steeds een chr -> tekst zit tussen quotes
# dit gaan we niet gebruiken voor een categorische veranderlijke
class(regiocodes)
# factor zet chr om naar factor (nominale veriabele)
factor(regiocodes)
class(factor(regiocodes))
# we maken een nieuwe kolom
pollutie$regio = factor(regiocodes) # er komt een nieuwe kolom bij in onze pollutiedataset
# we verwijderen de regiocodes in de environment, want we willen geen overbodige analyse doen
rm(regiocodes)

# het inkomen is een numerieke (continue) veranderlijke
# geen twee steden hebben hetzelfde inkomen (mediaan van de inkomens in die stad)
# we categoriseren dus het inkomen -> we maken er een factor van
# !! 4 CATEGORIEËN = 5 BREAKS !!
# om algemeen te zijn, gebruiken we INF
# eigenlijk is het geen goed idee om voor de labels getallen te gebruiken
# --> zet liever "laag", "midden", "hoog", "zeer hoog"
pollutie$inkomen = cut(pollutie$income, c(0,30000, 35000, 40000, Inf), labels=1:4)

# we vermijden het gebruik van pollutie$... door attach te gebruiken
# hierdoor worden de kolommen gekopieerd in de environment
# --> als je hier dus aanpassingen aan doet, worden deze aanpassingen gedaan in de kopie
# doe dit pas !! NA ALLE DATAVERWERKING !!
attach(pollutie)

# categorische veranderlijken
table(regio)
barplot(table(regio))

# numerieke veranderlijken
mean(income) # income is numeriek, dus dit zou moeten werken, maar er is een ONTBREKENDE WAARDE -> NA
mean(income, na.rm=TRUE) # geen na meer
# we zoeken de ontbrekende waarden, hiervoor gebruiken we deze logical:
is.na(income)
pollutie[is.na(income),]

var(income, na.rm=TRUE) # gekwadrateerd
sd(income, na.rm=TRUE)
median(income,na.rm=TRUE)
hist(income) # NIET GEBRUIKEN
boxplot(income)

# 2 categorische veranderlijken
table(regio, inkomen)
barplot(table(regio, inkomen)) # niet gebruiken, kruistabel is beter in dit geval

# 2 numerieke veranderlijken
plot(JanTemp, JulyTemp)
# verband vergelijken
cov(JanTemp, JulyTemp) # schaalafhankelijk, enkel teken is interpreteerbaar
cor(JanTemp, JulyTemp) # tussen -1 en 1 -> uitersten = perfect lineair verband
cor(JanTemp, JulyTemp, method="spearman")
# verdelingen vergelijken
boxplot(JanTemp, JulyTemp) 

# categorische en numerieke vergelijken
mean(income[regio=="N"])
mean(income[regio=="NO"])
tapply( income, regio, mean, na.rm=TRUE ) # !!
tapply( income, regio, sd, na.rm=TRUE )
tapply( income, regio, max, na.rm=TRUE )
plot( income~regio ) # tilde betekent "in functie van"

detach()
