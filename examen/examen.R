# 2.5
(0.8*0.3)/(0.8*0.3+0.1*0.6+0.1*0.1)



############################################################

# 3.1
aantal = 1:7
kans = c(0.237,0.317,0.178,0.157,0.070,0.026,0.015)
# verwachtingswaarde
EX = sum(aantal*kans)
VarX = sum((aantal-EX)^2*kans)
EX; sqrt(VarX);


# 3.9
?pnbinom
dnbinom(0,20,prob=0.98)
dbinom(0,20,0.02)
# kans op meer dan 2 failures = 1 - P(1 failure of minder)
1-pnbinom(2,18,0.98)
1-pbinom(2,20,0.02)

# 3.15
# Kans dat oproep binnen de 30 seconden beantwoord wordt
# is gelijk aan 0.75=p

# 1)
20*0.75
# --> 15

# 2)
dgeom(3,0.75)
# --> 1.1%

# 3)
1/0.75

# 4)
# Kans dat men zes maal moet bellen om twee oproepen te bekomen
# die binnen de 30 seconden worden beantwoord
# --> negatief binomiaal
?pnbinom
dnbinom(4, 2, 0.75)




############################################################


#################
# Oefening 4.12 #
#################

# X = werktijd per stuk
# X ~ N(4,1^2)

# Y = werktijd voor 90 stuks
# Y = 90X_
# Y ~ N(90*4, sqrt(90)^2)

# P(40Y<=kost) = P(Y<=kost/40) = 0.95
# => kost = 40*Q(0.95)
40*qnorm(0.95,90*4,sqrt(90))

# Controle
opl = 15024
pnorm(opl/40, 90*4, sqrt(90))



############################################################



# 5.1
p = 1/1000000
n = 250000
sd = sqrt(n*p*(1-p))
dbinom(2,n,p) # exact
dpois(2,n*p)  # goed (want np<5 en n>100)
pnorm(2.5,n*p,sd)-pnorm(1.5,n*p,sd) # niet goed (want np>5 en n(1-p)>5)


# 5.2
n = 1000
p = 0.51
sd = sqrt(n*p*(1-p))
pbinom(499,n,p)
n*p                 # poisson zal NIET goed zijn (want niet np<5 en n>100)
ppois(499,n*p)      # idd
pnorm(499.5,n*p,sd) # WEL goed


# 5.3
mu = 2837
# P (X<1807)=0.1
# IQR? = Q(0.75)-Q(0.25)
# Q(0.9)?
sigma = (1807-2837)/qnorm(0.1)
# controle!!!
pnorm(1807,mu,sigma)
qnorm(0.75,mu,sigma)-qnorm(0.25,mu,sigma)
qnorm(0.9,mu,sigma)


# 5.4
lambda = -(1/6)*log(1-0.889) # 'minder dan 6' kan ook 5.999999... zijn !!
# CONTROLE!!
pexp(6,lambda)
qexp(0.5,lambda)
p=1-pexp(15,lambda)
1-pbinom(19,250,p)


# 5.5
x = 0:4
d = c(.46,.21,.22,.08,.03)
# verwachtingswaarde
EX = sum(x*d)
# variantie
VarX = sum((x-EX)^2*d)



###########################################################



# 6.1
qnorm(0.01,1.4,0.3/sqrt(125))

# 6.2
# Y = aantal geboorten in 1 jaar
#   = 365X_
1-pnorm(82000,365*228,20*sqrt(365))

# 6.3
54/0.5
pnorm(54+0.25,54,sqrt(108)*0.01)-pnorm(54-0.25,54,sqrt(108)*0.01)

# 6.4
n = 1500
p = .7
gem = n*p; gem
sd = sqrt(n*p*(1-p)); sd
# exact
1-pbinom(999,n,p)
n*p # groter dan 5 -> normale benadering
1-pnorm(999.5,gem,sd)
1-pbinom(1199,n,p)
1-pnorm(1199.5,gem,sd)
n = 1700
gem = n*p; gem
sd = sqrt(n*p*(1-p)); sd
1-pbinom(1199,n,p)
1-pnorm(1199.5,gem,sd)

# 6.5
n = 1540
p = 0.15
proc13 = .13*n
proc17 = .17*n
n*p
sd = sqrt(n*p*(1-p))
pnorm(proc17,n*p,sd)-pnorm(proc13,n*p,sd)
# new_sd moet gelijk zijn aan sd/2
sqrt((n*4)*p*(1-p))


# 6.6
t = c(20,30,40,50)
freq = c(.1,.5,.3,.1)
# gem en sd
sum(t*freq)
sqrt(sum((t-34)^2*freq))
# voor X streep
mu = 50*34; sd=8*sqrt(50)
1-pnorm(480*4,mu,sd)


# 6.7
sd = 1/(2*sqrt(50))
1-(pnorm(0.6,1/2,sd)-pnorm(0.4,1/2,sd))
(5*qnorm(0.005))^2


# 6.9
x = 1:6
EX = sum(x*(1/6));
VarX = sum((x-EX)^2/6)

-pnorm(700,(7/2)*210,sqrt(35*210/12))+pnorm(700,(7/2)*180,sqrt(35*180/12))




############################################################



# 8.1
2*pt(-5.38,df=59)

# 8.6

gem_g = 8.2
sd_g = 1.4
gem_b = 6.9
sd_b = 1
n_g = 10
n_b = 12

# We willen de volgende hypothese testen:
#   H0: mu_g <= mu_b
#   H1: mu_g > mu_b   (alternatief: bio verbruikt minder)

# Test voor twee gemiddelden, ongepaarde groepen (n !='nd) -> varianties gelijk?
# Eerst F-test!
f_kwadraat_g = sd_g^2
f_kwadraat_b = sd_b^2
f_obs = f_kwadraat_g/f_kwadraat_b

# welke kant van de verdeling (enkel positieve waarden)
x=seq(0.001, 4, by=0.001)
plot(x,df(x,df1=9,df2=11))
abline(v=qf(0.5,df1=9,df2=11))  # mediaan
abline(v=f_obs)                 # observatie

# !!!
# We zitten aan de rechterkant van de mediaan, dus p = 2P(X>=t_obs) !!!
# !!!

# p-waarde voor F-test (zie hierboven!!)
#   df1 = df gewone diesel = 9
#   df2 = df biodiesel = 11
2*(1-pf(f_obs, df1=9, df2=11))
# Aangezien p heel groot is, zijn de varianties GELIJK

# We voeren dus een test uit voor twee gemiddelden, ongepaarde groepen, GELIJKE varianties
# --> GEPOOLDE VARIANTIE!!
Sp = sqrt((9*sd_g^2+11*sd_b^2)/(10+12-2))
t_obs = (8.2-6.9)/(Sp*sqrt((1/10)+(1/12)))
1-pt(t_obs,20)
# --> We moeten voorzichtig zijn met onze besluiten, aangezien dit geen 0-orde is
# 'We hebben een INDICATIE...'





############################################################


####################
# Examen 1 '19-'20 #
####################

load("people-gegevens.RData")

people

data = data.frame(ind_gender = people$ind_gender, 
                  ind_age = people$ind_age, 
                  health_emo = people$health_emo, 
                  health_fys = people$health_fys)


attach(data)


#############
# VRAAG 2.1 #
#############
# Verschilt de proportie mannen met een emotionele gezondheid kleiner 
# dan 50 van de overeenkomstige proportie vrouwen?

# waargenomen proporties
table(ind_gender[health_emo < 50])
table(ind_gender)

# test voor twee proporties
prop.test(c(90,144), c(818, 914))

# waargenomen proporties: 0.11 vs 0.16
# p = 0.005
# dit is een ZEER SIGNIFICANTE p-waarde,
# dus we verwerpen de nulhypothese dat beide proporties gelijk zijn.


#############
# VRAAG 2.2 #
#############
# Gegeven het model lm(health_fys∼ind_age*ind_gender).
#   (a) Geef een volledige vergelijking van het model dat de fysieke gezondheid verklaart.
#   (b) Leg de coëfficiënten van het model uit.
#   (c) Bespreek inferentie van de coëfficiënten.

model = lm(health_fys~ind_age*ind_gender)

summary(model)

#####
# a #
#####
# afgeronde waarden:
#   intercept man:  95          intercept vrouw:  95-3=92
#   ind_age man:    -0.29       ind_age vrouw:    -0.29-0.07=-0.36
# VERGELIJKING:
#   fysieke gezondheid man    = 95 - 0.29 * ind_age
#   fysieke gezondheid vrouw  = 92 - 0.36 * ind_age

#####
# b #
#####
# -   De fysieke gezondheid van een man stijgt gemiddeld met 2.9 per 10 jaar 
#     dat de man ouder wordt.
# -   De fysieke gezondheid van een vrouw is over het algemeen lager en 
#     neemt sneller af (3.6 per 10 jaar) naar mate de vrouw ouder wordt.

#####
# c #
#####
# - De p-waarde van de individuele Student t-test van de regressiecoefficient
#   voor de leeftijd, waarmee de de binaire dummy-variabele ind_gender interageert,
#   is niet significant (0.4) en daarom kan deze interactie weggelaten worden.
# - Ook verschilt het intercept niet significant (p=0.5) naar gelang het geslacht van
#   de respondent. Ook het hoofdeffect van de indicator kan dus weggelaten worden.


###########
# VRAAG 3 #
###########

# X   = aantal borden in 1 uur  ~ pois(lambda)
# X'  = aantal borden in 10 uur ~ pois(10*lambda)

# p = P(X'>=46) = 1-P(X'<=45)

# Y = aantal afgewerkte borden over een periode van 30 dagen
# Y ~ binom(30,p)

# H0: lambda = 5
# H1: lambda < 5

# Verwerpingsregel: y<15

# Onder H0 (lambda=5):
p = 1-ppois(45,10*5)

# Kans op type I fout
# P( Y<15 | Y~binom(30,0.7331=p) )
pbinom(14, 30, p)
# --> alfa = 0.002

# Kans op type II fout
# lambda_1 = 4.4
p_1 = 1-ppois(45,44)
1-pbinom(14, 30, p_1)
# --> beta = 0.2

detach(data)



####################
# Examen 2 '19-'20 #
####################

load("people-gegevens.RData")

people

data = data.frame(ind_atwork = people$ind_atwork, 
                  hh_parent = people$hh_parent, 
                  ind_edu = people$ind_edu, 
                  health_emo = people$health_emo,
                  hh_nchild = people$hh_nchild)


attach(data)

# exponentiele kwantielplot
qqplot(x=qexp(ppoints(100)),health_emo)

#############
# VRAAG 2.1 #
#############
# Is er een verschil in het gemiddeld aantal kinderen naargelang men betaald werk uitvoert in de
# groep van respondenten van ouders met minstens 1 kind onder 18 jaar?

x = hh_nchild[ind_atwork=="ja" & hh_parent=="ja"]
y = hh_nchild[ind_atwork=="nee" & hh_parent=="ja"]

mean(x)-mean(y) # --> significant?

# Test voor twee gemiddelden, ongepaarde groepen
#   -> niet normaal verdeeld:
qqnorm(x); qqline(x);
qqnorm(y); qqline(y);

# Voldoende observaties --> CLS geldt
na.omit(length(x)); hist(x);
na.omit(length(y)); hist(y);

t.test(x,y,paired=FALSE,var.equal=FALSE)

# De p-waarde is niet significant (0.3), dus we verwerpen de nulhypthese
# dat beide gemiddelden gelijk zijn NIET.

#############
# VRAAG 2.2 #
#############
# Is er afhankelijkheid tussen het opleidingsniveau en de emotionele gezondheid?
summary(ind_edu)
summary(health_emo)

# We zien dat health_emo een continue numerieke veranderlijke is,
# maar dat ind_edu een categorische veranderlijke is. We zullen
# met andere woorden health_emo moeten categoriseren, zodat we de
# afhankelijkheid kunnen nagaan in een kruistabel.
he2 = cut(health_emo, c(0,20,40,60,80,Inf), include.lowest=TRUE, right=FALSE)
summary(he2)

table(ind_edu, he2)
# --> we onderzoeken afhankelijkheid in deze kruistabel

test = chisq.test(ind_edu, he2)

# Cochranregel voldaan? --> JAZEKERRR
test$expected

test;
# --> zeer significante p-waarde! (8*10^{-7} < 0.001)

# Residuen?
test$residuals
# BESLUIT
# - Een diploma minder dan SO heeft gemiddeld gezien vaker een geluksscore onder de 60.
# - Een diploma hoger dan SO heeft gemiddel vaker een geluksscore boven de 80
#   en gemiddeld minder vaak een geluksscore onder de 60.

detach(data)



###########
# VRAAG 3 #
###########

# X   = aantal jassen in 1 uur  ~ pois(lambda)
# X'  = aantal jassen in 10 uur ~ pois(10*lambda)

# p = P(X'>=28) = 1-P(X'<=27)

# Y = aantal afgewerkte jassen over een periode van 20 dagen
# Y ~ binom(20,p)

# H0: lambda = 3
# H1: lambda < 3

# Verwerpingsregel: y<10

# Onder H0 (lambda=3):
p0 = 1-ppois(27,10*3)

# Kans op type I fout
# P( Y<10 | Y~binom(20,p0) ) = P( Y<=9 | Y~binom(20,p0) )
pbinom(9, 20, p0)
# --> alfa = 0.04

# Kans op type II fout
# P( Y>=10 | Y~binom(20,p1) ) = 1 - P( Y<=9 | Y~binom(20,p1) ) 
# lambda_1 = 2.6
p_1 = 1-ppois(27,26)
1-pbinom(9, 20, p_1)
# --> beta = 0.2



#######################
# Examen 24 juni 2019 #
#######################

geobserveerd = c(22,17,23,18,9,6,3,2)
f = function (x) dpois(x,2.15)

verwacht = f(0:6)

1-sum(verwachtRij) # dit is gelijk aan 1-ppois(...)
1-ppois(6,2.15)

verwacht = c(verwacht, 1-ppois(6,2.15))
verwacht

plot(0:7,verwacht)
points(0:7,geobserveerd/sum(geobserveerd), col="red")

# We testen of de geobserveerde aantallen de verwachte verdeling f(x) volgen
test = chisq.test(geobserveerd,p=verwacht)

test


#######################
# Examen 15 juni 2018 #
#######################

# Y = aantal omers drinken voor eerste keer Lukaku

# H0: alle rode duivels komen evenveel voor (p=1/23)
# H1: niet alle rode duivels komen evenveel voor

# Y ~ geom(p) (onder H0 is p=1/23)

# Significantie = kans op type I fout = alfa
# P(Y>40) = 1-P(Y<=39)
?pgeom # --> aantal FAILURES !!
1-pgeom(39,1/23)

# Stel dat Lukaku maar in 1/50 pakjes voorkomt: p=1/50
# --> kans op max 39 failures voor we de eerste keer Lukaku hebben
pgeom(39,1/50)

# Hoeveel Omers moeten er gedronken worden om H0 te verwerpen op een
# significantieniveau van 5%?
qgeom(.95,1/25)



#######################
# Examen 12 juni 2018 #
#######################

X_obs = 19*55/26

# 20 metingen met steekproef variantie = 55

# Aanvaarden?
# P(X^2 > X_obs)?
1-pchisq(40.19,df=19) # --> NIET aanvaarden!

# Power = 1-beta
#       = 1-P(type II fout)
# Met sigma_1=9 --> echte variantie is dus 81
1-pchisq(19*55/81, df=19)

# Kans op type I fout als we H0 verwerpen vanaf een s^2=50
1-pchisq(19*50/36, df=19)




##################
# Examen 16'-17' #
##################

# X = wachttijd tussen twee elfstedentochten
# X ~ exp(8)

# Kans op minstens twintig jaar geen elfstedentocht
1-pexp(20, 1/8)

# Kans op dit jaar een elfstedentocht
pexp(1,1/8)

# Binnen hoeveel jaar met 95% zekerheid elfstedentocht?
# P(X<=wachttijd) = 0.95, 
# dus wachttijd = ...
qexp(0.95,1/8)  # ... 23,97 jaar
2017+24
# Dus in 2041 met 95% zekerheid elfstedentocht

# Gemiddelde wachttijd is met 50% gestegen -> EX = 12 -> lambda = 1/12
# Kans op type II, gegeven lambda_1 = 1/12 = beta
pexp(24,1/12)











