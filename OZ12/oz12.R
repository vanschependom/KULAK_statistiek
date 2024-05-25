# betrouwbaarheid
betrouwbaarheid = function(d) { pnorm(abs(d), mean=0, sd=0.2/5) - pnorm(-abs(d), mean=0, sd=0.2/5) }

# significantie alfa
alfa = function(d) {1 - betrouwbaarheid(d) }


###########
# 12.1 a) #
###########

# Wat is de kans dat men hercalibreert als dit niet nodig is?
# --> type I fout
# --> alfa
alfa(0.08)


###########
# 12.1 b) #
###########

# Wat is de kans dat men niet hercalibreert terwijl mu=10.1?
#   = Kans op H0 aanvaarden terwijl H0 vals is
#   = Kans op type II fout gegeven mu=mu_1=10.1
pnorm(-0.02, mean=0, sd=0.2/5) - pnorm(-1.18, mean=0, sd=0.2/5)


###########
# 12.1 c) #
###########

# Welke afwijking delta = |mu_1-mu_o| zal met kans 95% ontdekt worden?
#   --> We willen de power (1-beta) berekenen voor mu_1
#   Beta is de kans op H0 aanvaarden terwijl H1 geldt: mu=mu_1
#   De power moet 95% zijn, dus we zoeken mu_1 zodat beta gelijk is aan 0.05
mu_1 = 10.08 + qnorm(0.05)*0.04; mu_1


###########
# 12.1 d) #
###########

# Wat is de vereiste steekproefgrootte n zodat de kans om mu=10.1
# te ontdekken gelijk is aan 95%?

# We willen met andere woorden een onderscheidingsvermogen van 95% voor mu=mu_1=10.1
# --> beta moet gelijk zijn aan 5%
(-0.08-10.1+10)
(0.08-10.1+10)
n = ( -qnorm(0.05)*(-0.2/0.02) )^2; n

# Betrouwbaarheid in functie van de getolereerde fout d   ( |x_-10| >= d )
plot(betrouwbaarheid)





########
# 12.2 #
########

pnorm(180, mean=182, sd=1/sqrt(24))


########
# 12.3 #
########

t_obs = abs( (194-200)/(12/sqrt(30)) )
2*( 1 - pt ( t_obs, df=29 ) )

n = ( 2*(qnorm(0.95)-qnorm(0.025)) )^2; n


########
# 12.4 #
########

z_obs = (18.12-16.87)/sqrt((1.6^2/40)+(1.4^2/32))
1-pnorm(z_obs)

breuk = 1/sqrt((1.6^2/40)+(1.4^2/32))
pnorm(qnorm(0.99)-breuk) - pnorm(qnorm(0.01)-breuk)

n_2 = 1.4^2/( (1/(qnorm(0.95)-qnorm(0.1)))^2 - 1.6^2/40 )
