# OEFENING 4
rm(list=ls())


# X = aantal minuten vertraing
# X ~ exp(lambda)

lambda = - (1/6) * log(1-0.889)

# controle
pexp(6,lambda)

# mediane vertraging = tweede kwartiel
qexp(0.5,lambda) # 1.89

# de kans dat een vertraging groter is dan 15 min
p = 1 - pexp(15,lambda)


# Y = aantal ritten met meer dan 15 minuten vertraging
# Y ~ bin(250,p)
n = 250

# P(Y>=20) = 1-P(Y<20) = 1-P(Y<=19)
1 - pbinom(19,n,p)


# DE BINOMIALE VERDELING IS NIET GOED
# !!! want het zijn geen onafhankelijke gebeurtenissen !!!

# we moeten dus voor elke specifieke lijn de vertragingskans weten