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


####################
# Oefening '18-'19 #
####################

p = 1-ppois(27,lambda=30)

pbinom(10, size=20, p)


