# X = gemiddelde bruto maandloon in Belgie
# X ~ N(mu, sigma^2)

mu = 2837

#     F_z( (1807-2837)/sigma ) = 0.1
# <=> (1807-2837)/sigma = Q_z(0.1)
# <=> sigma = (1807-2837) / Q_z(0.1)
sigma = (1807-2837) / qnorm(0.1)

# controle
qnorm(0.1,mu,sigma) # effectief gelijk aan 1807.

# IQR
iqr = qnorm(0.75,mu,sigma) - qnorm(0.25,mu,sigma)
