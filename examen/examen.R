x = seq(from=1,to=30,by=0.01)
plot(x, pexp(x, 1/8), type="line")

# kans op 20 jaar geen elfstedentocht
1-pexp(20,1/8)

# kans op dit jaar een elfstedentocht
pexp(1,1/8)

# in welk jaar met 95 procent kans elfstedentocht zonder klimaatverandering
qexp(0.95,1/8)
