# X_1 = uitkomst van een dobbelsteenworp

x1 = 0:6
dichth1 = rep(1/6,6)

# verwachtingswaarde
EX1 = sum(x1 * dichth1)

# variantie
VAR1 = sum( (x1-EX1)^2*dicht1 )


# X_2 = aantal kinderen per huishouden

x2 = 0:4
dichth2 = c(0.46,0.21,0.22,0.08,0.03)

# verwachtingswaarde
EX2 = sum(x2 * dichth2)

# variantie
VAR2 = sum( (x2-EX2)^2*dichth2 )
