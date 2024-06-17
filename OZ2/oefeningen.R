# 1. we laden de data in
cars <- read.delim2("~/Documents/Code/KULAK_statistiek/OZ2/cars.dat", na.strings="*", stringsAsFactors = TRUE)

# 3.
# alles is in inches, 1 inch = 0.0254m
cars$length = 0.0254 * cars$length
cars$width = 0.0254 * cars$width
# pounds / 2.205 = kg
cars$weight = cars$weight / 2.205
# sanity check
summary(cars$weight)
summary(cars$width)
summary(cars$length)
# miles per gallon to liter per 100km
cars$city = 235.214583/cars$cityMPG
cars$hghwy = 235.214583/cars$hghwyMP
mean(cars$LPOK)
cars$cityMPG = NULL
cars$hghwyMP = NULL

class(cars$drivetr) # niet juist, want dit is een categorische veranderlijke
cars$drivetr = factor(cars$drivetr, levels=c(0,1,2), label=c("rear", "front", "all"))
cars$transm = factor(cars$transm, levels=c(0,1), label=c("auto", "manual"))

# het bedrag van de opties is gelijk aan de maximum- min de minimumprijs
cars$opties = cars$maxpr - cars$minpr

attach(cars)

# 6.
plot(minpr,length)
plot(minpr,width)
plot(minpr,weight)

# 7.
plot(minpr,opties)
model[opties > minpr] # opties duurder dan de minprice

# 8.
plot(hghwy,city)
boxplot(hghwy,city)
model[hghwy>city]

# 9.
table(type)
means = tapply(horsepo, type, mean, na.rm=TRUE)
levels(type)
array(means)

