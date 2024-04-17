load("~/Documents/Code/KULAK_statistiek/OZ9/pollutie.RData")

attach(pollutie)

#### {E\'en gemiddelde of gepaarde groepen}
# Voorbeeld: Verschilt de gemiddelde januaritemperatuur in de U.S.A significant van het vriespunt?
mean(JanTemp)
sd(JanTemp)
length(JanTemp)
t = mean(JanTemp)/sd(JanTemp)*sqrt(length(JanTemp)); t
qt(.975, 59)
2*pt(-abs(t),length(JanTemp)-1)
t.test(JanTemp, mu=0, alternative="two.sided") # two sided is standaard want eerste in help file c()

# Voorbeeld: Is de gemiddelde julitemperatuur in de U.S.A significant hoger dan de gemiddelde januaritemperatuur?
mean(JanTemp)
mean(JulyTemp)
mean(JanTemp - JulyTemp)
sd(JanTemp - JulyTemp)
length(JanTemp - JulyTemp)
qt(.05,59)
t.test(JanTemp, y=JulyTemp, alternative="less",
       paired=TRUE)
t.test(JanTemp-JulyTemp, mu=0, alternative="less")

#### {Twee gemiddelden, ongepaarde groepen}
# Voorbeeld: Is er een significant verschil tussen de gemiddelde temperatuur in januari in de noordelijke en noordoostelijke regio van de U.S.A.?
table(regio)
tapply(JanTemp, regio, mean)
tapply(JanTemp, regio, sd)
shapiro.test(JanTemp[regio=="N"])
shapiro.test(JanTemp[regio=="NO"])
var.test(JanTemp[regio=="N"], JanTemp[regio=="NO"])
t.test(JanTemp[regio=="N"], y=JanTemp[regio=="NO"],
       paired=FALSE, var.equal=FALSE)

# Voorbeeld: Is er een significant verschil tussen het mediane inkomen in steden van de noordelijke en noordoostelijke regio van de U.S.A.?
tapply(income, regio, mean, na.rm=TRUE)
tapply(income, regio, sd, na.rm=TRUE)
shapiro.test(income[regio=="N"])
shapiro.test(income[regio=="NO"])
t.test(income[regio=="N"], y=income[regio=="NO"],
       paired = FALSE, var.equal=FALSE)

# Voorbeeld: Is er een significant verschil tussen de gemiddelde temperatuur in januari in de noordelijke en de centrale regio van de U.S.A.?
wilcox.test(JanTemp[regio=="N"], y=JanTemp[regio=="C"],
            alternative="two.sided", paired=FALSE)

detach(pollutie)
