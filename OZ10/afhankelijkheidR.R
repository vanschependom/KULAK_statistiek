load("pollutie-gegevens.RData")

attach(pollutie)

# Voorbeeld: Volgt aantal steden in steekproef populatiedistributie?
dichtheid = c(.26 ,.11, .16, .30, .17)
table(regio)
dichtheid*60
chisq.test(table(regio),p=dichtheid)
test = chisq.test(table(regio),p=dichtheid)
attributes(test)
test$observed
test$expected
test$residuals

#### {Test voor afhankelijkheid}
# Voorbeeld: Is er significante samenhang tussen de hoeveelheid arbeiders en het inkomen?
chisq.test(arbeid, inkomen)
chisq.test(arbeid, inkomen)$expected
# --> we moeten hercoderen!!
arbeid2 = cut(100-X.WC, c(0,55,100),
              labels=c("laag2","hoog2"), ordered_result=TRUE)
inkomen2 = cut(income, c(0,30000,35000,Inf),
               labels=c("1","2","3-4"), ordered_result=TRUE)
chisq.test(arbeid2, inkomen2)$expected
chisq.test(arbeid2, inkomen2)
# --> we kunnen hier NIETS MEE DOEN!!
# --> de p-waarde is niet beduidend genoeg
# --> ligt TE DICHT BIJ DE GRENS
chisq.test(arbeid2, inkomen2)$residuals

# Voorbeeld: Is er ook in de noordoostelijke regio significante samenhang tussen de hoeveelheid arbeiders en het inkomen?
x = arbeid[regio=="NO"]; y = inkomen[regio=="NO"]
chisq.test(x,y)$expected
fisher.test(x,y)

# Voorbeeld: Is er significante samenhang tussen januari- en julitemperatuur?
plot(JanTemp, JulyTemp)
cor(JanTemp, JulyTemp)
cor(JanTemp, JulyTemp, method = c("spearman"))
cor(rank(JanTemp),rank(JulyTemp))
shapiro.test(JanTemp)
shapiro.test(JulyTemp)
cor.test(JanTemp, JulyTemp, method = c("spearman"))

#### {Test voor proporties}
# Voorbeeld: Verschilt de proportie steden met een gemiddelde hoeveelheid arbeiders significant van 50%?
table(arbeid)
length(arbeid)
binom.test(46, 60, p=0.50, alternative="two.sided")
2*(1-pbinom(45, 60, .5))

# Voorbeeld: Is er een significant verschil tussen de proportie steden met een gemiddeld aantal arbeiders in de  noordoostelijke regio en de rest van de U.S.A.?
table(arbeid = arbeid=="gemiddeld",regio =  regio=="NO")
prop.test(c(17,29), c(24,36), alternative="two.sided")
chisq.test(table(arbeid = arbeid=="gemiddeld",regio =  regio=="NO"))

detach(pollutie)

