pollutie <- read.delim("~/Documents/Code/KULAK_statistiek/OZ1/pollutie.txt", na.strings="")
pollutie$JanTemp = 5/9 * (pollutie$JanTemp-32)    # omzetten van F naar C
pollutie$JulyTemp = 5/9 * (pollutie$JulyTemp-32)  # omzetten van F naar C
pollutie$Temp = (pollutie$JanTemp + pollutie$JulyTemp) / 2

pollutie[ order(pollutie$JanTemp) , 2 ]
pollutie[ order(0-pollutie$JanTemp) , c(1,2)]
pollutie[ order(pollutie$JanTemp,decreasing=TRUE) , c(1,2) ]
pollutie[ pollutie$JanTemp > 5 & pollutie$JulyTemp < 25 , c(1,2,3) ]

