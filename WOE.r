

WOE <- function(Ramka.danych,zmienna.modelowana,zmienna.objasniajaca,klasa.modelowana)

{

#Ramka.danych <- nowa_ramka
#zmienna.modelowana <- 'Jakosc'
#klasa.modelowana <- 1
#zmienna.objasniajaca <- 'band'


band <- zmienna.objasniajaca

par(mfrow=c(1,2))
 
ile.zmiennych = dim(Ramka.danych)[2]

 for (i in 1:ile.zmiennych)
 {
    if (names(Ramka.danych[i]) == zmienna.modelowana) k = i
 }
    zmienna.modelowana = Ramka.danych[,k]

 for (i in 1:ile.zmiennych)
 {
    if (names(Ramka.danych[i]) == zmienna.objasniajaca) l = i
 }
    zmienna.objasniajaca = Ramka.danych[,l]


a=table(zmienna.objasniajaca,zmienna.modelowana)

wsz.dobrzy=nrow(Ramka.danych[zmienna.modelowana==klasa.modelowana,])
wsz.zli= nrow(Ramka.danych[zmienna.modelowana!=klasa.modelowana,])

proc.dobrych=a[,2]/ wsz.dobrzy
proc.zlych=a[,1]/ wsz.zli

ylimmin = ((min((log(proc.dobrych/ proc.zlych))*100))%/% 50)*50
ylimmax = ((max((log(proc.dobrych/ proc.zlych))*100))%/% 50+1)*50

pom = 0.39*(max(nchar(levels(zmienna.objasniajaca)))) +3.12

var <- 1/a[,2] + 1/a[,1] - 1/wsz.dobrzy - 1/wsz.zli

#n <- a[,2] + a[,1]

par(mar=c(pom,4,4,2))


woe <- (log(proc.dobrych/ proc.zlych))*100

rys.woe <- barplot( woe,col="lightblue",las = 2, ylim = c(ylimmin,ylimmax),
        cex.axis = par("cex.axis"), ylab="WoE" )

points(x = rys.woe, y = woe,col="black",pch = 16)

points(x = rys.woe, y = woe + (sqrt(var))*100,col="black",pch = 24)

points(x = rys.woe, y = woe - sqrt(var)*100,col="black",pch = 25)
        
#points(x = rys.woe, y = woe + 1.64*(sqrt(var/n))*100,col="black",pch = 24)

#points(x = rys.woe, y = woe - 1.64*sqrt(var/n)*100,col="black",pch = 25)

barplot(summary(Ramka.danych[,band]), las=2, ylab = "number of observations")

par(mfrow=c(1,1))

woe
}

