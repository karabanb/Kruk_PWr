
#Information Value

# autor: Leszek Sroka
# data utworzenia: 30.05.2011

# Generuje wykres rozk³adu oraz struktury cechy w podziale na dodatkowa cecha
# input:
#       Ramka.danych - objekt data frame 
#       zmienna.modelowana - nazwa kolumny ze zmienna modelowana (char)
#       klasa.modelowana - nazwa klasy modelowanej (char) 
#       zmienna.objasniajaca - nazwa zmiennej objasniajacej (char)

InformationValue <- function(Ramka.danych,zmienna.modelowana,klasa.modelowana,zmienna.objasniajaca)
  
{
  
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
  
  
  apom <- table(zmienna.objasniajaca,zmienna.modelowana)
  a <- apom[!(apom[,1]==0 & apom[,2]==0),]
  
  wsz.dobrzy=nrow(Ramka.danych[zmienna.modelowana==klasa.modelowana,])
  wsz.zli= nrow(Ramka.danych[zmienna.modelowana!=klasa.modelowana,])
  
  proc.dobrych=a[,2]/wsz.dobrzy
  proc.zlych=a[,1]/wsz.zli
  
  IV = sum((proc.dobrych - proc.zlych)*log(proc.dobrych / proc.zlych))
  
  print(IV)
  
}