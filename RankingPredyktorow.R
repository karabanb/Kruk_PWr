
# RankingPredyktorow
# autor: Leszek Sroka
# data utworzenia: 30.05.2011

# Generuje zestawienie zmiennych pod katem si³y predykcyjnej (Information Value)
# input:
#       Ramka.danych - objekt data frame 
#       zmienna.modelowana - nazwa kolumny ze zmienna modelowana (char)
#       klasa.modelowana - nazwa klasy modelowanej (char)
#table(Y)
#Ramka.danych=MSP.raw1
#zmienna.modelowana="Y" 
#klasa.modelowana="1"   
RankingPredyktorow <- function(Ramka.danych,zmienna.modelowana,klasa.modelowana)
  
{
  
  nazwy.zmiennych <- attr(Ramka.danych,"names")
  ile.zmiennych = dim(Ramka.danych)[2]
  
  zmienna.modelowana.index <- which(nazwy.zmiennych == zmienna.modelowana) 
  zmienna.modelowana = Ramka.danych[,zmienna.modelowana.index]
  
  
  #klasa.modelowana = 1
  wsz.dobrzy=nrow(Ramka.danych[zmienna.modelowana==klasa.modelowana,]) 
  wsz.zli= nrow(Ramka.danych[zmienna.modelowana!=klasa.modelowana,])
  
  b = matrix (0,ile.zmiennych,2)
  
  for (i in 1:ile.zmiennych)     
  {          # if (i==40 || i==190) next
     
    cat(paste("aktualna wartosc i to:",i,"\n"))
    zmienna.objasniajaca = Ramka.danych[,i]
    zmienna.objasniajaca.nazwa =  names(Ramka.danych)[i]
    
    #zmienna.objasniajaca=Wplaty_30_Data0    
    #zmienna.objasniajaca.nazwa =  names(Ramka.danych)[10]
    
    apom <- table(zmienna.objasniajaca,zmienna.modelowana)
    a <- apom[!(apom[,1]==0 & apom[,2]==0),]
    if (is.null(dim(a)) == TRUE) next
    proc.dobrych <- a[,2]/wsz.dobrzy
    proc.zlych <- a[,1]/wsz.zli 
    proc.dobrych[proc.dobrych==0] <- proc.zlych[proc.dobrych==0]
    proc.zlych[proc.zlych==0] <- proc.dobrych[proc.zlych==0]
    
    IV <- sum((proc.dobrych - proc.zlych)*log(proc.dobrych / proc.zlych))  
    
    
    #zrzut wyniku
    
    b[i,1] <- zmienna.objasniajaca.nazwa
    b[i,2] <- IV
  }
  
  bpom = b
  d=data.frame(bpom)
  d[order(-as.numeric(d$X2)),]
  
}

ranking.predyktorow<-function(ramka.danych,objasniana,klasa.modelowana=1,
                               wersja=list(c(2),c("org","Leszek","PM","Karas","R"))) 
{
  if( wersja[[1]] == 5 ) 
  {
    library(woe)
    output <- iv.mult(df=ramka.danych,y=objasniana,summary=T)
    names(output)[1] <- "cecha"
    return(output)
  }
  
  nazwy.cech <- names(ramka.danych[,!names(ramka.danych)%in%objasniana])
  IV=0
  
  for (i in 1:length(nazwy.cech)) 
  {
    IV[i] <- wartosc.informacyjna(ramka.danych,objasniana,klasa.modelowana
                                  ,nazwy.cech[i],wersja)
    cat(paste("wykonano:",i,", wszystkich ",length(nazwy.cech),"\n"))
  }
  output <- data.frame(cecha=as.character(nazwy.cech),IV=as.numeric(IV))
  
  return(output[order(output$IV,decreasing=T),])
}