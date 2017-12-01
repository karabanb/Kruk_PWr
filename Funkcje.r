
# wczytanie biblotek

biblioteki <- function()
{
  library(rpart)

  library(tree)

  library("RODBC")

  library(maptree)

  library(party)

  library(car)
}

# polonczenie

polonczenie <- function(nazwa1,nazwa2)
{
  db <- odbcConnect("SATURN")

  uczacy <<- sqlQuery(db,nazwa1)

  testowy <<- sqlQuery(db,nazwa2)
}


# do drzewa

do_drzewa <- function(ramka,zmienna,arg,modelowana,waga)
{
  a <- ramka[,zmienna]
  nowa_ramka <- ramka[a != arg,]
  
  w = sum(nowa_ramka[,modelowana] == 1 )/sum(nowa_ramka[,modelowana] == 0 )
  
  nowa_ramka[nowa_ramka[,modelowana] == 0,waga] <- w
  
  nowa_ramka
  
}

# drzewo

drzewo <- function(modelow,zmienna,ramka,waga,deviancja,mnoznik)
{

  formula <- paste(modelow," ~ ",zmienna)
     
  w <<- c(ramka[,waga])  
           
  tree <- tree( formula
                       ,data = ramka
                       ,weights = w
                       ,mincut= sum(w)*.05*mnoznik
                       ,mindev=deviancja
                       )
 x <- length(tree$frame[,1])

if ( x > 1 ) {
  plot(tree)
  text(tree)
  }
  
tree
}

# przedzialy od drzewa

przedzialy <- function(tree,ramka,zmienna)
{

#ramka = nowa_ramka

x <- as.matrix(tree$frame)

#dimnames(x)[[1]]

spr <- max(c(as.numeric(dimnames(x)[[1]][abs(tree$frame$yprob[,2]-tree$frame$yprob[,1]) == 1]),-1))
trzy <- as.numeric(dimnames(x)[[1]][abs(tree$frame$yprob[,2]-tree$frame$yprob[,1]) == 1])

split <- floor(trzy/2)

if ( spr > 0 )  {
  a <- tree$frame$splits[-split,1]
 } else {
 a <- tree$frame$splits[,1]
 }
  
z = as.numeric(c(substr(a,2,nchar(a)),min(ramka[,zmienna]),max(ramka[,zmienna])))

z <- sort(z)

breaks <- z

band <- cut(ramka[,zmienna],breaks=breaks,include.lowest=T)

band

}

# przedzialy reczne

podzial.reczny <- function(ramka,zmienna,wektor)
{

breaks <- sort(wektor)

band <- cut(ramka[,zmienna],breaks=breaks,include.lowest=T)

band
}



#ROC

ROC <- function(model,uczacy,testowy,modelowana)
{

  scoring <- predict(model, uczacy, type = "response")

  min.score = min(scoring)
  max.score = max(scoring)
  scoringi.siatka = seq(from=min.score,to=max.score,by=0.01)

  scoringi.GOOD = scoring[uczacy[,modelowana]==1]
  scoringi.BAD = scoring[uczacy[,modelowana]==0]

  F.BAD <- ecdf(scoringi.BAD)
  F.B=F.BAD(scoringi.siatka)

  F.GOOD <- ecdf(scoringi.GOOD)
  F.G=F.GOOD(scoringi.siatka)

  plot(1-F.B,1-F.G,type="l",lwd=3,lty=1,xlab="F(s|BAD)",ylab="F(s|GOOD)",col="black",xlim=c(0,1),ylim=c(0,1))

  lines(x=c(0,1),y=c(0,1),lty=3,col="black",lwd=3)
  grid()
  title("ROC curve")

  scoring <- predict(model, testowy, type = "response")

  min.score_tst = min(scoring)
  max.score_tst = max(scoring)
  scoringi.siatka_tst = seq(from=min.score,to=max.score,by=0.01)

  scoringi.GOOD = scoring[testowy[,modelowana]==1]
  scoringi.BAD = scoring[testowy[,modelowana]==0]

  F.BAD_tst <- ecdf(scoringi.BAD)
  F.B_tst=F.BAD_tst(scoringi.siatka)

  F.GOOD_tst <- ecdf(scoringi.GOOD)
  F.G_tst=F.GOOD_tst(scoringi.siatka)
  
  lines(1-F.B_tst,1-F.G_tst,lwd=3,lty=2,col="red")
 
  legend(x="topleft",legend=c("zbior testowy","zbior uczacy"),lwd=3,col=c("red","black"),cex=1.2,lty=c(2,1)) 
  
}

# Histogram scoringow

wyk.histogramu <- function(model,ramka,modelowana,breaks)
{
  scoring <- predict(model, ramka, type = "response")
  
  par(mfrow=c(1,2))
  
  hist(scoring[ramka[,modelowana] == 1],lwd=3,lty=1,col="red",main = paste("Rozklad good"),breaks = breaks,xlab="Good")
  hist(scoring[ramka[,modelowana] == 0],lwd=3,lty=1,col="blue",main = paste("Rozklad bad"),breaks = breaks,xlab="Bad")
  
  par(mfrow=c(1,1))
  
}

#stabilnosc cech
cechy.stabilne <- function(ramka,modelowana,modelowane,liczba.petl,wielkosc)
{
  
  # ramka <- uczacy
  # modelowana <- "Jakosc"
  # modelowane <- names(uczacy)[c(-1,-2,-3,-4)]
  # liczba.petl <- 2
  # wielkosc <- 0.1
  # i <- 1
   
   for (j in 1:(liczba.petl))
   
   {
   
    i=sample(nrow(uczacy),wielkosc*nrow(uczacy))

    nowa_ramka <- uczacy[i,] 
  
    formulaa <- paste(modelowana,"~",paste(modelowane,collapse="+"))
     
    m <- glm(as.formula(paste("Jakosc","~1")), data = nowa_ramka)
    m <- step(object = m, scope = formulaa, direction = "forward", trace = 0)
    
      if ( j == 1 ) { 
        nazwy <- c(names(summary(m)$coefficients[,"Estimate"]))
      } else {
        nazwy <- c(nazwy,names(summary(m)$coefficients[,"Estimate"]))
      }
 
  }
  
  summary(data.frame(nazwy),maxsum = 100)
  
}



gini.coeff <- function(ramka,modelowana,model)
{
 n<-dim(ramka)[1]
 m <- 101
 cutoffs<-seq(0,1,length=m)
 probing.argument <- 0.8
  # modelowana = c("Jakosc")
  # ramka = uczacy
scores <- predict.glm(model,ramka,type="response")
wynik <- data.frame(ramka,scores)

scores.index<-which(names(wynik)=="scores")
target.variable.index<-which(names(wynik)==modelowana)

ramka[,scores.index] <- scores
 sensitivity.vector=vector(mode = "numeric", length = m)
 specificity.vector=vector(mode = "numeric", length = m)
 for(i in 1:m)   # i = 1
      {


                temp.scores<-as.integer(ramka[,scores.index]>=cutoffs[i])
                cm<-table(temp.scores,ramka[,target.variable.index] )
                if (dim(cm)[1]==1 && unique(temp.scores)==1)
                  {sensitivity.vector[i]=1}
                else if (dim(cm)[1]==1 && unique(temp.scores)==0)
                  {specificity.vector[i]=1}
                else
                {
                sensitivity.vector[i]=cm[2,2]/sum(cm[,2])  #poprawnosc klasyfikacji dobrych
                specificity.vector[i]=cm[1,1]/sum(cm[,1])  #poprawnosc klasyfikacji zlych
                }
        }

 gini.coefficient<-(trapezeIntegral(specificity.vector,sensitivity.vector)-0.5)*2
 probing.value<-sensitivity.vector[which(specificity.vector>=probing.argument)[1]]

gini.coefficient*100

 }

