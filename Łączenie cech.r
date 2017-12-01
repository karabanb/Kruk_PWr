
ParyCech <- function(modelowana,ramka,zmienne,poziom,deviancja.wektor)

 {

    #modelowana <- 'Jakosc'
    #ramka <-uczacy
    #zmienne <- names(uczacy)[c(-1,-2,-3,-4)]
    #poziom=2

    poziom = 2^poziom

    #i=25
    #j=62

    pary <- matrix (0,length(zmienne)*length(zmienne)/2,length(deviancja.wektor))

    for (l in 1 : length(deviancja.wektor))
    {
        deviancja <- deviancja.wektor[l]

        k <- 1

        liczba.zmiennych <- length(zmienne)

       for (i in 1:(liczba.zmiennych-1))
          {

          zmienna1 <- zmienne[i]

            for (j in (i+1):liczba.zmiennych)
                 {

                  zmienna2 <- zmienne[j]

                  formula <- as.formula(paste("as.factor(",modelowana,")","~",zmienna1,"+",zmienna2))

                  tree <- tree(formula,data=ramka,
                  mincut = 50,
                  mindev=deviancja)

                  x <- as.matrix(tree$frame)

                  x <- as.numeric(dimnames(x)[[1]])

                  #rm(nazwy_z_drzew)

                  nazwy_z_drzew <- tree$frame[ x <= poziom,"var"]

                  nazwy_z_drzew <- unique(nazwy_z_drzew)

                  nazwy_z_drzew <- nazwy_z_drzew[nazwy_z_drzew!="<leaf>"]

                  nazwy_z_drzew <- nazwy_z_drzew[is.na(nazwy_z_drzew) == FALSE]

                  liczba.cech.z.drzewa <- length(nazwy_z_drzew)

                  if  (liczba.cech.z.drzewa == 2)
                       {
                         pary[k,l] <- paste(nazwy_z_drzew[1],"+",nazwy_z_drzew[2])

                        k = k + 1

                       }
                  }
          }
      }
          pary
      
}



PolonczenieCech <- function(modelowana,ramka,Cechy,deviancja)
{

    #modelowana <- 'Jakosc'
    #ramka <-uczacy
    #Cechy <- cechy[cechy[,3] != "0",3]

  liczba.zmiennych <- length(Cechy)

      #i=1

  for (i in 1:(liczba.zmiennych))
    {
        zmienna <- Cechy[i]

                  formula <- as.formula(paste("as.factor(",modelowana,")","~",zmienna))

                  tree <- tree(formula,data=ramka,
                  mincut = 50,
                  mindev = deviancja)

                  ramka[,paste('band',i,sep = "")] <- as.factor(tree$where)

    }
    
    ramka
}

Drzewo_do_par  <- function(modelowana,ramka,Cechy,waga,deviancja,numer)
{
  zmienna <- Cechy[numer]    

  w <<- c(ramka[,waga])

  formula <- as.formula(paste("as.factor(",modelowana,")","~",zmienna))

  tree <- rpart( formula
                ,ramka
                ,weights = w
                ,cp = deviancja
                ,minbucket = sum(w)*.01 )

  draw.tree(tree,cex=0.7,nodeinfo=TRUE)
  
  tree

}













































 #       a1 <- c(1:length(names(ramka)))[names(ramka)==nazwy_z_drzew[1]]
 #
 #       if ( length(unique(ramka[,a1])) > 20 ) {

 #            a <- tree$frame$splits[tree$frame$var == nazwy_z_drzew[1],1]
 #
 #            z = as.numeric(c(substr(a,2,nchar(a)),min(ramka[,a1]),max(ramka[,a1])))

 #           z <- sort(z)

 #           breaks <- z

 #           band1 <- cut(ramka[,a1],breaks=breaks,include.lowest=T)

 #         } else {

 #           band1 <- ramka[,a1]

 #         }

  #      a2 <- c(1:length(names(ramka)))[names(ramka)==nazwy_z_drzew[2]]

  #      if ( length(unique(ramka[,a2])) > 20 )
  #      {

  #         a <- tree$frame$splits[tree$frame$var == nazwy_z_drzew[2],1]

  #           z = as.numeric(c(substr(a,2,nchar(a)),min(ramka[,a2]),max(ramka[,a2])))

  #          z <- sort(z)

   #         breaks <- z

   #         band2 <- cut(ramka[,a2],breaks=breaks,include.lowest=T)

   #       } else {

   #         band2 <- ramka[,a1]

   #       }
   
   
   

 # nazwy_z_drzew <- tree$frame[ x <= poziom,"var"]
  #      nazwy_z_drzew <- unique(nazwy_z_drzew)
   
   #     nazwy_z_drzew <- unique(nazwy_z_drzew[nazwy_z_drzew!="<leaf>"])
    #    nazwy_z_drzew <- unique(nazwy_z_drzew[is.na(nazwy_z_drzew) == FALSE])

         #ramka[,paste(names(ramka)[names(ramka)==nazwy_z_drzew[1]],"_",names(ramka)[names(ramka)==nazwy_z_drzew[2]])] <- as.factor(tree$where)

