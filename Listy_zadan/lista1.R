library(data.table)

#! Zadanie 1
tab <- data.table(U=runif(10000), Z=rnorm(10000))

tabTheSame <- tab 
tabCopy <- copy(tab)
tabTheSame[, One:=1]

head(tab, 3) # jest kolumna one
head(tabCopy, 3) # nie ma kolumny one

#! Zadanie 2
tabCopy[, list(
  AvgU=mean(U),
  StdU=sd(U),
  AvgZ=mean(Z),
  StdZ=sd(Z))]

tabCopy[, Ben:=ifelse(runif(10000) < 0.3, 0, 1)]

tabCopy[, list(
  Count=.N,
  AvgU=mean(U),
  StdU=sd(U),
  AvgZ=mean(Z),
  StdZ=sd(Z)), by=Ben]

#! Zadanie 3
tab3 <- data.table(
  Id=1:10, 
  Age=sample(23:75, 10), 
  Gender=sample(c("F", "M"), 10, replace=TRUE))

# ustaw klucze/indexy  
setDT(tab3, key="Id")

tabCopy[, Id:=rep(1:10, each=1000)]
setDT(tabCopy, key="Id")

# join po kluczu
tabCopy[tab3]

# join po wskazanej kolumnie (musi mieć takš samš nazwę w obu tabelach)
tabCopy[tab3, on="Id"]

tabCopy[tab3][, list(
  Count=.N,
  AvgAge=mean(Age),
  StdAge=sd(Age),
  AvgU=mean(U),
  StdU=sd(U),
  AvgZ=mean(Z),
  StdZ=sd(Z)), by=list(Id, Gender)]

#! Zadanie 4
tab4 <- tabCopy[tab3]

n <- 10

for (i in 1:n) { # użyteczna konstrukcja
  expr <- paste0("tab4[, N", i, ":=rnorm(10000)]")
  eval(parse(text=expr))
}

#! Zadnaie 5
cols <- c("N3", "N6", "N9")
kol0_5 <- "U"

# parametry .SD i .SDcols
tab4[, .SD, .SDcols=c("Id", "Age", "Gender", cols)]

# funkcja get()
tab4[get(kol0_5) < 0.5, .SD, .SDcols=c("Id", "Age", "Gender", cols)]
