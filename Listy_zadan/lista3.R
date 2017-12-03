


library(mice)

library(VIM)

getwd()
setwd("...")

load("KrukUWr2017.Rdata")


names(cases)
names(events)

dim(cases)
dim(events)


##ZAD 0.  Stwórz nowš ramkę danych events1 bazujšc na ramce events, która będzie zawierała tylko dane z pierwszego miesišca.

#bierzemy tylko zdarzenia z pierwszego miesiaca
events1<-events[which(events$Month==1),]
summary(events1)
dim(events1)

#usuwanie rekordow z brakami
#na.omit(sprawy)

dim(cases)
dim(na.omit(cases)) 

#przydatna funkcja

df_status<-function (data){ 
  df = data.frame(
    type = sapply(data, class),
    q_na = sapply(data, function(x) sum(is.na(x))), ##iloć braków
    p_na = round(100 * sapply(data, function(x) sum(is.na(x)))/nrow(data),2), ##procent braków
    q_zeros = sapply(data, function(x) sum(x == 0, na.rm = T)),  ##iloć 0
    p_zeros = round(100 * sapply(data, function(x) sum(x == 0, na.rm = T))/nrow(data),2), ##procent 0
    q_1 = sapply(data, function(x) sum(x == -1, na.rm = T)),  ##iloć -1
    p_1 = round(100 * sapply(data, function(x) sum(x == -1, na.rm = T))/nrow(data),2), ##procent -1
    unique = sapply(data,function(x) sum(!is.na(unique(x))))
  )
  df$variable = rownames(df)
  rownames(df) = NULL
  df= df[,c(9, 1:8)]
}


### ZAD 1. - Rozpoznaj czy i w jakich zmiennych występujš braki danych w podanych zbiorach cases i events1
### ROZW 1
summary(cases)


unique(cases$Product) 
unique(cases$Gender) 

length(cases$Gender)
length(na.omit(cases$Gender))

summary(events1) 



###-------------------------------------------------------------------
### ZAD 2. - Czy w podanych zbiorach danych sa zmienne dla ktorych uzupelnianie brakow nie ma sensu 
###          lub mozna je pominÄc ze wzgledu na wnoszenie zbyt maĹej informacji do danych?
### ROZW 2

(status.sprawy<-df_status(cases))



(status.events<-df_status(events1))




#####------------------------------------------------- 
### ZAD 3. - Wskaż zmienne (pary zmiennych), dla których braki danych sš ze sobš powišzane i uzasadnij swojš odpowied  najlepiej liczbowo i słownie 
### ROZW 3


# plec i wiek - bo sa obliczane z peselu
sum(cases$Age == -1)    #1520
sum(is.na(cases$Gender))#1520
sum(is.na(cases$Gender)==TRUE & cases$Age==-1)#1520



pattern<-md.pattern(cases)
dim(pattern) 


png("pattern.png",width = 700,height = 500)
aggr_plot <- aggr(cases, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(cases), cex.axis=.5, gap=3, ylab=c("Histogram of missing data","Pattern"))
dev.off()


#####---------------------------------------------------------------
### ZAD 4. - Uzupełnij braki danych dla zmiennych LoanAmount, Wiek i Plec poprzez uzupełnienie redniš, medianš, modš, wartociš stałš. Co zauważasz?

### ROZW 4

#LoanAmount

LoanAmount.mean<-ifelse(is.na(cases$LoanAmount),mean(cases$LoanAmount, na.rm = T),cases$LoanAmount)
LoanAmount.median<-ifelse(is.na(cases$LoanAmount),median(cases$LoanAmount, na.rm = T),cases$LoanAmount)

##moda 
LoanAmount.count<-table(na.omit(cases$LoanAmount))
LoanAmount.count[which(LoanAmount.count==max(LoanAmount.count))]
LoanAmount.mode<-ifelse(is.na(cases$LoanAmount),1000,cases$LoanAmount)

##wartoć stała
LoanAmount.const<-ifelse(is.na(cases$LoanAmount),-1,cases$LoanAmount)


summary(cases$LoanAmount)
summary(LoanAmount.mean)
summary(LoanAmount.median)
summary(LoanAmount.mode)
summary(LoanAmount.const)


#AGE: w tej chwili braki dla wieku to -1 czyli sa zastapione stala wartoscia
summary(cases$Age)
length(cases$Age)

#co sie stanie jesli beda to nulle
Age.null<-ifelse(cases$Age!=-1,cases$Age, NA)
Age.mean<-ifelse(is.na(Age.null),mean(Age.null,na.rm=TRUE),Age.null)
Age.median<-ifelse(is.na(Age.null),median(Age.null,na.rm=TRUE),Age.null)

Age.count<-table(na.omit(Age.null))
Age.count[which(Age.count==max(Age.count))]
Age.mode<-ifelse(is.na(cases$Age),36,cases$Age)


summary(Age.null)
summary(cases$Age)
summary(Age.mean)
summary(Age.median)
summary(Age.mode)



#GENDER
summary(cases$Gender) 

class(Gender2)
Gender2<-cases$Gender
Gender2[is.na(Gender2)]<-"brak" 
unique(Gender2)

##Gender2<-factor(Gender2,levels =c(unique(Gender2),"brak"))
##Gender2[is.na(Gender2)]<-"brak"


Gender.count<-table(cases$Gender)
Gender.count[which(Gender.count==max(Gender.count))]
Gender.mode<-ifelse(is.na(cases$Gender),"MALE",cases$Gender)
table(Gender.mode)
table(cases$Gender)


##-------------------------------------------
### ZAD 5 Uzupelnianij braki danych zmiennych LoanAmount, Bailiff, Gender z ich rozkladĂłw
#         Porownaj otrzymane wyniki z pierwotnymi danymi (histogram,gesosc,boxplot lub cokolwiek)
### ROZW 5


sample.missing.value<-function(data,feature.name){
  
  missing<-which(is.na(data[,feature.name]))
  data[missing,feature.name]<-sample(data[-missing,feature.name],size = length(missing),replace = TRUE)
  return(data[,feature.name])
}

names(cases)
sprawy.uzup<-cases #tabelka do uzupelniania

missing<-which(is.na(cases[,"LoanAmount"]))



#LoanAmount
sprawy.uzup$LoanAmount<-sample.missing.value(sprawy.uzup,"LoanAmount")

summary(sprawy$LoanAmount)
summary(sprawy.uzup$LoanAmount)


#Bailiff
sprawy.uzup$Bailiff<-sample.missing.value(sprawy.uzup,"Bailiff")

summary(sprawy$Bailiff)
summary(sprawy.uzup$Bailiff)

#Gender
sprawy.uzup$Gender<-sample.missing.value(sprawy.uzup,"Gender")

table(sprawy$Gender)/length(na.omit(sprawy$Gender))
table(sprawy.uzup$Gender)/length(na.omit(sprawy.uzup$Gender))


