

set.seed(1234)

library(tidyverse)  
library(caret)      
library(rpart)      
library(rpart.plot)
library(partykit)
library(data.table)


####### ładowanie danych ########

load("KrukPWr2018.Rdata")

#######  Eksploracyjna Analiza Danych ##########################################################

############### ćwiczenie #######################

## sprawdz rozmiar ramek danych, wystepowanie NA,s, typy danych dla obiektow 'events' i 'cases'






####### Usuniecie brakow danych ##############################################################

############### ćwiczenie #######################

#znajdz metoda zastapienia brakow danych mediana

cases<-as.data.frame(cases)
ncol(cases)


# po zamianie sprawdz ramke danych poprzez summary
#summary(cases)

# zaproponuj metode postepowania uzupelnienia brakow danych kategorycznych

events <- as.data.frame(events)

# zamien wszytskie wartosci NA na 0 

# sprawdz podumowanie danych po wykonaniu tej operacji

#################################################


##### przekodowanie do factora #################

cases <- cases%>%
  mutate_if(., is.character, as.factor)%>%
  mutate(., Land = as.factor(Land))


####### Definiowanie Y ##########################

#### grpujemy wszystkie zdarzenia po CaseId w celu utworzenia cech:
#### if_concluded - czy zawarta ugoda w ciagu 12M
#### if paid - czy byla jakoklwiek wplata w ciagu 12M

events <- events %>%
  group_by(., CaseId)%>%
  summarise(., if_concluded = sum(NumberOfAgreementConcluded),
            if_paid = sum(PaymentAmount)
           )


############### ćwiczenie #########################
###### sprawdz strukture po przeksztalceniu danych

## zamien wartosci w cechach if_concluded i if_concludedna wartosc 0 jezeli nie bylo wplaty lub zawartej ugody 
## lub na 1 w przeciwnym przypadku


## zakoduj analogicznie na cechy factorowe 1=='good', 0 == 'bad' i zapisz je jako:
## if_concluded_fctr i 
## if_concluded_fctr


####### Laczenie danych ###########################

############### ćwiczenie #########################
# polacz zbiory danych case i events i zachowaj nowy zbior jako raw.data , sprawdz rozmiar ramek 




######## Podzial na zbior uczacy i testowy ########

set.seed(1234)
ind.ucz<-createDataPartition(raw.data$if_concluded, times= 1, p=0.7, list=FALSE)

walidacyjne <- raw.data[-ind.ucz, ] 
uczace <- raw.data[ind.ucz, ]

ind.ucz <- createDataPartition(uczace$if_concluded, times= 1, p=0.7, list=FALSE)

uczace <- uczace[ind.ucz,]
testowe <- uczace[-ind.ucz,]


############### ćwiczenie #########################

### sprawdz rozmiary ramek 'walidacyjne','uczace,'testowe'

### sprawdz rozklady 'if_concluded' we wszystkich trzech zbiorach

### zobacz rownierz frakcje 'if_paid'


######## modelowanie ##############################

### uzywamy 1 cechy TOA

(treey1c<- ctree(if_paid_fctr~ TOA ,uczace))

(treey1r <- rpart(if_paid_fctr ~ TOA, uczace))

(tree2r <- rpart(if_paid_fctr ~ TOA, uczace, control = rpart.control(cp =0.0001), method = "class"))


### predyckcja i macierz pomylek

pred.1c <- predict(treey1c, newdata = testowe, type = "response")

confusionMatrix(data = pred.1c, reference = testowe$if_paid_fctr)

#### ćwiczenie : utworz macierze pomylek modelu 'tree2r'  dla zbiorow 'uczacy' i 'testowy'



#### wszytskie dostepne cechy

uczace_t <- uczace%>%
  select(., -if_concluded, -if_concluded_fctr, -if_paid, if_paid_fctr, - CaseId)


############### ćwiczenie #########################
### wyswietl tresc pomocy dla funkcji rpart i rpart.control



tree3c <- ctree(if_paid_fctr~., uczace_t, control = ctree_control(minsplit = 1000))
tree4r <- rpart(if_paid_fctr~., uczace_t)

# parametryzacja 
tree5r <- rpart(if_paid_fctr~., uczace_t, control = rpart.control(cp =0.001, minsplit = 1000))


############### ćwiczenie #########################
# zbuduj kilka drzew metoda rpart z wybranymi przez Ciebie parametrami (max 3)


##### prezentacja graficzna 

plot(tree3c)
rpart.plot(tree4r)
rpart.plot(tree5r)

############### ćwiczenie #########################
# 'wydrukuj' drzewo z wybranymi przez Ciebie parametrami


##### macierze klasyfikacji

pred.3c <- predict(tree3c, newdata = testowe, type = "response")
confusionMatrix(data = pred.3c, reference = testowe$if_paid_fctr)

pred.4r <- predict(tree4r, newdata = testowe, type = "class")
confusionMatrix(data = pred.4r, reference = testowe$if_paid_fctr)

pred.5r <- predict(tree5r, newdata = testowe, type = "class")
confusionMatrix(data = pred.5r, reference = testowe$if_paid_fctr)


############### ćwiczenie #########################

### dodaj macierz klasyfikacji dla zbudowanych przez Ciebie drzew

### porownaj wybrany przez Ciebie model drzewa z modelem regresji




