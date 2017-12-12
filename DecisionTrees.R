

set.seed(1234)

library(tidyverse)  
library(caret)      
library(rpart)      
library(rpart.plot)
library(partykit)
library(data.table)


####### ładowanie danych ########

load("KrukPWr2018.Rdata")

#######  Eksploracyjna Analiza Danych ###########
str(uczace)

##summary(cases)
##glimpse(cases)
##dim(cases)
# tak samo dla events

prop.table(table(testowe$if_concluded))


####### Usuniecie brakow danych #################

#znajdz metoda zastapienia brakow danych mediana

# land to factor i nie mozna mediana

cases[is.na(cases$Land), "Land"] <- 0 

cases<-as.data.frame(cases)
ncol(cases)
for(i in 1:ncol(cases)){
  med<-median(na.omit(cases[,i]))  
  cases[is.na(cases[,i]),i]<-med
} 

# po zamianie sprawdz ramke danych poprzez summary
#summary(cases)

# zaproponuj metode postepowania uzupelnienia brakow danych kategorycznych

cases[is.na(cases$Product), "Product"] <- names(which.max(table(cases$Product)))
cases[is.na(cases$Gender), "Gender"] <- names(which.max(table(cases$Gender)))
cases$Land <- as.factor(cases$Land)


events <- as.data.frame(events)

summary(events)

# zamien wszytskie wartosci NA na 0 

for(i in 1:ncol(events)){
  events[is.na(events[,i]),i] <- 0
} 

# sprawdz podumowanie danych po wykonaniu tej operacji

summary(events)

##### przekodowanie do factora #################

cases <- cases%>%
          mutate_if(., is.character, as.factor)


####### Definiowanie Y ##########################

#### grpujemy wszystkie zdarzenia po CaseId w celu utworzenia cech:
#### if_concluded - czy zawarta ugoda w ciagu 12M
#### if paid - czy byla jakoklwiek wplata w ciagu 12M

events <- events %>%
            group_by(., CaseId)%>%
            summarise(., if_concluded = sum(NumberOfAgreementConcluded),
                         if_paid = sum(PaymentAmount)
                      )
            # mutate(., if_concluded = ifelse(if_concluded > 0, 1, 0),      # Czy zawarta ugoda 12M
            #           if_paid = ifelse(if_paid > 0, 1, 0))                # Czy jakakolwiek wplata 12M

# zamien wartosci w cechach if_concluded i if_paid na wartosc 0 jezeli nie bylo wplaty lub zawartej ugody 
# lub na 1 w przeciwnym przypadku

events$if_concluded <- ifelse(events$if_concluded > 0, 1, 0)
events$if_paid <- ifelse(events$if_paid > 0, 1, 0)


events$if_concluded_fctr <- as.factor(ifelse(events$if_concluded==0, 'bad', 'good'))
events$if_paid_fctr <- as.factor(ifelse(events$if_paid==0, 'bad', 'good'))



####### Laczenie danych ###########################

raw.data <- inner_join(cases, events)

#save(raw.data, file="raw.data.Rdata")

# zakoduj analogicznie za cechy factorowe 1=='good', 0 == 'bad'

dim(raw.data)
str(raw.data)


######## Podzial na zbior uczacy i testowy ########

set.seed(1234)
ind.ucz<-createDataPartition(raw.data$if_concluded, p=0.7, list=FALSE)

walidacyjne <- raw.data[-ind.ucz, ] 
uczace <- raw.data[ind.ucz, ]

ind.ucz <- createDataPartition(uczace$if_concluded, times= 1, p=0.7, list=FALSE)

uczace <- uczace[ind.ucz,]
testowe <- uczace[-ind.ucz,]


### sprawdz rozmiary ramek 'walidacyjne','uczace,'testowe'

dim(walidacyjne)
dim(uczace)
dim(testowe)
dim(cases)
dim(events)
#save(uczace, walidacyjne, testowe, cases, events, file = "KrukPWr2018.Rdata")

  ### sprawdz rozklady 'if_concluded' we wszystkich trzech zbiorach

summary(uczace$if_concluded)
summary(testowe$if_concluded)
summary(walidacyjne$if_concluded)

### zobacz rwonierz rozklady 'if_paid'

summary(uczace$if_paid)
summary(testowe$if_paid)
summary(walidacyjne$if_paid)


######## modelowanie ##############################

### uzywamy 1 cechy TOA

(treey1c<- ctree(if_paid_fctr~ TOA ,uczace))

(treey1r <- rpart(if_paid_fctr ~ TOA, uczace))

(tree2r <- rpart(if_paid_fctr ~ TOA, uczace, control = rpart.control(cp =0.0001), method = "class"))


### predyckcja i macierz pomylek

pred.1c <- predict(treey1c, newdata = testowe, type = "response")

confusionMatrix(data = pred.1c, reference = testowe$if_paid_fctr)

pred.2r <- predict(tree2r , newdata = testowe, type = "class")

confusionMatrix(data = pred.2r, reference = testowe$if_paid_fctr)

#### wszytskie dostepne cechy

uczace_t <- uczace%>%
            select(., -if_concluded, -if_concluded_fctr, -if_paid, if_paid_fctr, - CaseId)


(tree3c <- ctree(if_paid_fctr~., uczace_t, control = ctree_control(minsplit = 1000)))
(tree4r <- rpart(if_paid_fctr~., uczace_t))

# parametryzacja 

(tree5r <- rpart(if_paid_fctr~., uczace_t, control = rpart.control(cp =0.001, minsplit = 1000)))

# zbuduj drzewo z wybranymi przez Ciebie parametrami

(tree6r <- rpart(if_paid_fctr~., uczace_t, control = rpart.control(cp = 0.001, minsplit = 100)))

##### prezentacja graficzna 

plot(tree3c)
rpart.plot(tree4r)
rpart.plot(tree5r)
rpart.plot(tree6r)


##### macierze klasyfikacji

pred.3c <- predict(tree3c, newdata = testowe, type = "response")
confusionMatrix(data = pred.3c, reference = testowe$if_paid_fctr)

pred.4r <- predict(tree4r, newdata = testowe, type = "class")
confusionMatrix(data = pred.4r, reference = testowe$if_paid_fctr)

pred.5r <- predict(tree5r, newdata = testowe, type = "class")
confusionMatrix(data = pred.5r, reference = testowe$if_paid_fctr)

### dodaj macierz klasyfikacji dla zbudowanego przez Ciebie drzewa

pred.6r <- predict(tree6r, newdata = testowe, type = "class")
confusionMatrix(data = pred.6r, reference = testowe$if_paid_fctr)

#### koniec


