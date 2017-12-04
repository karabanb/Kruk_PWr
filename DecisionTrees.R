

set.seed(1234)

library(tidyverse)  
library(caret)      
library(rpart)      
library(rpart.plot)
library(partykit)
library(xray)
library(data.table)


####### ładowanie danych ########

load("KrukUWr2018.Rdata")

#######  Eksploracyjna Analiza Danych ###########

cases.anom <- anomalies(cases)$variables
summary(cases)


####### Usuniecie brakow danych #################

cases<-as.data.frame(cases)
ncol(cases)
for(i in 1:seq_along(!is.character(cases))){
  med<-median(na.omit(cases[,i]))  
  cases[is.na(cases[,i]),i]<-med
} 

summary(cases)

events <- as.data.frame(events)

summary(events)

for(i in 1:ncol(events)){
  events[is.na(events[,i]),i] <- 0
} 

summary(events)

##### przekodowanie do factora #################

cases <- cases%>%
          mutate_if(., is.character, as.factor)


####### Definiowanie Y ##########################


events <- events %>%
            group_by(., CaseId)%>%
            summarise(., if_concluded = sum(NumberOfAgreementConcluded),
                         if_paid = sum(PaymentAmount)
                      )%>%
            mutate(., if_concluded = ifelse(if_concluded > 0, 1, 0),      # Czy zawarta ugoda 12M
                      if_paid = ifelse(if_paid > 0, 1, 0))                # Czy jakakolwiek wplata 12M



####### Laczenie danych ###########################

raw.data <- inner_join(cases, events)
raw.data$if_concluded_fctr <- as.factor(ifelse(raw.data$if_concluded==0, 'bad', 'good'))
raw.data$if_paid_fctr <- as.factor(ifelse(raw.data$if_paid==0, 'bad', 'good'))

dim(raw.data)
str(raw.data)


######## Podzial na zbior uczacy i testowy ########

ind.ucz<-createDataPartition(raw.data$if_concluded, times= 1, p=0.7, list=FALSE)

walidacyjne <- raw.data[-ind.ucz, ]
uczace <- raw.data[ind.ucz, ]

ind.ucz <- createDataPartition(uczace$if_concluded, times= 1, p=0.7, list=FALSE)

uczace <- uczace[ind.ucz,]
testowe <- uczace[-ind.ucz,]

dim(walidacyjne)
dim(uczace)
dim(testowe)

dim(testowe)[1]

summary(uczace$if_concluded)
summary(testowe$if_concluded)

summary(uczace$if_paid)
summary(testowe$if_paid)


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

uczace <- uczace%>%
            select(., -if_concluded, -if_concluded_fctr, -if_paid, if_paid_fctr, - CaseId)


(tree3c <- ctree(if_paid_fctr~., uczace, control = ctree_control(minsplit = 1000)))
(tree4r <- rpart(if_paid_fctr~., uczace))
(tree5r <- rpart(if_paid_fctr~., uczace, control = rpart.control(cp =0.001, minsplit = 1000)))
(tree6r <- rpart(if_paid_fctr~., uczace, control = rpart.control(cp = 0.001, minsplit = 100)))


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

pred.6r <- predict(tree6r, newdata = testowe, type = "class")
confusionMatrix(data = pred.6r, reference = testowe$if_paid_fctr)



