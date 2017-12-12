
library(tidyverse)  
library(rpart)      
library(rpart.plot)
library(partykit)
library(data.table)
library(psych)
library(caret)


load("KrukPWr2018.Rdata")

####### Drzewa Klasyfikacyjne #########

############### ćwiczenie #########################

### sprawdz rozmiary ramek 'walidacyjne','uczace,'testowe'

### sprawdz rozklady 'if_concluded' we wszystkich trzech zbiorach

### zobacz rownierz frakcje 'if_paid'


######## modelowanie ##############################

### uzywamy 1 cechy TOA
set.seed(1234)

tree1c<- ctree(if_concluded_fctr~ TOA ,uczace)

tree1r <- rpart(if_concluded_fctr ~ TOA, uczace)

tree1r

tree2r <- rpart(if_concluded_fctr ~ TOA, uczace, control = rpart.control(cp =0.0001), method = "class")

tree2r


### predyckcja i macierz pomylek

pred.1c <- predict(tree1c, newdata = testowe, type = "response")

confusionMatrix(data = pred.1c, reference = testowe$if_paid_fctr, positive = "good")

# zbuduj macierz pomylek dla 'tree1r i 'tree2r'



pred.2r <- predict(tree2r , newdata = testowe, type = "class")

confusionMatrix(data = pred.2r, reference = testowe$if_paid_fctr)


#### wszytskie dostepne cechy

uczace_t <- uczace%>%
  select(., -if_concluded, -if_concluded_fctr, -if_paid, if_paid_fctr, - CaseId)


############### ćwiczenie #########################
### wyswietl tresc pomocy dla funkcji rpart i rpart.control

?rpart
?rpart.control

(tree3c <- ctree(if_paid_fctr ~ ., uczace_t, control = ctree_control(minsplit = 1000)))
(tree4r <- rpart(if_paid_fctr~., uczace_t))

# parametryzacja 
(tree5r <- rpart(if_paid_fctr~., uczace_t, control = rpart.control(cp =0.001, minsplit = 1000)))


############### ćwiczenie #########################
# zbuduj drzewo z wybranymi przez Ciebie parametrami



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
### dodaj macierz klasyfikacji dla zbudowanego przez Ciebie drzewa


######## Regresja Liniowa ################
##kopia danych do przeksztalcen
dane<-cases
summary(cases)


##################uzupelnianie brakow ###########

##pomocna funkcja
df_status<-function (data){ 
  df = data.frame(
    type = sapply(data, class),
    q_na = sapply(data, function(x) sum(is.na(x))), ##ilo?? brak?w
    p_na = round(100 * sapply(data, function(x) sum(is.na(x)))/nrow(data),2), ##procent brak?w
    q_zeros = sapply(data, function(x) sum(x == 0, na.rm = T)),  ##ilo?? 0
    p_zeros = round(100 * sapply(data, function(x) sum(x == 0, na.rm = T))/nrow(data),2), ##procent 0
    q_1 = sapply(data, function(x) sum(x == -1, na.rm = T)),  ##ilo?? -1
    p_1 = round(100 * sapply(data, function(x) sum(x == -1, na.rm = T))/nrow(data),2), ##procent -1
    unique = sapply(data,function(x) sum(!is.na(unique(x))))
  )
  df$variable = rownames(df)
  rownames(df) = NULL
  df= df[,c(9, 1:8)]
}

(status.sprawy<-df_status(dane))
names(dane) #numer zmiennej w ramce

#1. LoanAmount NA  mediana
(med<-median(dane$LoanAmount,na.rm=TRUE))
dane$LoanAmount<-ifelse(is.na(dane$LoanAmount),med,dane$LoanAmount)
summary(dane$LoanAmount)
#summary(dane)


#6. Other NA wyliczamy
head(dane[which(is.na(dane$Other))])
dane$Other<-ifelse(is.na(dane$Other),dane$TOA - dane$Principal - dane$Interest ,dane$Other)


#8. D_ContractDateToImportDate NA mediana
########################?wiczenie##################################
(med <- median(dane$D_ContractDateToImportDate, na.rm = TRUE))
dane$D_ContractDateToImportDate <- ifelse(is.na(dane$D_ContractDateToImportDate), med, dane$D_ContractDateToImportDate)

#zamie? braki danych w zmiennej D_ContractDateToImportDate korzystaj?c z mediany
####################################################################

#9. DPD 0 mediana
(med<-median(dane$DPD,na.rm=TRUE))
dane$DPD<-ifelse(is.na(dane$DPD),med,dane$DPD)


#10. ExternalAgency -1 rozklad dwumianowy
cases <- as.data.table(cases)
dane <- as.data.table(dane)

cases[, .N, by = ExternalAgency]
dane[, .N, by=ExternalAgency]
dane$ExternalAgency[which(is.na(dane$ExternalAgency))] <- rbinom(22070,size = 1,prob = mean(dane$ExternalAgency,na.rm = T))


#11. Bailiff NA rozklad dwumianowy
dane[, .N, by=Bailiff]
dane$Bailiff[which(is.na(dane$Bailiff))] <- rbinom(6904,size = 1,prob = mean(dane$Bailiff,na.rm = T))

#12. ClosedExecution NA - czy mo?e by? tam gdzie nie by?o komornika
#orginalne dane
cases[Bailiff == 1, .N, by=ClosedExecution]
cases[Bailiff == 0, .N, by=ClosedExecution]

#tam gdzie Baliff 0 zamieniamy na 0, a tam gdzie 1 losujemy z dwumianowego
dane[, .N, by=ClosedExecution] ##6904 NA
dane$ClosedExecution[which(dane$Bailiff==0)]<-0  
dane[, .N, by=ClosedExecution] ##zostaje X
dim(dane[which(is.na(dane$ClosedExecution))])[1]

dane$ClosedExecution[which(is.na(dane$ClosedExecution))] <- rbinom(dim(dane[which(is.na(dane$ClosedExecution))])[1], size = 1,prob = mean(cases$ClosedExecution,na.rm = T))


#13. Land NA

cases[, .(.N, MaxMS=max(MeanSalary), minMS=min(MeanSalary),MaxGDP=max(GDPPerCapita),MinGDP=min(GDPPerCapita)),
      by=Land][order(Land)]

dane$Land[which(is.na(dane$Land))]<-10

#19. GDPPerCapita NA z Landu
dane$GDPPerCapita[which(is.na(dane$GDPPerCapita))]<-16660
#20. MeanSalary NA z Landu
dane$MeanSalary[which(is.na(dane$MeanSalary))]<-2470
#15. Age -1 mediana
(med<-median(dane$Age[which(dane$Age!=-1)]))
dane$Age<-ifelse(dane$Age==-1,med,dane$Age)

#16. Gender NA moda 
cases[, .N, by=Gender]
dane$Gender[which(is.na(dane$Gender))]<-"MALE"

#17. LastPaymentAmount NA mediana, s? te? 0
(med<-median(dane$LastPaymentAmount,na.rm=TRUE))
dane$LastPaymentAmount<-ifelse(is.na(dane$LastPaymentAmount),med,dane$LastPaymentAmount)

#18. M_LastPaymentToImportDate NA
##warto?? skrajna 50


dane$M_LastPaymentToImportDate[which(is.na(dane$M_LastPaymentToImportDate))]<-50

#14. PopulationInCity NA mediana
(med<-median(dane$PopulationInCity,na.rm=TRUE))
dane$PopulationInCity<-ifelse(is.na(dane$PopulationInCity),med,dane$PopulationInCity)

##sprawdzenie
(status.sprawy<-df_status(dane))
##################



str(dane)

################ factory/chary na dummy #####################

table(dane$Gender)
dane$IsMale<-ifelse(dane$Gender=="MALE",1,0)

table(dane$Product)
dane$ProductCashLoan<-ifelse(dane$Product=="Cash loan",1,0)
#dane.ucz$ProductCreditCard<-ifelse(dane.ucz$Product=="Credit card",1,0)

##Land n-1 zmiennych dummy


library(psych)
(land_D <- dummy.code(dane$Land))

#ta sama kolejnosc wierszy
head(land_D)
head(dane)

land_D<-data.frame(land_D)

dane_all<-cbind(dane,land_D)

names(dane_all)

names(dane)

############## KORELACJE ##################
load("splitted.Rdata")
library(corrplot)
str(dane)
kor <- cor(dane[,-c(1,7,13,16)])
kor_all <- cor(dane_all[,-c(1,7,13,16)])

corrplot(kor, type="lower")
corrplot(kor, type="lower",method = 'number')

corrplot(kor_all, type="lower")
corrplot(kor_all, type="lower",method = 'number')

names(dane_all)

#wybierz zmienne skorelowane, kt?r chcesz usun?c z uczenia w dalszym kroku 


######zczytanie idik?W zbioru ucz?cego/testowego##########
#load("splitted.Rdata")

names(uczace)
#wybieramy tylko kolumny CaseId i sukces if_concluded
wsad.uczace<-uczace[,c(1,21)]
wsad.testowe<-testowe[,c(1,21)]


uczace.lm<-inner_join(wsad.uczace,dane_all[,-c(7,13,16,19,3,4)]) 
uczace.lm<-uczace.lm[,-1]

summary(uczace.lm)
dim(uczace.lm)

testowe.lm<-inner_join(wsad.testowe,dane_all[,-c(7,13,16,19,3,4)])
testowe.lm<-testowe.lm[,-1]

summary(testowe.lm)
dim(testowe.lm)



################ MODEL #####################
?lm


(formula1<-as.formula('if_concluded~.')) ##wszystkie zmienne
uczace.lm = data.frame(uczace.lm)

model1 <- lm(formula1, data = uczace.lm)
summary(model1)

model1 <- step(model1,direction = "both")
summary(model1)


#zapisanie wyestymowanego modelu
setwd("~/...tw?j folder..")
save(model1,file = "model_lm.R")


coefficients(model1) # model coefficients
pred<-fitted(model1) # predicted values
summary(pred)


summary(residuals(model1))# residuals



layout(matrix(c(1,2,3,4),2,2)) # 
plot(model1)


##klasyfikacja
dim(uczace.lm)
length(pred)

uczace.lm$klasa_lm<-ifelse(pred>=0.5,1,0)

table(uczace.lm$klasa_lm)

########################?wiczenie##################################
##napisz kod, kt?ry tworzy zmienn? klasa_lm2 por?wnuj?c nie do 0.5 ale do proporcji sukcesu w pr?bce ucz?cej
####################################################################

########################?wiczenie##################################
##na podstawie por?wnania otrzymanej predykcji z modelowanym sukcesem 
#stw?rz macierz klasyfikacji na zbiorze uczacym (klasa z modelu vs prawdziwy sukces) i oblicz odpowiednio:

#macierz klasyfikacji (table)


#true negative TN
(tn_l<-
   #true positive TP
(tp_l<-
      #false negative FN
(fn_l<-
         #false positive FP
(fp_l<-
            
            
#oraz czu?o?? i specyficzno??
#SE (sensitivity, czu?o??) ? okre?la zdolno?? klasyfikatora do wykrywania klasy pozytywnej 
#SE = TP / (TP + FN)
#SP (specificity, specyficzno??) ? okre?la zdolno?? klasyfikatora do wykrywania klasy negatywnej 
#SP = TN / (TN + FP)
#ACC (Total Accuracy) ? ca?kowita sprawno?? klasyfikatora, okre?la prawdopodobie?stwo poprawnej klasyfikacji, czyli stosunek poprawnych klasyfikacji do wszystkich klasyfikacji
#ACC = (TP + TN) / (TP + TN + FP + FN)
            
(SE_l <-
(SP_l <- 
(ACC_l<-
                     
                     
pred.test<-predict.lm(model1,newdata = testowe.lm)
                   
########################?wiczenie##################################
#te same miary wyznacz na zbiorze testowym
                   
#czy model jest przeuczony?




