ls() ##sprawdzenie obiektów w workspace

##czyszczenie workspace
##rm(list=ls())

library(data.table)
library(psych)
library(corrplot)
library(dplyr)


load("~/...twój folder../KrukPWr2018.Rdata")
##kopia danych do przeksztalcen
dane<-cases
summary(cases)


##################uzupelnianie brakow ###########

##pomocna funkcja
df_status<-function (data){ 
  df = data.frame(
    type = sapply(data, class),
    q_na = sapply(data, function(x) sum(is.na(x))), ##iloœæ braków
    p_na = round(100 * sapply(data, function(x) sum(is.na(x)))/nrow(data),2), ##procent braków
    q_zeros = sapply(data, function(x) sum(x == 0, na.rm = T)),  ##iloœæ 0
    p_zeros = round(100 * sapply(data, function(x) sum(x == 0, na.rm = T))/nrow(data),2), ##procent 0
    q_1 = sapply(data, function(x) sum(x == -1, na.rm = T)),  ##iloœæ -1
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
########################æwiczenie##################################
#zamieñ braki danych w zmiennej D_ContractDateToImportDate korzystaj¹c z mediany
####################################################################

#9. DPD 0 mediana
(med<-median(dane$DPD,na.rm=TRUE))
dane$DPD<-ifelse(is.na(dane$DPD),med,dane$DPD)


#10. ExternalAgency -1 rozklad dwumianowy

cases[, .N, by=ExternalAgency]
dane[, .N, by=ExternalAgency]
dane$ExternalAgency[which(is.na(dane$ExternalAgency))] <- rbinom(22070,size = 1,prob = mean(dane$ExternalAgency,na.rm = T))


#11. Bailiff NA rozklad dwumianowy
dane[, .N, by=Bailiff]
dane$Bailiff[which(is.na(dane$Bailiff))] <- rbinom(6904,size = 1,prob = mean(dane$Bailiff,na.rm = T))

#12. ClosedExecution NA - czy mo¿e byæ tam gdzie nie by³o komornika
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
dane[, .N, by=Gender]
dane$Gender[which(is.na(dane$Gender))]<-"MALE"

#17. LastPaymentAmount NA mediana, s¹ te¿ 0
(med<-median(dane$LastPaymentAmount,na.rm=TRUE))
dane$LastPaymentAmount<-ifelse(is.na(dane$LastPaymentAmount),med,dane$LastPaymentAmount)

#18. M_LastPaymentToImportDate NA
##wartoœæ skrajna 50

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


(land_D <- dummy.code(dane$Land))

#ta sama kolejnosc wierszy
head(land_D)
head(dane)

land_D<-data.frame(land_D)

dane_all<-cbind(dane,land_D)

names(dane_all)

names(dane)

############## KORELACJE ##################

str(dane)
kor <- cor(dane[,-c(1,7,13,16)])
kor_all <- cor(dane_all[,-c(1,7,13,16)])

corrplot(kor, type="lower")
corrplot(kor, type="lower",method = 'number')

corrplot(kor_all, type="lower")
corrplot(kor_all, type="lower",method = 'number')

names(dane_all)

#wybierz zmienne skorelowane, któr chcesz usun¹c z uczenia w dalszym kroku 


######zczytanie idikóW zbioru ucz¹cego/testowego##########
load("~/Moje/menago/kursR/pwr_workshop/splitted.Rdata")

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
setwd("~/...twój folder..")
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

########################æwiczenie##################################
##napisz kod, który tworzy zmienn¹ klasa_lm2 porównuj¹c nie do 0.5 ale do proporcji sukcesu w próbce ucz¹cej
####################################################################

########################æwiczenie##################################
##na podstawie porównania otrzymanej predykcji z modelowanym sukcesem 
#stwórz macierz klasyfikacji na zbiorze uczacym (klasa z modelu vs prawdziwy sukces) i oblicz odpowiednio:

#macierz klasyfikacji (table)


#true negative TN
(tn_l<-
#true positive TP
(tp_l<-
#false negative FN
(fn_l<-
#false positive FP
(fp_l<


#oraz czu³oœæ i specyficznoœæ
#SE (sensitivity, czu³oœæ) – okreœla zdolnoœæ klasyfikatora do wykrywania klasy pozytywnej 
#SE = TP / (TP + FN)
#SP (specificity, specyficznoœæ) – okreœla zdolnoœæ klasyfikatora do wykrywania klasy negatywnej 
#SP = TN / (TN + FP)
#ACC (Total Accuracy) – ca³kowita sprawnoœæ klasyfikatora, okreœla prawdopodobieñstwo poprawnej klasyfikacji, czyli stosunek poprawnych klasyfikacji do wszystkich klasyfikacji
#ACC = (TP + TN) / (TP + TN + FP + FN)

(SE_l <-
(SP_l <- 
(ACC_l<-


pred.test<-predict.lm(model1,newdata = testowe.lm)

########################æwiczenie##################################
#te same miary wyznacz na zbiorze testowym

#czy model jest przeuczony?
