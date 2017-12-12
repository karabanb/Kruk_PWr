ls()
##rm(list=ls())


##kopia danych do przeksztalcen
load("~/Moje/menago/kursR/pwr_workshop/KrukPWr2018.Rdata")
dane<-cases
summary(cases)


##################uzupelnianie brakow ###########
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
summary(cases)
summary(dane)

#8. D_ContractDateToImportDate NA mediana
(med<-median(dane$D_ContractDateToImportDate,na.rm=TRUE))
dane$D_ContractDateToImportDate<-ifelse(is.na(dane$D_ContractDateToImportDate),med,dane$D_ContractDateToImportDate)

#9. DPD 0 mediana
(med<-median(dane$DPD,na.rm=TRUE))
dane$DPD<-ifelse(is.na(dane$DPD),med,dane$DPD)

library(data.table)
#10. ExternalAgency -1 rozklad dwumianowy
#komentarz ¿e 0 s¹ wiêc tu nie wiemy
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
cases[is.na(Bailiff), .N, by=ClosedExecution]

dane[Bailiff == 1, .N, by=ClosedExecution]
dane[Bailiff == 0, .N, by=ClosedExecution]
dane[is.na(Bailiff), .N, by=ClosedExecution]


#tam gdzie Baliff 0 zamieniamy na 0, a tam gdzie 1 losujemy z dwumianowego
dane[, .N, by=ClosedExecution] ##6904 NA
dane$ClosedExecution[which(dane$Bailiff==0)]<-0 ##zostaje 2770

dane$ClosedExecution[which(is.na(dane$ClosedExecution))] <- rbinom(2770, size = 1,prob = mean(cases$ClosedExecution,na.rm = T))


#13. Land NA
#bierzemy mode, ale mo¿na spróbowaæ losowaæ wg czêstoœci
dane[, .(.N, MaxMS=max(MeanSalary), minMS=min(MeanSalary),MaxGDP=max(GDPPerCapita),MinGDP=min(GDPPerCapita)),
      by=Land][order(Land)]
#recznie wybieramy land 10
dane$Land[which(is.na(dane$Land))]<-10

#19. GDPPerCapita NA z Landu
dane$GDPPerCapita[which(is.na(dane$GDPPerCapita))]<-16660
#20. MeanSalary NA z Landu
dane$MeanSalary[which(is.na(dane$MeanSalary))]<-2470
#15. Age -1 mediana
(med<-median(dane$Age[which(dane$Age!=-1)]))
dane$Age<-ifelse(dane$Age==-1,med,dane$Age)

#16. Gender NA moda ale mo¿na by losowaæ z rok³adu
dane[, .N, by=Gender]
dane$Gender[which(is.na(dane$Gender))]<-"MALE"
#17. LastPaymentAmount NA mediana, s¹ te¿ 0
(med<-median(dane$LastPaymentAmount,na.rm=TRUE))
dane$LastPaymentAmount<-ifelse(is.na(dane$LastPaymentAmount),med,dane$LastPaymentAmount)

#18. M_LastPaymentToImportDate NA
##tam gdzie NA mo¿e oznaczaæ brak wp³aty
##wartoœæ skrajna 50

dane$M_LastPaymentToImportDate[which(is.na(dane$M_LastPaymentToImportDate))]<-50

#14. PopulationInCity NA mediana
(med<-median(dane$PopulationInCity,na.rm=TRUE))
dane$PopulationInCity<-ifelse(is.na(dane$PopulationInCity),med,dane$PopulationInCity)

##sprawdzenie
(status.sprawy<-df_status(dane))
##################

################ factory
str(dane)

################ factory/chary na dummy #####################

table(dane$Gender)
dane$IsMale<-ifelse(dane$Gender=="MALE",1,0)
table(dane$IsMale)

table(dane$Product)
dane$ProductCashLoan<-ifelse(dane$Product=="Cash loan",1,0)
#dane.ucz$ProductCreditCard<-ifelse(dane.ucz$Product=="Credit card",1,0)


##Land n-1 zmiennych dummy

library(psych)
(land_D <- dummy.code(dane$Land))

#ta sama kolejnosc wierszy
head(land_D)
head(dane)
##nie dzia³a
##names(land_D)<-c("Land1",	"Land2",	"Land3",	"Land4",	"Land5",	"Land6",	"Land7",	"Land8",	"Land9",	"Land10",	"Land11",	"Land12",	"Land13",	"Land14",	"Land15",	"Land16",	"Land17",	"Land18",	"Land19",	"Land20",	"Land21",	"Land22",	"Land23",	"Land24",	"Land25",	"Land26",	"Land27",	"Land28",	"Land29",	"Land30",	"Land31",	"Land32",	"Land33",	"Land34",	"Land35",	"Land36",	"Land37")
land_D<-data.frame(land_D)

dane_all<-cbind(dane,land_D)

names(dane_all)
## te zmienione
##dane[c(7,13,16)] 

summary(dane)
dim(dane)

############## KORELACJE ##################
library(corrplot)
str(dane)
kor <- cor(dane[,-c(1,7,13,16)])
kor_all <- cor(dane_all[,-c(1,7,13,16)])


# kor <- cor(dane.ucz[,-c(1,3,11)])
#rownames(kor) <- substr(rownames(kor),1,4)
#colnames(kor) <- substr(colnames(kor),1,4)
library(corrplot)
corrplot(kor, type="lower")
corrplot(kor, type="lower",method = 'number')

corrplot(kor_all, type="lower")
corrplot(kor_all, type="lower",method = 'number')

names(dane_all)

#dodatkowo wykluczamy 19 GDP, 3 TOA, 4 Principal
dane.ucz<-dane_all[,-c(1,7,13,16,19,3,4)]
 
summary(dane.ucz)

######zczytanie idikóW zbioru ucz¹cego##########
load("~/Moje/menago/kursR/pwr_workshop/splitted.Rdata")
ls()
head(uczace)
names(uczace)
wsad.uczace<-uczace[,c(1,21)]
wsad.testowe<-testowe[,c(1,21)]

library(dplyr)

uczace.lm<-inner_join(wsad.uczace,dane_all[,-c(7,13,16,19,3,4)]) #case id potrzebne do laczenia
uczace.lm<-uczace.lm[,-1] #w kolejnym kroku go usuwamy
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

#poprawic formule
##(formula2<-as.formula(paste('if_concluded~',paste(names(dane)[,2],collapse = '+'))))
model1 <- lm(formula1, data = uczace.lm)
summary(model1)
# drop1(model1)
model1 <- step(model1,direction = "both")
summary(model1)

setwd("~/Moje/menago/kursR/pwr_workshop")
save(model1,file = "model_lm_new.R")
load("~/Moje/menago/kursR/pwr_workshop/model_lm.R")


##wybraæ co pokazywaæ
coefficients(model1) # model coefficients
#confint(model1, level=0.95) # CIs for model parameters 
pred<-fitted(model1) # predicted values
summary(pred)


summary(residuals(model1))# residuals


layout(matrix(c(1,2,3,4),2,2)) # 
plot(model1)


##klasyfikacja
dim(uczace.lm)
length(pred)
head(pred)
str(uczace.lm)


uczace.lm$klasa_lm<-ifelse(pred>=0.5,1,0)
hist(uczace.lm$if_concluded)
summary(pred)
table(uczace.lm$klasa_lm)

########################æwiczenie##################################
##napisz kod, który tworzy zmienn¹ klasa_lm2 porównuj¹c nie do 0.5 ale do proporcji sukcesu w próbce ucz¹cej
####################################################################
mean(uczace.lm$if_concluded)
uczace.lm$klasa_lm2<-ifelse(pred>=mean(uczace.lm$if_concluded),1,0)
table(uczace.lm$klasa_lm2)


########################æwiczenie##################################
##na podstawie porównania otrzymanej predykcji z modelowanym sukcesem 
#stwórz macierz klasyfikacji na zbiorze uczacym i oblicz odpowiednio:

head(uczace.lm)
(m_klas_learn<-table(uczace.lm$klasa_lm2,uczace.lm$if_concluded))
(tn_l<-m_klas_learn[1,1])
(tp_l<-m_klas_learn[2,2])
(fn_l<-m_klas_learn[1,2])
(fp_l<-m_klas_learn[2,1])

(SE_l <-tp_l / (tp_l + fn_l))
(SP_l = tn_l / (tn_l + fp_l)) 
(ACC_l = (tp_l + tn_l) / (tp_l + tn_l + fp_l + fn_l)) 

########################æwiczenie##################################
#te same miary wyznacz na zbiorze testowym

pred.test<-predict.lm(model1,newdata = testowe.lm)
testowe.lm$klasa_lm2<-ifelse(pred.test>=mean(uczace.lm$if_concluded),1,0)
table(testowe.lm$klasa_lm2)
(m_klas_test<-table(testowe.lm$klasa_lm2,testowe.lm$if_concluded))
(tn_t<-m_klas_test[1,1])
(tp_t<-m_klas_test[2,2])
(fn_t<-m_klas_test[1,2])
(fp_t<-m_klas_test[2,1])

(SE_t <-tp_t / (tp_t + fn_t))
(SP_t = tn_t / (tn_t + fp_t)) 
(ACC_t = (tp_t + tn_t) / (tp_t + tn_t + fp_t + fn_t)) 


#to nie
library(ggplot2)
##graficzne sprawdzenie przyk³adowych liniowych i nieliniowych
plot(uczace.lm$Age,uczace.lm$if_concluded)
ggplot(aes(x=Age),data=uczace.lm)+geom_density()+facet_grid(~if_concluded)
ggplot(aes(x=Age,color=as.factor(if_concluded)),data=uczace.lm)+geom_density()

hist(uczace.lm$Age)
