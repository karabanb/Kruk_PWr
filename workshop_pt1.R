
library(tidyverse)
library(Information)
library(smbinning)
library(mlr)
library(caret)
library(xray)
library(rpart)
library(tree)
library(corrplot)

source("WOE.r")
source("Information Value.R")
source("logistic regression.R")
source("Funkcje.r")


####### loading data frames ##########

load("datasets_PWr.Rdata")
load("Biblioteka_ZAS.RData")


###### ekslporacyjna analiza danych ###

anom <- anomalies(cases)

anom$problem_variables

#### 

raw.data <- events %>%
              group_by(., CaseId)%>%
              summarise(., 
                           if_concluded = sum(!is.na(NumberOfAgreementConcluded)),
                           if_signed = sum(!is.na(NumberOfAgreementSigned)),
                           if_to_legal = sum(!is.na(TransferToLegalProcess)),
                           sum_of_payments = sum(!is.na(PaymentAmount))
                        )%>%
              mutate(.,
                          if_concluded = if_else(if_concluded>0, 1,0),
                          if_signed = if_else(if_signed>0, 1, 0),
                          if_to_legal = if_else(if_to_legal>0, 1, 0),
                          if_paid = if_else(sum_of_payments>0, 1, 0)
                     )%>%
              left_join(.,cases, by = c("CaseId" = "CaseId"))%>%
              mutate_if(., is.character, as.factor)%>%
              replace_na(., replace = list(LoanAmount = -1,
                                           Other = 0,
                                           D_ContractDateToImportDate = -1,
                                           ExternalAgency = -1,
                                           Bailiff = -1,
                                           ClosedExecution = -1,
                                           Land = 100,
                                           PopulationInCity = -1,
                                         #  Gender = 3,
                                           LastPaymentAmount = -1,
                                           M_LastPaymentToImportDate = -1,
                                           GDPPerCapita = -1,
                                           MeanSalary = -1)
                        )%>%
            mutate(Land = as.factor(Land))
              

###### training in rpart #######


data <- raw.data%>%
  select(., -if_signed, -sum_of_payments, -if_to_legal, -CaseId, -if_paid)%>%
  mutate(., if_concluded = as.factor(if_else(if_concluded == 0, 'bad', 'good')))

train.ix <- createDataPartition(y = data$if_concluded, p = 0.7, list = FALSE)

set.train <- data[train.ix, ]
set.test <- data[-train.ix, ]


ctrls <- rpart.control(cp = 0.0001, minsplit = 30, maxdepth = 30)

m.rpart <- rpart(if_concluded~., data = set.train, control = ctrls)
pred.rpart <- predict(m.rpart, newdata = set.test, type = "class")

confusionMatrix(data = pred.rpart, reference = set.test$if_concluded)

#### regresja logistyczna ######

wagi <- sum(data$if_concluded == "bad")/sum(data$if_concluded == "good")
data$waga <- ifelse(data$if_concluded == "bad",1, wagi)

sukces<-"if_concluded"

data.class<-as.vector(lapply(1:ncol(data),function(i) class(data[,i])))
unique(data.class)

dane.do.woe<-data.frame(Sukces=data$if_concluded, Waga=data$waga)



data[ ,sukces] <- as.numeric(ifelse(data[,sukces]=="good", 1, 0))

################ Loan amount #########################

zmienna <-"LoanAmount"   # czy kategoryzacja WB ma sens?
summary(data[,zmienna])

data <- as.data.frame(data)

data[,zmienna] <- ifelse(is.na(data[,zmienna])==TRUE,0, data[,zmienna])

InformationValue(data,sukces,"1",zmienna)
bland_danych <- -100
nowa_ramka <- do_drzewa(ramka = data, zmienna = zmienna, bland_danych, modelowana = sukces, "waga") ##tworzy wagi dla obserwacji

(my.tree <- drzewo(sukces,zmienna,nowa_ramka,"waga",0.015,1) )
nowa_ramka$band <- przedzialy(my.tree,nowa_ramka,zmienna)

WOE(nowa_ramka, sukces, "band",1)
InformationValue(nowa_ramka,sukces,"1","band")

wektor <-c(min(data[,zmienna])
           ,c(4150,7700,9200, 12200, 13500, 19000, 35000)
           ,max(data[,zmienna] )
)

zmienna.b<- podzial.reczny(data,zmienna,wektor)
cechy_band<-data.frame(sukces=data[,sukces], zmienna_band=zmienna.b)
names(cechy_band)
WOE(cechy_band,"sukces","zmienna_band",1)

InformationValue(Ramka.danych = cechy_band,zmienna.modelowana = "sukces" ,klasa.modelowana = "1",zmienna.objasniajaca = "zmienna_band")


dane.do.woe<-data.frame(dane.do.woe,LoanAmount = cechy_band$zmienna_band)


################ TOA #########################

zmienna <-"TOA"   # czy kategoryzacja WB ma sens?
summary(data[,zmienna])

data[,zmienna] <- ifelse(is.na(data[,zmienna])==TRUE,0, data[,zmienna])

InformationValue(data,sukces,"1",zmienna)
bland_danych <- -100
nowa_ramka <- do_drzewa(ramka = data,zmienna = zmienna,bland_danych,modelowana = sukces,"waga") ##tworzy wagi dla obserwacji

(my.tree <- drzewo(sukces,zmienna,nowa_ramka,"waga",0.01,1) )
nowa_ramka$band <- przedzialy(my.tree,nowa_ramka,zmienna)

WOE(nowa_ramka, sukces, "band",1)
InformationValue(nowa_ramka,sukces,"1","band")

wektor <-c(min(data[,zmienna])
           ,c(690,2500,6600,13900)
           ,max(data[,zmienna] )
)

zmienna.b<- podzial.reczny(data,zmienna,wektor)
cechy_band<-data.frame(sukces=data[,sukces], zmienna_band=zmienna.b)
names(cechy_band)
WOE(cechy_band,"sukces","zmienna_band",1)

InformationValue(Ramka.danych = cechy_band,zmienna.modelowana = "sukces" ,klasa.modelowana = "1",zmienna.objasniajaca = "zmienna_band")

dane.do.woe<-data.frame(dane.do.woe,TOA = cechy_band$zmienna_band)



################ Principal #########################

zmienna <-"Principal"   # czy kategoryzacja WB ma sens?
summary(data[,zmienna])

data[,zmienna] <- ifelse(is.na(data[,zmienna])==TRUE,0, data[,zmienna])

InformationValue(data,sukces,"1",zmienna)
bland_danych <- -1
nowa_ramka <- do_drzewa(ramka = data,zmienna = zmienna,bland_danych,modelowana = sukces,"waga") ##tworzy wagi dla obserwacji

(my.tree <- drzewo(sukces,zmienna,nowa_ramka,"waga",0.01,1) )
nowa_ramka$band <- przedzialy(my.tree,nowa_ramka,zmienna)

WOE(nowa_ramka, sukces, "band",1)
InformationValue(nowa_ramka,sukces,"1","band")

wektor <-c(min(data[,zmienna])
           ,c(550,11200, 13000, 17800, 26000)
           ,max(data[,zmienna] )
)

zmienna.b<- podzial.reczny(data,zmienna,wektor)
cechy_band<-data.frame(sukces=data[,sukces], zmienna_band=zmienna.b)
names(cechy_band)
WOE(cechy_band,"sukces","zmienna_band",1)

InformationValue(Ramka.danych = cechy_band,zmienna.modelowana = "sukces" ,klasa.modelowana = "1",zmienna.objasniajaca = "zmienna_band")

dane.do.woe<-data.frame(dane.do.woe,Principal = cechy_band$zmienna_band)

######################## Interest #########################

zmienna <-"Interest"   # czy kategoryzacja WB ma sens?
summary(data[,zmienna])

data[,zmienna] <- ifelse(is.na(data[,zmienna])==TRUE,0, data[,zmienna])

InformationValue(data,sukces,"1",zmienna)
bland_danych <- -1
nowa_ramka <- do_drzewa(ramka = data,zmienna = zmienna,bland_danych,modelowana = sukces,"waga") ##tworzy wagi dla obserwacji

(my.tree <- drzewo(sukces,zmienna,nowa_ramka,"waga",0.01,1) )
nowa_ramka$band <- przedzialy(my.tree,nowa_ramka,zmienna)

WOE(nowa_ramka, sukces, "band",1)
InformationValue(nowa_ramka,sukces,"1","band")

wektor <-c(min(data[,zmienna])
           ,c(350, 810 , 1950 , 2250, 2750, 5700)
           ,max(data[,zmienna] )
)

zmienna.b<- podzial.reczny(data,zmienna,wektor)
cechy_band<-data.frame(sukces=data[,sukces], zmienna_band=zmienna.b)
names(cechy_band)
WOE(cechy_band,"sukces","zmienna_band",1)

InformationValue(Ramka.danych = cechy_band,zmienna.modelowana = "sukces" ,klasa.modelowana = "1",zmienna.objasniajaca = "zmienna_band")

dane.do.woe<-data.frame(dane.do.woe,Interest = cechy_band$zmienna_band)


######################## Other  #########################

zmienna <-"Other"   # czy kategoryzacja WB ma sens?
summary(data[,zmienna])

data[,zmienna] <- ifelse(is.na(data[,zmienna])==TRUE,0, data[,zmienna])

InformationValue(data,sukces,"1",zmienna)
bland_danych <- -100000
nowa_ramka <- do_drzewa(ramka = data,zmienna = zmienna,bland_danych,modelowana = sukces,"waga") ##tworzy wagi dla obserwacji

(my.tree <- drzewo(sukces,zmienna,nowa_ramka,"waga",0.01,1) )
nowa_ramka$band <- przedzialy(my.tree,nowa_ramka,zmienna)

WOE(nowa_ramka, sukces, "band",1)
InformationValue(nowa_ramka,sukces,"1","band")

wektor <-c(min(data[,zmienna])
           ,c(1100, 1900, 4100, 8100)
           ,max(data[,zmienna] )
)

zmienna.b<- podzial.reczny(data,zmienna,wektor)
cechy_band<-data.frame(sukces=data[,sukces], zmienna_band=zmienna.b)
names(cechy_band)
WOE(cechy_band,"sukces","zmienna_band",1)

InformationValue(Ramka.danych = cechy_band,zmienna.modelowana = "sukces" ,klasa.modelowana = "1",zmienna.objasniajaca = "zmienna_band")

dane.do.woe<-data.frame(dane.do.woe,Other  = cechy_band$zmienna_band)



########################  D_ContractDateToImportDate  #########################

zmienna <-"D_ContractDateToImportDate"   # czy kategoryzacja WB ma sens?
summary(data[,zmienna])

data[,zmienna] <- ifelse(is.na(data[,zmienna])==TRUE,0, data[,zmienna])

InformationValue(data,sukces,"1",zmienna)
bland_danych <- -1000
nowa_ramka <- do_drzewa(ramka = data,zmienna = zmienna,bland_danych,modelowana = sukces,"waga") ##tworzy wagi dla obserwacji

(my.tree <- drzewo(sukces,zmienna,nowa_ramka,"waga",0.02,1) )
nowa_ramka$band <- przedzialy(my.tree,nowa_ramka,zmienna)

WOE(nowa_ramka, sukces, "band",1)
InformationValue(nowa_ramka,sukces,"1","band")

wektor <-c(min(data[,zmienna])
           ,c(2400)
           ,max(data[,zmienna] )
)

zmienna.b<- podzial.reczny(data,zmienna,wektor)
cechy_band<-data.frame(sukces=data[,sukces], zmienna_band=zmienna.b)
names(cechy_band)
WOE(cechy_band,"sukces","zmienna_band",1)

InformationValue(Ramka.danych = cechy_band,zmienna.modelowana = "sukces" ,klasa.modelowana = "1",zmienna.objasniajaca = "zmienna_band")

dane.do.woe<-data.frame(dane.do.woe,D_ContractDateToImportDate  = cechy_band$zmienna_band)


########################  DPD  #########################

zmienna <-"DPD"   # czy kategoryzacja WB ma sens?
summary(data[,zmienna])

data[,zmienna] <- ifelse(is.na(data[,zmienna])==TRUE,0, data[,zmienna])

InformationValue(data,sukces,"1",zmienna)
bland_danych <- -1
nowa_ramka <- do_drzewa(ramka = data,zmienna = zmienna,bland_danych,modelowana = sukces,"waga") ##tworzy wagi dla obserwacji

(my.tree <- drzewo(sukces,zmienna,nowa_ramka,"waga",0.1,1) )
nowa_ramka$band <- przedzialy(my.tree,nowa_ramka,zmienna)

WOE(nowa_ramka, sukces, "band",1)
InformationValue(nowa_ramka,sukces,"1","band")

wektor <-c(min(data[,zmienna])
           ,c(480, 1370, 1540, 1820)
           ,max(data[,zmienna] )
)

zmienna.b<- podzial.reczny(data,zmienna,wektor)
cechy_band<-data.frame(sukces=data[,sukces], zmienna_band=zmienna.b)
names(cechy_band)
WOE(cechy_band,"sukces","zmienna_band",1)

InformationValue(Ramka.danych = cechy_band,zmienna.modelowana = "sukces" ,klasa.modelowana = "1",zmienna.objasniajaca = "zmienna_band")

dane.do.woe<-data.frame(dane.do.woe,DPD=cechy_band$zmienna_band)

########################  Bailiff  #########################

zmienna <-"Bailiff"   # czy kategoryzacja WB ma sens?
summary(data[,zmienna])

data[,zmienna] <- ifelse(is.na(data[,zmienna])==TRUE,0, data[,zmienna])

InformationValue(data,sukces,"1",zmienna)
bland_danych <- 5
nowa_ramka <- do_drzewa(ramka = data,zmienna = zmienna,bland_danych,modelowana = sukces,"waga") ##tworzy wagi dla obserwacji

(my.tree <- drzewo(sukces,zmienna,nowa_ramka,"waga",0.001,1) )
nowa_ramka$band <- przedzialy(my.tree,nowa_ramka,zmienna)

WOE(nowa_ramka, sukces, "band",1)
InformationValue(nowa_ramka,sukces,"1","band")


cechy_band<-data.frame(sukces=data[,sukces], zmienna_band = nowa_ramka$band)
names(cechy_band)
WOE(cechy_band,"sukces","zmienna_band",1)

InformationValue(Ramka.danych = cechy_band,zmienna.modelowana = "sukces" ,klasa.modelowana = "1",zmienna.objasniajaca = "zmienna_band")

dane.do.woe<-data.frame(dane.do.woe,Bailiff  = cechy_band$zmienna_band)


########################  Age  #########################

zmienna <-"Age"   # czy kategoryzacja WB ma sens?
summary(data[,zmienna])

data[,zmienna] <- ifelse(is.na(data[,zmienna])==TRUE,0, data[,zmienna])

InformationValue(data,sukces,"1",zmienna)
bland_danych <- -3
nowa_ramka <- do_drzewa(ramka = data,zmienna = zmienna,bland_danych,modelowana = sukces,"waga") ##tworzy wagi dla obserwacji

(my.tree <- drzewo(sukces,zmienna,nowa_ramka,"waga",0.03,1) )
nowa_ramka$band <- przedzialy(my.tree,nowa_ramka,zmienna)

WOE(nowa_ramka, sukces, "band",1)
InformationValue(nowa_ramka,sukces,"1","band")

cechy_band<-data.frame(sukces=data[,sukces], zmienna_band = nowa_ramka$band)
names(cechy_band)
WOE(cechy_band,"sukces","zmienna_band",1)

InformationValue(Ramka.danych = cechy_band,zmienna.modelowana = "sukces" ,klasa.modelowana = "1",zmienna.objasniajaca = "zmienna_band")

dane.do.woe<-data.frame(dane.do.woe, Age  = cechy_band$zmienna_band)


########################  M_LastPaymentToImportDate #########################

zmienna <-"M_LastPaymentToImportDate"   # czy kategoryzacja WB ma sens?
summary(data[,zmienna])

data[,zmienna] <- ifelse(is.na(data[,zmienna])==TRUE,0, data[,zmienna])

InformationValue(data,sukces,"1",zmienna)
bland_danych <- -100
nowa_ramka <- do_drzewa(ramka = data,zmienna = zmienna,bland_danych,modelowana = sukces,"waga") ##tworzy wagi dla obserwacji

(my.tree <- drzewo(sukces,zmienna,nowa_ramka,"waga",0.001,1) )
nowa_ramka$band <- przedzialy(my.tree,nowa_ramka,zmienna)

WOE(nowa_ramka, sukces, "band",1)
InformationValue(nowa_ramka,sukces,"1","band")

cechy_band<-data.frame(sukces=data[,sukces], zmienna_band = nowa_ramka$band)
names(cechy_band)
WOE(cechy_band,"sukces","zmienna_band",1)

InformationValue(Ramka.danych = cechy_band,zmienna.modelowana = "sukces" ,klasa.modelowana = "1",zmienna.objasniajaca = "zmienna_band")

dane.do.woe<-data.frame(dane.do.woe, M_LastPaymentToImportDate  = cechy_band$zmienna_band)


################ przekoduj na WoE ######################################

dane.woe.x <- PrzekodujNaWOE(Ramka.danych = dane.do.woe[,-2], zmienna.modelowana = "Sukces", klasa.modelowana = "good")

dane.woe <- data.frame(dane.woe.x$Ramka.danych.WOE, Waga = dane.do.woe$Waga)
dane.woe.przejsciowka <- dane.woe.x$Przejsciowka

names(dane.woe) <- str_c("WoE_",colnames(dane.woe))

corr <- cor(select(
  dane.woe,
  -WoE_Waga,
 -WoE_Sukces,
  -WoE_Bailiff,
 - WoE_D_ContractDateToImportDate,
  - WoE_Principal,
  - WoE_LoanAmount
  )
  )


corrplot(abs(corr)>0.6, order = "alphabet", cl.ratio = 0.2,tl.cex = 0.6, type = "lower")


dane.woe <- select(dane.woe, -WoE_Waga, -WoE_Sukces, -WoE_Bailiff, - WoE_D_ContractDateToImportDate, - WoE_Principal, - WoE_LoanAmount)


prepared.data <- bind_cols(data,dane.woe)
     
prepared.data$if_concluded<- as.factor(prepared.data$if_concluded)             



###### model regresji logistycznej ###########################

m.rl <- glm(
            data = prepared.data, 
            formula = if_concluded ~ WoE_TOA + WoE_Interest + WoE_Other + WoE_DPD + WoE_Age + WoE_M_LastPaymentToImportDate,
            family = binomial(link = "logit")
            )


pred.rl <- predict(m.rl, newdata = prepared.data, type = "response")



gini.coeff(ramka = prepared.data, objasniana = "if_concluded",model = m.rl)

wykresy(prepared.data, "if_concluded", m.rl)





calssif.task <- makeClassifTask(
                                  data = data,
                                  target = "if_paid"
                                  )

lrn.rpart <- makeLearner("classif.rpart", par.vals = list(cp = 0.001, maxdepth = 20), minsplit = 1000,predict.type = "prob")

rdesc <- makeResampleDesc(method = "CV", stratify = TRUE)

r <- resample(lrn.rpart,calssif.task,rdesc, models = TRUE, measures = list(acc, auc))

m1 <- mlr::train(lrn.rpart, calssif.task)

pred.m1 <- predict(m1, newdata = data, type = "class", list = FALSE)$data

confusionMatrix(data = pred.m1$response, reference = pred.m1$truth)


