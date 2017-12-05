# *************************************************************
# ******************** MODEL LOGITOWY *************************
# *************************************************************
###############################################################

load("sprawy.Rdata")
load("zdarzenia.Rdata")


library(dplyr)

library(InformationValue)

library(caret)


Mode <- function(x) {
  ux <- levels(x)
  ux[which.max(tabulate(match(x, ux)))]
}



#ZAD 0. Przygotuj dane do budowy modelu, uwzględniajšc następujšce punkty:
summary(dane)

#a)	Wybierz jakš zmiennš sukcesu chcesz modelować, zakoduj jako 0 i 1

#czy wplata powyżej 300 zł w ciagu 6 miesiecy

target <-
  zdarzenia %>%
  filter(Month %in% 1:6) %>%
  select(CaseId,PaymentAmount) %>%
  group_by(CaseId) %>%
  summarise(y = sum(PaymentAmount,na.rm = T))


dim(target)
summary(target$y)
nrow(target)


#dane <- data.frame(inner_join(sprawy,target, by="CaseId")) %>% filter(target$y > 300 | target$y == 0)


dane <- data.frame(inner_join(sprawy,target, by="CaseId"))
dane$y[which(dane$y > 0)] <- 1
dane <- dane[which(dane$y >= 0),]

summary(dane)
table(dane$y)



#b)	Usuń braki danych w cechach, w których jest to możliwe do wykonania.


#uzupełniamy
dane$LoanAmount[is.na(dane$LoanAmount)] <- mean(dane$LoanAmount, na.rm = TRUE)
dane$Other[is.na(dane$Other)] <- mean(dane$Other, na.rm = TRUE)
dane$D_ContractDateToImportDate[is.na(dane$D_ContractDateToImportDate)] <- round(mean(dane$D_ContractDateToImportDate, na.rm = TRUE))
dane$PopulationInCiti[is.na(dane$PopulationInCiti)] <- mean(dane$PopulationInCiti, na.rm = TRUE)
dane$LastPaymentAmount[is.na(dane$LastPaymentAmount)] <- median(dane$LastPaymentAmount, na.rm = TRUE)
#median=0
dane$M_LastPaymentToImportDate[is.na(dane$M_LastPaymentToImportDate)] <- round(mean(dane$M_LastPaymentToImportDate, na.rm = TRUE))

dane$GDPPerCapita[is.na(dane$GDPPerCapita)] <- mean(dane$GDPPerCapita, na.rm = TRUE)
dane$MeanSalary[is.na(dane$MeanSalary)] <- mean(dane$MeanSalary, na.rm = TRUE)

#prop.table(table(dane$Bailiff))[2]
dane$Bailiff[is.na(dane$Bailiff)] <- ifelse(runif(sum(is.na(dane$Bailiff)),0,1)<prop.table(table(dane$Bailiff))[2],1,0)
dane$ClosedExecution[is.na(dane$ClosedExecution) & dane$Bailiff==0] <- 0
#prop.table(table(dane$ClosedExecution))[2]
dane$ClosedExecution[is.na(dane$ClosedExecution)] <- ifelse(runif(sum(is.na(dane$ClosedExecution)),0,1)<prop.table(table(dane$ClosedExecution))[2],1,0)


#zostawiamy jako potencjalnie istotny brak
levels(dane$Gender)<-c("FEMALE", "MALE", "brak")
dane$Gender[is.na(dane$Gender)] <- "brak"
summary(dane)


#c)	Usuń zmienne skorelowane
dane[c('ClosedExecution','GDPPerCapita',"Interest",'Other')] <- list(NULL)


##################################################
################ factory na dummy ################
##################################################
which(lapply(dane,class)=="factor")

table(dane$Gender)
dane$IsMale<-ifelse(dane$Gender=="MALE",1,ifelse(dane$Gender=="FEMALE",0,-1))
table(dane$IsMale)


table(dane$Product)
dane$ProductCashLoan<-ifelse(dane$Product=="Cash loan",1,ifelse(dane$Product=="Credit card",2,ifelse(dane$Product=="Housing loan",3,4)))
table(dane$ProductCashLoan)

#usuwamy przekodowane zmienne 
dane[c("Gender",'Product')] <- list(NULL)


summary(dane)


##################################################
#                   IV
##################################################
##wewnętrzna funkcja wyliczajšca IV dla każdej zmiennej, IV dostępne także w pakiecie ??InformationValue
RankingPredyktorow(dane,"y",1)

summary(dane)



##################################################
#                   WOE
##################################################
dane2<-dane

#przywracanie danych
#dane<-dane2


summary(dane$Age)

temp <- cut(dane$Age,c(min(dane$Age),0,33,39,45,62,max(dane$Age)),include.lowest = T)

table(temp)
# WOETable(temp,dane$y)
(levels(temp) <- WOETable(temp,dane$y)$WOE*100)
dane$Age <- as.numeric(as.character(temp))
summary(dane$Age)


summary(dane$M_LastPaymentToImportDate)
temp <- cut(dane$M_LastPaymentToImportDate,c(0,3,6,9,12,18,24,27,38,88,max(dane$M_LastPaymentToImportDate)),include.lowest = T)
table(temp)
(levels(temp) <- WOETable(temp,dane$y)$WOE*100)
dane$M_LastPaymentToImportDate <- as.numeric(as.character(temp))
table(dane$M_LastPaymentToImportDate)


summary(dane$DPD)
temp <- cut(dane$DPD,c(0,177,834,994,2430,max(dane$DPD)),include.lowest = T)
table(temp)
(levels(temp) <- WOETable(temp,dane$y)$WOE*100)
dane$DPD <- as.numeric(as.character(temp))
table(dane$DPD)



#summary(dane$D_ContractDateToImportDate)
temp <- cut(dane$D_ContractDateToImportDate,c(0,520,1950,2750,2970,3180,max(dane$D_ContractDateToImportDate)),include.lowest = T)
#
table(temp)
(levels(temp) <- WOETable(temp,dane$y)$WOE*100)
dane$D_ContractDateToImportDate <- as.numeric(as.character(temp))
#summary(dane$D_ContractDateToImportDate)


temp <- cut(dane$LoanAmount,c(0,1110,8000,10600,25600,29500,max(dane$LoanAmount)),include.lowest = T)
table(temp)
(levels(temp) <- WOETable(temp,dane$y)$WOE*100)
dane$LoanAmount <- as.numeric(as.character(temp))
summary(dane$LoanAmount)


temp <- cut(dane$LastPaymentAmount,c(0,0.5,26,85,142,180,308,max(dane$LastPaymentAmount)),include.lowest = T)
table(temp)
(levels(temp) <- WOETable(temp,dane$y)$WOE*100)
dane$LastPaymentAmount <- as.numeric(as.character(temp))
summary(dane$LastPaymentAmount)

summary(dane$Bailiff)
temp <- cut(dane$Bailiff,c(0,0.5,max(dane$Bailiff)),include.lowest = T)
table(temp)
(levels(temp) <- WOETable(temp,dane$y)$WOE*100)
dane$Bailiff <- as.numeric(as.character(temp))
summary(dane$Bailiff)

table(dane$IsMale)
temp <- cut(dane$IsMale,c(-1,-0.5,0,1),include.lowest = T)
table(temp)
(levels(temp) <- WOETable(temp,dane$y)$WOE*100)
dane$IsMale <- as.numeric(as.character(temp))
summary(dane$IsMale)

table(dane$ProductCashLoan)
temp <- cut(dane$ProductCashLoan,c(1,1.5,2,3,4),include.lowest = T)
table(temp)
(levels(temp) <- WOETable(temp,dane$y)$WOE*100)
dane$ProductCashLoan <- as.numeric(as.character(temp))
summary(dane$ProductCashLoan)

summary(dane$TOA) 
temp <- cut(dane$TOA,c(min(dane$TOA), c(293.36,1048.17,2022.06),max(dane$TOA)),include.lowest = T)
table(temp)
(levels(temp) <- WOETable(temp,dane$y)$WOE*100)
dane$TOA <- as.numeric(as.character(temp)) 


temp <- cut(dane$MeanSalary,c(min(dane$MeanSalary), c(1538,1644,1798), max(dane$MeanSalary)),include.lowest = T)
table(temp)
(levels(temp) <- WOETable(temp,dane$y)$WOE*100)
dane$MeanSalary <- as.numeric(as.character(temp)) 


summary(dane)
names(dane)


###############################################################
#podział na uczšcy i testowy
###############################################################

names(dane)
dane.num <- dane[,-c(1,4,5,7,8,9,10)]
#dane.num <- dane

names(dane.num)
summary(dane.num)


nrow(dane.num)
index.spr <- createDataPartition(dane.num$y, times=1, p=4/6, list=FALSE)
dim(index.spr)
dane.ucz <- dane.num[index.spr[,1],]
dane.tst<-dane.num[-index.spr[,1],]
dim(dane.ucz)
dim(dane.tst)
dim(dane.num)



abs(mean(dane.ucz$y)-mean(dane.num$y))

dane.ucz$wagi <- dane.ucz$y
dane.ucz$wagi[which(dane.ucz$wagi == 0)] <- table(dane.ucz$y)[2]/table(dane.ucz$y)[1]

names(dane.ucz)
table(dane.ucz$wagi)
str(dane.ucz)


################ MODEL bez wag#####################
names(dane.ucz)
# (formula1<-as.formula('y~.'))


(formula1<-as.formula(paste('y~',paste(names(dane.ucz)[-c(8,11)],collapse = '+'))))
#model1 <- glm(formula1, data = dane.ucz,family = binomial(link = 'logit'))
model0 <- glm(formula1, data = dane2,family = binomial(link = 'logit'))
model1 <- glm(formula1, data = dane.ucz,family = binomial(link = 'logit'))
summary(model0)
summary(model1)
# drop1(model1)
model1 <- step(model1,direction = "both")
summary(model1)


############## KORELACJE ##################

library(corrplot)
# 
library(corrgram)
kor <- cor(dane.num)
rownames(kor) <- substr(rownames(kor),1,4)
colnames(kor) <- substr(colnames(kor),1,4)
corrplot(kor, type="lower")
names(dane.num)
# corrplot(kor, type="lower",method = 'number')
# names(dane.num)[findCorrelation(kor, cutoff = .5)] # 
library(caret)


names(dane.ucz)
# (formula1<-as.formula('y~.'))
(formula2<-as.formula(paste('y~',paste(names(dane.ucz)[-c(5,8,11)],collapse = '+'))))
#model1 <- glm(formula1, data = dane.ucz,family = binomial(link = 'logit'))
model2 <- glm(formula2, data = dane.ucz,family = binomial(link = 'logit'))
summary(model2)
# drop1(model1)
model2 <- step(model2,direction = "both")
summary(model2)



# jak się dobrać do informacji o modelu
coefficients(model2) # model coefficients
confint(model2, level=0.95) # CIs for model parameters 
fitted(model2) # predicted values
residuals(model2) # residuals
anova(model2) # anova table 
vcov(model2) # covariance matrix for model parameters 
influence(model2) # regression diagnostics


################ MODEL z wagami #####################
names(dane.ucz)
# (formula1<-as.formula('y~.'))
(formula1w<-as.formula(paste('y~',paste(names(dane.ucz)[-c(5,8,11)],collapse = '+'))))
#model1 <- glm(formula1, data = dane.ucz,family = binomial(link = 'logit'))
model1w <- glm(formula1w, data = dane.ucz,family = binomial(link = 'logit'),weights = wagi)
summary(model1w)
# drop1(model1)
model1w <- step(model1w,direction = "both")
summary(model1w)
summary(model2)


scores_w <- predict.glm(model1w,dane.ucz,type="response")
summary(scores_w)
wynik_w <- data.frame(dane.ucz,scores_w)


#wewnętrzne funkcje, współczynniki sš też w ogólnodostępnych pakietach
#wykresy(dane.ucz,'y',model1)
gini.curve(model1w,wynik_w,"y")


scores_t <- predict.glm(model1,newdata=dane.tst,type="response")
wynik_t <- data.frame(dane.tst,scores_t)

#wykresy(dane.ucz,'y',model1)

gini.curve(model1,wynik_w,"y")
gini.curve(model1,wynik_t,"y")


class.real <- dane.ucz$y # real / actual class
class.real_t <- dane.tst$y 

scores.pred_bezwag <- predict(model2, dane.ucz, type = "response") # predicted class
scores.pred_bezwag_t <- predict(model2, newdata=dane.tst, type = "response")

scores.pred_zwagami <- predict(model1w, dane.ucz, type = "response") # predicted class
scores.pred_zwagami_t <- predict(model1w, newdata=dane.tst, type = "response")


# scoring    <- predict(model1, dane.ucz, type = "prob")[, "yes"] # predicted class probability


#rozkład
table(cut(scores.pred_bezwag*100,seq(0,100,20)),class.real)
table(cut(scores.pred_zwagami*100,seq(0,100,20)),class.real)

#wewnętrzne funkcja 
good.bad.density(model2,dane.ucz, "y","rozkład good/bad")
good.bad.density(model1w,dane.ucz, "y","rozkład good/bad")


#macierz klasyfikacji
class.pred_bezwag <- ifelse(scores.pred_bezwag > 0.3,1,0)
class.pred_bezwag_t <- ifelse(scores.pred_bezwag_t > 0.3,1,0)

class.pred_zwagami<-ifelse(scores.pred_zwagami > 0.5,1,0)
class.pred_zwagami_t<-ifelse(scores.pred_zwagami_t > 0.5,1,0)


table(class.real, class.pred_bezwag)
table(class.real_t, class.pred_bezwag_t)

table(class.real, class.pred_zwagami)
table(class.real_t, class.pred_zwagami_t)

prop.table(table(class.real, class.pred_bezwag),margin=2)
prop.table(table(class.real_t, class.pred_bezwag_t),margin=2)

prop.table(table(class.real, class.pred_zwagami),margin=2)
prop.table(table(class.real_t, class.pred_zwagami_t),margin=2)


library(pROC)
plot(roc(class.real, scores.pred_bezwag))
plot(roc(class.real, scores.pred_zwagami))

plotROC(dane.ucz$y, scores.pred_bezwag, Show.labels = FALSE, returnSensitivityMat = T) 
plotROC(dane.ucz$y, scores.pred_zwagami, Show.labels = FALSE, returnSensitivityMat = T) 




# czynnik inflacji wariancji

library(car)
vif(model2)