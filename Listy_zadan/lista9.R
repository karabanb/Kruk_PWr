

# A glance at the data

summary(Dane)

# Handling missing data

Mode <- function(x) {
  ux <- levels(x)
  ux[which.max(tabulate(match(x, ux)))]
}


Dane$LoanAmount[is.na(Dane$LoanAmount)] <- mean(Dane$LoanAmount, na.rm = TRUE)
Dane$Other[is.na(Dane$Other)] <- mean(Dane$Other, na.rm = TRUE)
Dane$D_ContractDateToImportDate[is.na(Dane$D_ContractDateToImportDate)] <- round(mean(Dane$D_ContractDateToImportDate, na.rm = TRUE))
Dane$PopulationInCiti[is.na(Dane$PopulationInCiti)] <- mean(Dane$PopulationInCiti, na.rm = TRUE)
Dane$Gender[is.na(Dane$Gender)] <- Mode(Dane$Gender)
Dane$LastPaymentAmount[is.na(Dane$LastPaymentAmount)] <- mean(Dane$LastPaymentAmount, na.rm = TRUE)
Dane$M_LastPaymentToImportDate[is.na(Dane$M_LastPaymentToImportDate)] <- round(mean(Dane$M_LastPaymentToImportDate, na.rm = TRUE))
Dane$GDPPerCapita[is.na(Dane$GDPPerCapita)] <- mean(Dane$GDPPerCapita, na.rm = TRUE)
Dane$MeanSalary[is.na(Dane$MeanSalary)] <- mean(Dane$MeanSalary, na.rm = TRUE)
Dane$Bailiff[is.na(Dane$Bailiff)] <- ifelse(runif(sum(is.na(Dane$Bailiff)),0,1)<0.399,1,0)
Dane$ClosedExecution[is.na(Dane$ClosedExecution) & Dane$Bailiff==0] <- 0
Dane$ClosedExecution[is.na(Dane$ClosedExecution)] <- ifelse(runif(sum(is.na(Dane$ClosedExecution)),0,1)<0.07303,1,0)


# Proportion of tail data to be removed from the dataset

Proportion = 0.001

Dane <- Dane[Dane$LoanAmount<quantile(Dane$LoanAmount, probs=1-Proportion),]
Dane <- Dane[Dane$DPD<quantile(Dane$DPD, probs=1-Proportion),]
Dane <- Dane[Dane$DPD<quantile(Dane$DPD, probs=1-Proportion),]
Dane <- Dane[Dane$LastPaymentAmount<quantile(Dane$LastPaymentAmount, probs=1-Proportion),]


# Good/bad clients

Dane$GoodClient <- 'B'
Dane$GoodClient[Dane$M1+Dane$M2+Dane$M3+Dane$M4+Dane$M5+Dane$M6 > 0] <- 'G'

Dane$GoodClient <- as.factor(Dane$GoodClient)


#  Decoding variables

Dane$CreditCard <- 0
Dane$CreditCard[Dane$Product == "Credit card"] <- 1

Dane$Female <- 0
Dane$Female[Dane$Gender == "FEMALE"] <- 1


#  Correlation analysis

Variables = c(         "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "Other",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "PopulationInCiti",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita",
                       "MeanSalary",
                       "CreditCard",
                       "Female",
                       "Bailiff",
                       "ClosedExecution"
)

cor(Dane[Variables], use="pairwise.complete.obs", method="pearson")
cor(Dane[Variables], use="pairwise.complete.obs", method="spearman")

library(Hmisc)
rcorr(as.matrix(Dane[Variables]), type="spearman")

library(corrgram)
corrgram(Dane[Variables], lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="ApplicationData")


library(corrplot)
corrplot(cor(Dane[Variables]), order = "hclust", tl.col='black', tl.cex=.75)


#  Matrix of R2 for quadratic relationship

options(scipen=999)

R2matrix <- matrix(nrow=dim(Dane[Variables])[2], ncol=dim(Dane[Variables])[2])
colnames(R2matrix) <- Variables
rownames(R2matrix) <- Variables

for (i in 1:dim(Dane[Variables])[2]) {
  for (j in 1:dim(Dane[Variables])[2]) {                   
    
    if (i != j) {model_lm <- lm(paste(Variables[i],paste(Variables[j],"+",Variables[j],"^2"),sep="~"), data = Dane)
    R2matrix[i,j] <- summary(model_lm)$adj.r.squared
    }
    if (i==j) R2matrix[i,j] <- 1
    
  }
}
R2matrix


#  VIF quick analysis

for (i in 1:dim(Dane[Variables])[2]) {
  model_lm <- lm(paste(Variables[i],paste(Variables[-i], collapse="+"),sep="~"), data = Dane)
  if (i == 1) {vif <- data.frame(Variable = Variables[i], AdjR2 = summary(model_lm)$adj.r.squared, VIF = summary(model_lm)$adj.r.squared/(1-summary(model_lm)$adj.r.squared))}
  else {
    vif = rbind(vif,data.frame(Variable = Variables[i], AdjR2 = summary(model_lm)$adj.r.squared, VIF = summary(model_lm)$adj.r.squared/(1-summary(model_lm)$adj.r.squared)))
  }
}
vif


# SR12M

Dane$SR12M <- (Dane$M1+Dane$M2+Dane$M3+Dane$M4+Dane$M5+Dane$M6+Dane$M7+Dane$M8+Dane$M9+Dane$M10+Dane$M11+Dane$M12)/Dane$TOA

summary(Dane)


# GAM modelling SR12M

library(gam)

#  Training and test sets

n = dim(Dane)[1]                           
index.learn = sample(1:n, dim(Dane)[1]/10)

DaneTst = Dane[-index.learn,]
DaneTrn = Dane[index.learn,]
summary(DaneTrn)


#  Model

model_SR12M <- gam(as.formula(paste("SR12M~",paste(Variables,collapse="+"))), family=gaussian(link=identity), data = DaneTrn)

summary(model_SR12M)  


#  Stepwise selection

Variables_Discrete = c("CreditCard","Female","Bailiff","ClosedExecution")

Lista <- c()
for (i in 1:length(Variables)) {
  
  if (sum(Variables[i] == Variables_Discrete) == 0) {
    Lista[[i]] = as.formula(paste("~1+", Variables[i], 
                                  "+s(", Variables[i], ",2)", "+s(", 
                                  Variables[i], ",3)", sep = ""))
  }
  else {
    Lista[[i]] <- as.formula(paste("~1+", Variables[i], 
                                   sep = ""))
  }
}

step.model_SR12M <-step.gam(model_SR12M, scope=Lista, dir='both')

summary(step.model_SR12M)


#  Final model

model_SR12M <- gam(
  SR12M~
    s(LoanAmount,3) +                    
    s(TOA,3) +                            
    s(Principal,3) +                       
    s(D_ContractDateToImportDate,3) +     
    s(DPD,3) +                             
    PopulationInCiti +                                                         
    s(Age,3) +                             
    s(LastPaymentAmount,3) +               
    s(M_LastPaymentToImportDate,3) +       
    GDPPerCapita +                                                             
    MeanSalary +                                                               
    Female                              
  , family=gaussian(link=identity), data = DaneTrn)

summary(model_SR12M)


#  Partial prediction plots

plot.gam(model_SR12M,ask=TRUE)


#  Payments 12M Forecast

sum(predict.gam(model_SR12M, newdata=DaneTst, type='response')*DaneTst$TOA)
sum(DaneTst$M1+DaneTst$M2+DaneTst$M3+DaneTst$M4+DaneTst$M5+DaneTst$M6+DaneTst$M7+DaneTst$M8+DaneTst$M9+DaneTst$M10+DaneTst$M11+DaneTst$M12)

plot(predict(model_SR12M, newdata=DaneTst, type='response'),DaneTst[,c("SR12M")], xlab="Forecasts", ylab="Actuals", ylim=c(0,1.5))


# GAM modelling SR12M  - binary response variable



Dane$Good <- ifelse(Dane$GoodClient == 'B',0,1)

#  Training and test sets

n = dim(Dane)[1]
index.learn = sample(1:n, dim(Dane)[1]/10)

DaneTst = Dane[-index.learn,]
DaneTrn = Dane[index.learn,]
summary(DaneTrn)




#  Model

model_SR12M <- gam(as.formula(paste("Good~",paste(Variables,collapse="+"))), family=binomial(link = "logit"), data = DaneTrn)

summary(model_SR12M)


#  Stepwise selection

Variables_Discrete = c("CreditCard","Female","Bailiff","ClosedExecution")

Lista <- c()
for (i in 1:length(Variables)) {
  
  if (sum(Variables[i] == Variables_Discrete) == 0) {
    Lista[[i]] = as.formula(paste("~1+", Variables[i],
                                  "+s(", Variables[i], ",2)", "+s(",
                                  Variables[i], ",3)", sep = ""))
  }
  else {
    Lista[[i]] <- as.formula(paste("~1+", Variables[i],
                                   sep = ""))
  }
}

step.model_SR12M <-step.gam(model_SR12M, scope=Lista, dir='both')

summary(step.model_SR12M)


#  Final model

model_SR12M <- gam(
  Good ~
    LoanAmount +
    Interest +
    s(Other, 3) +
    s(D_ContractDateToImportDate, 3) +
    s(DPD, 3) +
    s(Age, 3) +
    s(LastPaymentAmount, 3) +
    s(M_LastPaymentToImportDate, 3) +
    s(GDPPerCapita, 3) +
    s(MeanSalary, 3) +
    CreditCard +
    Female
  , family=binomial(link = "logit"), data = DaneTrn)

summary(model_SR12M)


#  Partial prediction plots

plot.gam(model_SR12M,ask=TRUE)


#  Payments 12M Forecast

DaneTrn$Forecast <- predict.gam(model_SR12M, newdata=DaneTrn, type='response')
DaneTst$Forecast <- predict.gam(model_SR12M, newdata=DaneTst, type='response')

Dane$Forecast <- predict.gam(model_SR12M, newdata=Dane, type='response')

Dane$Band <- cut(Dane$Forecast, breaks = 5)

DaneTrn$Band <- Dane$Band[index.learn]
DaneTst$Band <- Dane$Band[-index.learn]

Payments_Group <- tapply(DaneTrn$M1+DaneTrn$M2+DaneTrn$M3+DaneTrn$M4+DaneTrn$M5+DaneTrn$M6+DaneTrn$M7+DaneTrn$M8+DaneTrn$M9+DaneTrn$M10+DaneTrn$M11+DaneTrn$M12,DaneTrn$Band,sum)
TOA_Group <- tapply(DaneTrn$TOA,DaneTrn$Band,sum)
SR_Group <- Payments_Group/TOA_Group

TOA_Group_Tst <- tapply(DaneTst$TOA,DaneTst$Band,sum)

sum(SR_Group*TOA_Group_Tst)
sum(DaneTst$M1+DaneTst$M2+DaneTst$M3+DaneTst$M4+DaneTst$M5+DaneTst$M6+DaneTst$M7+DaneTst$M8+DaneTst$M9+DaneTst$M10+DaneTst$M11+DaneTst$M12)


#  Gini

scores <- predict.gam(model_SR12M, DaneTst, type = "response")

library(pROC)
plot(roc(DaneTst$Good, scores, direction="<"), col="yellow", lwd=3, main="ROC_BinaryModel")


#  Concurvity

library(mgcv)

model_SR12M <- gam(
  Good ~
    LoanAmount +
    Interest +
    s(Other) +
    s(D_ContractDateToImportDate) +
    s(DPD) +
    s(Age) +
    s(LastPaymentAmount) +
    s(M_LastPaymentToImportDate) +
    s(GDPPerCapita) +
    s(MeanSalary) +
    CreditCard +
    Female
  , family=binomial(link = "logit"), data = DaneTrn)


summary(model_SR12M)
plot(model_SR12M)

concurvity(model_SR12M,full=TRUE)
concurvity(model_SR12M,full=FALSE)



vis.concurvity(model_SR12M)



vis.concurvity <- function(b, type="estimate"){
  cc <- concurvity(b, full=FALSE)[[type]]
  
  diag(cc) <- NA
  cc[lower.tri(cc)]<-NA
  
  layout(matrix(1:2, ncol=2), widths=c(5,1))
  opar <- par(mar=c(5, 6, 5, 0) + 0.1)
  # main plot
  image(z=cc, x=1:ncol(cc), y=1:nrow(cc), ylab="", xlab="",
        axes=FALSE, asp=1, zlim=c(0,1))
  axis(1, at=1:ncol(cc), labels = colnames(cc), las=2)
  axis(2, at=1:nrow(cc), labels = rownames(cc), las=2)
  # legend
  opar <- par(mar=c(5, 0, 4, 3) + 0.1)
  image(t(matrix(rep(seq(0, 1, len=100), 2), ncol=2)),
        x=1:3, y=1:101, zlim=c(0,1), axes=FALSE, xlab="", ylab="")
  axis(4, at=seq(1,101,len=5), labels = round(seq(0,1,len=5),1), las=2)
  par(opar)
}

