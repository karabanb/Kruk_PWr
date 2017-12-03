#gcrm()
#setwd("C:/Users/dell/Desktop")

library(data.table)
library(gam)
load("KrukUWr2017.RData")

casesTmp <- copy(cases[Product == "Cash loan" & 
                         !is.na(LoanAmount + Land + PopulationInCiti + LastPaymentAmount), ])

#! Zadanie 1

# czy wpłata  
ifP <- events[casesTmp[, .SD, .SDcols=key(casesTmp)]][Month > 6, 
                                                      list(CaseId, ifP=ifelse(is.na(NumberOfPayment), 0 , 1))][, 
                                                                                                               list(CzyWplata=max(ifP)), by=CaseId]

# zmienne behawioralne        
eventsTmp <- events[Month <= 6, list(
  NumberOfLettersReceived=
    sum(ifelse(is.na(NumberOfLettersReceived), 0L, NumberOfLettersReceived)),
  PaymentAmount=
    sum(ifelse(is.na(PaymentAmount), 0.0, PaymentAmount)),
  NumberOfPayment=
    sum(ifelse(is.na(NumberOfPayment), 0L, NumberOfPayment)),
  NumberOfCallsWithClient=
    sum(ifelse(is.na(NumberOfCallsWithClient), 0L, NumberOfCallsWithClient)),
  NumberOfCalls=
    sum(ifelse(is.na(NumberOfCalls), 0L, NumberOfCalls))), 
  by=CaseId][casesTmp[, .SD, .SDcols=c(key(casesTmp), "TOA")]][, list(
    CaseId,
    NumberOfLettersReceived,
    PercentPayed=PaymentAmount*1.0/TOA,
    PhoneReach=ifelse(NumberOfCalls == 0, 0, 
                      NumberOfCallsWithClient*1.0/NumberOfCalls))]

# podzbiór spraw + czyszczenie    
casesTmp <- casesTmp[ifP][eventsTmp]
casesTmp[, `:=`(Product=NULL, Interest=NULL, Other=NULL, 
                ExternalAgency=NULL, Bailiff=NULL, ClosedExecution=NULL,
                Gender=ifelse(is.na(Gender), NA_integer_, ifelse(Gender == "MALE", 1L, 0L)),
                Age=ifelse(Age == -1, NA_integer_, Age),
                Repayment=1.0 - Principal/LoanAmount,
                PrincipalToTOA=Principal/TOA)]
casesTmp[, `:=`(LoanAmount=NULL, Principal=NULL)]

# podział na tabele learn, test & valid 
casesTmp[, Rnd:=runif(casesTmp[, .N])] 

casesLearn <- casesTmp[Rnd <= 0.4, ]  
casesTest <- casesTmp[Rnd > 0.4 & Rnd <= 0.7, ]                               
casesValid <- casesTmp[Rnd > 0.7, ] 

#save(list=c("casesLearn", "casesTest", "casesValid"), file="Data.RData")
#load("Data.RData")
#casesLearn[, X:=NULL]

divideVariablePlot <- function(data, variable, bins=10, prc=FALSE) {
  suppressWarnings(data[, X:=NULL])
  
  smpOdd <- data[, log((0.5 + sum(CzyWplata))/(0.5 + .N - sum(CzyWplata)))]
  
  if (data[is.na(get(variable)), .N] > 0) {
    naWOE <- data[is.na(get(variable)), 
                  log((0.5 + sum(CzyWplata))/(0.5 + .N - sum(CzyWplata)))] - smpOdd
    naSR <- data[is.na(get(variable)), mean(CzyWplata)]
    naN <- data[is.na(get(variable)), .N]
  }
  
  setorderv(data, cols=variable)
  
  if (prc) {
    quant <- unique(quantile(data[!is.na(get(variable)), ][[variable]], 
                             probs=seq(from=0, to=1, length.out=bins))) 
    
    data[!is.na(get(variable)), X:=cut(get(variable), breaks=quant, 
                                       include.lowest=TRUE)]
  } else { 
    data[!is.na(get(variable)), X:=cut(get(variable), breaks=bins)]
  }
  
  tmp <- data[!is.na(get(variable)), 
              list(SR=mean(CzyWplata), N=.N, 
                   WOE=log((0.5 + sum(CzyWplata))/(0.5 + .N - sum(CzyWplata))),
                   MaxX=max(get(variable))), by=X]
  data[, X:=NULL]
  tmp[, WOE:=WOE - smpOdd]
  
  par(mfrow=c(2, 1))
  plot(1:tmp[, .N], tmp$SR, type='o', ylab="SR", xlab=variable, xaxt="n",
       main="Freq.", col="tomato", lwd=3, 
       ylim=c(0.95*min(tmp$SR), 1.05*max(tmp$SR)))
  axis(side=1, at=1:tmp[, .N], labels=tmp$X, cex.axis=0.8)
  
  par(new=TRUE)
  plot(1:tmp[, .N], tmp$N/sum(tmp$N), type='h', ylab=NA, xlab=NA, axes=F,
       lwd=5, col="darkgreen", ylim=c(0, 1.05*max(tmp$N/sum(tmp$N))))
  axis(side=4)
  
  plot(1:tmp[, .N], tmp$SR, type='o', ylab="SR", xlab=variable, xaxt="n",
       main="WOE", col="tomato", lwd=3, ylim=c(0.95*min(tmp$SR), 1.05*max(tmp$SR)))
  axis(side=1, at=1:tmp[, .N], labels=tmp$X, cex.axis=0.8)
  
  par(new=TRUE)
  plot(1:tmp[, .N], tmp$WOE, type='b', ylab=NA, xlab=NA, axes=F,
       lwd=5, col="darkgreen")
  axis(side=4)
  
  if (exists("naSR", inherits=FALSE)) {
    tmp <- rbindlist(list(tmp, 
                          data.table(X=NA, SR=naSR, N=naN, WOE=naWOE, MaxX=NA)))
  }
  
  tmp
}  

decodeWOE <- function(data, code, variable) {
  data[, WOEX:=NA_real_]
  
  if (code[is.na(X), .N] == 1) {
    data[is.na(get(variable)), WOEX:=code[is.na(X), ]$WOE]
  }
  
  code <- code[!is.na(X), ]
  
  for (i in 1:code[, .N]) {
    data[get(variable) <= code[i, ]$MaxX & is.na(WOEX), WOEX:=code[i, ]$WOE]
  }
  
  data[]
}

summary(casesLearn)

variable <- "Age"
windows()
woeCode <- divideVariablePlot(casesLearn, variable, bins=7, prc=F)

casesLearn <- decodeWOE(casesLearn, code=woeCode, variable)  
casesTest <- decodeWOE(casesTest, code=woeCode, variable)
casesValid <- decodeWOE(casesValid, code=woeCode, variable)

odds <- casesTest[, log((0.5 + sum(CzyWplata))/(0.5 + .N - sum(CzyWplata)))] 
casesTest[, list(
  MinX=min(get(variable)), 
  MaxX=max(get(variable)), 
  N=.N, 
  WOE=log((0.5 + sum(CzyWplata))/(0.5 + .N - sum(CzyWplata))) - odds), by=WOEX]

odds <- casesValid[, log((0.5 + sum(CzyWplata))/(0.5 + .N - sum(CzyWplata)))] 
casesValid[, list(
  MinX=min(get(variable)), 
  MaxX=max(get(variable)), 
  N=.N, 
  WOE=log((0.5 + sum(CzyWplata))/(0.5 + .N - sum(CzyWplata))) - odds), by=WOEX]

#library(Information)
#IV <- Information::create_infotables(
#  data=casesLearn[, .SD, .SDcols=c("Age", "CzyWplata")], 
#  valid=casesTest[, .SD, .SDcols=c("Age", "CzyWplata")], 
#  y="CzyWplata", bins=5, parallel=FALSE)
#print(head(IV$Summary), row.names=FALSE)
#
#windows()
#SinglePlot(IV, "Age", show_values=TRUE)

#! Zadanie 2 
rocPlot <- function(mod, newData=NULL) {
  xsc <- seq(from=0, to=1, by=0.01)
  
  score <- predict(mod, type="response")
  cdfBad <- ecdf(score[which(mod$y == 0)]) 
  cdfGood <- ecdf(score[which(mod$y == 1)])
  
  gini <- 2*attributes(ROCR::performance(ROCR::prediction(score, 
                                                          mod$y), "auc"))$y.values[[1]] - 1
  
  plot(1 - cdfBad(xsc), 1 - cdfGood(xsc), 
       type="l", lwd=3, lty=1, xlim=c(0, 1), ylim=c(0, 1),
       xlab="F(s|BAD)", ylab="F(s|GOOD)", col="darkgreen",
       main=paste0("ROC; Gini=", round(gini, 2)))
  lines(x=c(0, 1), y=c(0, 1), lty=3, col="black", lwd=2)    
  
  if (!is.null(newData)) {
    score <- predict(mod, newData, type="response")
    cdfBad <- ecdf(score[which(newData$CzyWplata == 0)]) 
    cdfGood <- ecdf(score[which(newData$CzyWplata == 1)])
    
    lines(1 - cdfBad(xsc), 1 - cdfGood(xsc), lwd=2, lty=1, col="tomato")
  }
  
  invisible()
}

preMod <- gam(formula=as.formula(paste0("CzyWplata~", variable)), 
              data=casesLearn, family=binomial)

varList <- list()
dFree <- 1
varList[[variable]] <- as.formula(paste("~ 1 +", variable, "+", 
                                        paste0("s(", variable, ", ", seq(from=1, to=3, by=dFree), ")", 
                                               collapse=" + ")))
modGam <- step.gam(preMod, scope=varList)
# dla Age błšd funkcji pakietowej
#modGam <- gam(CzyWplata ~ s(Age, 3), data=casesLearn, family=binomial)
summary(modGam)

modGlmWOE <- glm(CzyWplata~WOEX, 
                 data=casesLearn, family=binomial)
summary(modGlmWOE)

modGlm <- glm(formula=as.formula(paste0("CzyWplata~", variable)), 
              data=casesLearn, family=binomial)
summary(modGlm)

# porównanie efektów
plotData <- preplot(modGam)[[1]]

plot(plotData$x, plotData$y, type="l", col="darkgreen", 
     xlab=variable, ylab="logit", lwd=3)
lines(casesLearn[[variable]], casesLearn$WOEX, lty=3, col="tomato", lwd=3)
abline(h=modGlm$coefficients[2], col="darkblue", lty=2, lwd=3)
legend("bottomright", legend=c("gam", "woe", "lin"), 
       col=c("darkgreen", "tomato", "darkblue"), lty=c(1, 3, 2))

lines(plotData$x, plotData$y + 1.96*plotData$se.y, 
      type="l", col="darkgreen", lty=3)
lines(plotData$x, plotData$y - 1.96*plotData$se.y, 
      type="l", col="darkgreen", lty=3)                          

par(mfrow=c(1,3))
rocPlot(modGam, casesTest)
rocPlot(modGlmWOE, casesTest)
rocPlot(modGlm, casesTest)

#! Zadanie 3
classMatrix <- function(data, score="sGam", thr=NULL) {
  if (is.null(thr)) {
    thr <- data[, mean(CzyWplata)]
  }
  
  data[, .N, by=list(CzyWplata, 
                     ifelse(!is.na(get(score)) & get(score) > thr, 1, 0))][
                       order(CzyWplata, ifelse)][]
}                              

summary(casesValid)

scoreGam <- predict(modGam, casesValid, type="response")
casesValid[, sGam:=scoreGam]
casesValid[is.na(sGam) & is.na(get(variable)), ]
classMatrix(casesValid)

#casesValid[1, Age:=83] #casesValid[1, Age:=54]
#predict(modGam, head(casesValid, 3), type="terms")

scoreGlmWOE <- predict(modGlmWOE, casesValid, type="response")

casesValid[, sGlmWOE:=scoreGlmWOE]
casesValid[is.na(get(variable)), ]
casesValid[is.na(sGlmWOE), ]

scoreGlm <- predict(modGlm, casesValid, type="response")
casesValid[, sGlm:=scoreGlm]
casesValid[is.na(sGlm) & is.na(get(variable)), ]

#! Zadanie 4
varList <- list()
dFree <- 1

variable1 <- "Age"
varList[[variable1]] <- as.formula(paste("~ 1 +", variable1, "+", 
                                         paste0("s(", variable1, ", ", seq(from=1, to=3, by=dFree), ")", 
                                                collapse=" + ")))

variable2 <- "PercentPayed"
varList[[variable2]] <- as.formula(paste("~ 1 +", variable2, "+", 
                                         paste0("s(", variable2, ", ", seq(from=1, to=3, by=dFree), ")", 
                                                collapse=" + ")))

preMod <- gam(formula=as.formula(
  paste0("CzyWplata~", variable1, "+", variable2)), 
  data=casesLearn, family=binomial)

modGam <- step.gam(preMod, scope=varList)
plot(modGam, ask=T)





