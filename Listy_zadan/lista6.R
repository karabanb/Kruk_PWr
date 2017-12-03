gcrm()

library(data.table)
load("C:/Users/Dell/Desktop/KrukUWr2017.RData")


variables <- c("TOA", "LoanAmount", "Principal", 
               "D_ContractDateToImportDate", "DPD", "PopulationInCiti", "Age", 
               "LastPaymentAmount", "M_LastPaymentToImportDate", "MeanSalary")

casesTmp <- copy(cases[Product == "Cash loan" & 
                         !is.na(LoanAmount + Land + PopulationInCiti + LastPaymentAmount), 
                       .SD, .SDcols=c("CaseId", variables)])
testSampleA <- casesTmp[sample(casesTmp[, .N], 3000), ]
caseTmpA <- casesTmp[!(CaseId %in% testSampleA$CaseId), ]
testSampleB <- casesTmp[(1:3000) + sample(50000, 1), ]
caseTmpB <- casesTmp[!(CaseId %in% testSampleB$CaseId), ]


#! Zadanie 1
trimedMean <- function(x) {
  avg <- mean(x, na.rm=TRUE)
  
  list(Avg=avg, 
       Trm05=mean(x, na.rm=TRUE, trim=0.005)/avg,
       Trm25=mean(x, na.rm=TRUE, trim=0.025)/avg, 
       Trm50=mean(x, na.rm=TRUE, trim=0.05)/avg)
}

cases[, lapply(.SD, trimedMean), 
      .SDcols=c("TOA", "Age", "DPD", "LastPaymentAmount",
                "M_LastPaymentToImportDate", "MeanSalary")]

#! Zadanie 2
variable <- "TOA"

bp <- boxplot(cases$TOA)
x <- cases[get(variable) > bp$stats[1] & 
             get(variable) < bp$stats[5], ][[variable]]

windows()
boxplot(x)

#! Zadanie 3
grubbs <- function(x, alpha=0.05) {
  N <- length(x)
  G <- max(abs(x - mean(x)))/sd(x)
  
  G > (N - 1)/sqrt(N)*sqrt((qt(0.5*alpha/N, N-2))^2/
                             (N - 2 + (qt(0.5*alpha/N, N-2))^2))
}

tietjenMoore <- function(x, k) {
  N <- length(x)
  y <- sort(x)
  
  yk <- mean(y[(k+1):N])
  yK <- mean(y[1:(N-k)])
  
  den <- sum((y - mean(y))^2)
  tmp <- data.table(z=y, r=abs(y - mean(y)))[order(r)]
  
  list(Lk=sum((y[(k+1):N] - yk)^2)/den, LK=sum((y[1:(N-k)] - yK)^2)/den,
       Ek=tmp[1:(N-k), sum((z - mean(z))^2)]/tmp[, sum((z - mean(z))^2)])
  
}

critValueTM <- function(N=54714, k=1000, alpha=0.05, n=10000) {
  res <- data.table()
  
  for (i in 1:n) {  
    res <- rbindlist(list(res, tietjenMoore(rnorm(N), k)))
  }
  
  res[, lapply(.SD, quantile, prob=alpha)]
}

genESD <- function(x, r=1000, alpha=0.05) {
  tmp <- data.table(X=x, xTmp=abs(x - mean(x)))[order(xTmp)]
  
  n <- tmp[, .N]
  R <- c()
  lambda <- c()
  
  for (i in 1:r) {
    R <- c(R, tmp[, max(xTmp)/sd(X)]) 
    lambda <- c(lambda, (n - i)*qt(1-alpha/(2*(n - i + 1)), n - i - 1)/
                  sqrt((n - i - 1 + (qt(alpha/(2*(n - i + 1)), n - i - 1))^2)*
                         (n - i + 1)))
    
    tmp <- tmp[1:(tmp[, .N] - 1), ]
    tmp[, xTmp:=abs(X - mean(X))]
    tmp <- tmp[order(xTmp)]
  } 
  
  sum(R > lambda)
}

grubbs(x=casesTmp$TOA)
tietjenMoore(x=casesTmp$TOA, k=1000)
critValueTM()
# 0.8839246 0.8838785 0.8634724
genESD(x=casesTmp$TOA)

#! Zadanie 4
library(DMwR) 
tmp <- copy(testSampleB[, .SD, 
                        .SDcols=setdiff(variables, c("CaseId", "LastPaymentAmount", 
                                                     "M_LastPaymentToImportDate"))])

outlier.scores <- lofactor(tmp, k=50)
outliers <- order(outlier.scores, decreasing=T)[1:500]

library(mvoutlier)
mv <- uni.plot(tmp, cex=0.03, cex.main=0.9)
table(mv$outliers)
outliers2 <- order(mv$md, decreasing=T)[1:500]

intersect(outliers,outliers2)

#! Zadanie 5
compHist <- function(sampleA=caseTmpB, sampleB=testSampleB,
                     variable="M_LastPaymentToImportDate", nBin=20) {
  data <- rbindlist(list(sampleA, sampleB))
  vMax <- max(data[[variable]])
  vMin <- min(data[[variable]])  
  
  if (data[, .N, by=variable][, .N] < 20) {
    tmpA <- sampleA[, .N/sampleA[, .N], by=variable][order(get(variable))]
    tmpB <- sampleB[, .N/sampleB[, .N], by=variable][order(get(variable))]
    
    yMax <- max(tmpA$V1, tmpB$V1)
    plot(tmpA[[variable]], tmpA$V1, type="p", lwd=6, col="darkblue",
         ylim=c(0,yMax), xlim=c(vMin, vMax), xlab=variable, ylab="Probability",
         main="Distribution comparison")
    lines(tmpB[[variable]], tmpB$V1, type="p", lwd=3, col="gold")
    legend("topleft", legend=c("SampleA", "SampleB"), lwd=c(3, 3), lty=c(1, 1),
           col=c("darkblue", "gold"))
  } else {
    x <- vMin + (1:nBin)*(vMax - vMin)/nBin
    tmp <- data.table()
    for (i in 2:nBin) {
      tmp <- rbindlist(list(tmp, 
                            data.table(
                              X=x[i], 
                              A=sampleA[x[i-1] < get(variable) & 
                                          get(variable) <= x[i], .N]/sampleA[, .N],
                              B=sampleB[x[i-1] < get(variable) & 
                                          get(variable) <= x[i], .N]/sampleB[, .N])))
    }
    
    yMax <- max(tmp$A, tmp$B)  
    plot(tmp$X, tmp$A, type="p", lwd=6, col="darkblue",
         ylim=c(0,yMax), xlim=c(vMin, vMax), xlab=variable, ylab="Probability",
         main="Distribution comparison")
    lines(tmp$X, tmp$B, type="p", lwd=3, col="gold", lty=3)
    legend("topleft", legend=c("SampleA", "SampleB"), lwd=c(3, 3), lty=c(1, 1),
           col=c("darkblue", "gold"))
  }         
  
  invisible()
}

compHist()

#! Zadanie 6
library(corrgram)
corrgram(caseTmpB, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")

windows()

corrgram(testSampleB, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")

#! Zadanie 7
caseTmpB[,AgeBond:=cut(Age,c(0,20,40,60,150))]
caseTmpB[,DPDBond:=cut(DPD,c(0,200,400,600,1500))]
tab1=table(caseTmpB$AgeBond,caseTmpB$DPDBond)

testSampleB[,AgeBond:=cut(Age,c(0,20,40,60,150))]
testSampleB[,DPDBond:=cut(DPD,c(0,200,400,600,1500))]
tab2=table(caseTmpB$AgeBond,caseTmpB$DPDBond)
mosaicplot(tab1) 
windows()
mosaicplot(tab2)