


# Downloading data



# A glance at the data

summary(Dane)


#  Decoding variables

Dane[,CreditCard := ifelse(Product=="Credit card",1,0)]
Dane[,Female := ifelse(Gender=="FEMALE",1,0)]


# Handling missing data

Mode <- function(x) {
  ux <- levels(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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

nullCounts <- lapply(Dane[, .SD, .SDcols=Variables], function(x) sum(is.na(x)))


# Imputation with avg

variables <- c(        "LoanAmount",
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
                       "MeanSalary"
)

for (variable in variables) {      ## variable = 'Age'
  if (eval(parse(text=paste("nullCounts$",variable,sep=""))) > 0) {
    avg <- eval(parse(text=paste("mean(Dane[,",variable,"],na.rm=TRUE)",sep="")))
    eval(parse(text=paste("Dane[is.na(",variable,"), ",variable,":=avg]",sep="")))
  }           
}


# Other imputation

Dane[is.na(Female),Female:= ifelse(runif(1,0,1)<Dane[,mean(Female,na.rm=TRUE)],1,0)]
Dane[is.na(Bailiff),Bailiff:= ifelse(runif(1,0,1)<Dane[,mean(Bailiff,na.rm=TRUE)],1,0)]

Dane[is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0]
Dane[is.na(ClosedExecution), ClosedExecution:= ifelse(runif(1,0,1)<Dane[,mean(ClosedExecution,na.rm=TRUE)],1,0)]



#  Proportion of tail data to be removed from the dataset

Proportion = 0.001

Dane <- Dane[LoanAmount<quantile(Dane[,LoanAmount], probs=1-Proportion),]
Dane <- Dane[DPD<quantile(Dane[,DPD], probs=1-Proportion),]
Dane <- Dane[LastPaymentAmount<quantile(Dane[,LastPaymentAmount], probs=1-Proportion),]


#  Good/bad clients

Dane[,GoodClient := 'B']
Dane[M1+M2+M3+M4+M5+M6 > 0, GoodClient := 'G']


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

cor(Dane[,.SD,.SDcols = Variables], use="pairwise.complete.obs", method="pearson")
cor(Dane[,.SD,.SDcols = Variables], use="pairwise.complete.obs", method="spearman")

library(Hmisc)
rcorr(as.matrix(Dane[,.SD,.SDcols = Variables]), type="spearman")

library(corrgram)
corrgram(Dane[,.SD,.SDcols = Variables], lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="ApplicationData")


library(corrplot)
corrplot(cor(Dane[,.SD,.SDcols = Variables]), order = "hclust", tl.col='black', tl.cex=.75)


#  VIF quick analysis

for (i in 1:length(Variables)) {
  model_lm <- lm(paste(Variables[i],paste(Variables[-i], collapse="+"),sep="~"), data = Dane)
  if (i == 1) {vif <- data.frame(Variable = Variables[i], AdjR2 = summary(model_lm)$adj.r.squared, VIF = summary(model_lm)$adj.r.squared/(1-summary(model_lm)$adj.r.squared))}
  else {
    vif = rbind(vif,data.frame(Variable = Variables[i], AdjR2 = summary(model_lm)$adj.r.squared, VIF = summary(model_lm)$adj.r.squared/(1-summary(model_lm)$adj.r.squared)))
  }
}


#  Decomposition of Data Matrices by Factors - basic example

library(MASS)
Example1 <- mvrnorm(n = 1000, mu = c(0,0), Sigma = matrix(c(1,0.6,0.6,1),2,2), empirical = FALSE)
plot(Example1, main="Direction in Data", xlab="x ", ylab="y", pch=19)
pca <- prcomp(Example1, center = TRUE, scale = TRUE)
eig <- eigen(cor(Example1))

segments(-4*pca$rotation[1,1],-4*pca$rotation[2,1],4*pca$rotation[1,1],4*pca$rotation[2,1])
segments(-4*pca$rotation[1,2],-4*pca$rotation[2,2],4*pca$rotation[1,2],4*pca$rotation[2,2])



#  Standardization of variables

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
                       "MeanSalary"
                       #"CreditCard",
                       #"Female",
                       #"Bailiff",
                       #"ClosedExecution"
)


DaneStd <- scale(Dane[,.SD,.SDcols = Variables])

summary(DaneStd)


#  Correlation Matrix

t(DaneStd)%*%DaneStd/dim(DaneStd)[1]
CorMatrix <- cor(DaneStd)


#   Principal Components

eigen(CorMatrix)
pca <- prcomp(DaneStd, center = FALSE, scale = FALSE)

summary(pca)


#  Principal components

options(scipen=999)
DanePCA <- data.table(DaneStd %*% pca$rotation)

DanePCA[,GoodClient := Dane[,GoodClient]]

summary(DanePCA)


#  Variation explained graph

plot(pca$sdev^2/sum(pca$sdev^2))
barplot(pca$sdev^2/sum(pca$sdev^2), names.arg = 1:13, main="Variance explained", xlab="PC")


#  Correlation between PCAs and Variables

CorMatrix <- pca$rotation %*% diag(pca$sdev)


#  Graph - correlation of the original std variables with the PCs

library(plotrix)

plot(CorMatrix[,c(1,2)], xlim=c(-1.2,1.2), ylim=c(-1.2,1.2), xlab="PC1", ylab="PC2", asp=1)
draw.circle(0, 0, radius = 1)
abline(v=0)
abline(h=0)
text(CorMatrix[,c(1,2)], labels = Variables)


#  Plot PC1-PC2 by groups

library(ggplot2)

qplot(PC3,PC4,data=DanePCA, colour = GoodClient, label=GoodClient, xlab = "PC3", ylab = "PC4",size=I(1))


library(scatterplot3d)
library(rgl)
library(car)
scatter3d(x = DanePCA[,PC3], y = DanePCA[,PC4], z = DanePCA[,PC2], groups = as.factor(DanePCA[,GoodClient]), surface=FALSE)


#  Simple DA in the space of PCs

library(pROC)

DanePCA[,Good := ifelse(GoodClient=="G",1,0)]
pairs <- combn(1:7,2)
auc <- list()

for (i in (1:dim(pairs)[2])) {   # i=1
  
  n <- dim(DanePCA)[1]
  index.learn <- sample(1:n, n*0.5)
  
  DaneTst <- DanePCA[index.learn,]
  DaneTrn <- DanePCA[-index.learn,]
  
  model <- glm(formula =
                 paste0("Good ~PC",pairs[1,i],"+PC",pairs[2,i])
               , data = DaneTrn, family = binomial(link = "logit"))
  
  scores <- predict.glm(model, DaneTst, type = "response")
  r <- roc(DaneTst$Good, scores, direction="<")
  
  #plot(roc(DaneTst$Good, scores, direction="<"),
  #col="yellow", lwd=3, main="ROC")
  auc[[i]] <- r$auc
  
}


###########################################################################################################


#  Factor analysis - no rotation

Factors <- factanal(DaneStd, factors = 8, rotation = "none", na.action = na.omit)
Factors$loadings

print(Factors$loadings, digits = 3, cutoff = 0.4, sort = TRUE)

# Compute eigenvalue of factor 1

loadings_fac1 <- Factors$loadings[,1]
eigenv_fac1 <- sum(loadings_fac1^2)


# Compute variance proportion

eigenv_fac1/dim(DaneStd)[2]


#  Uniqueness of variables

Factors$uniquenesses


#  Visualization of loadings

plot(Factors$loadings[,1:2], type="n") # set up plot 
text(Factors$loadings[,1:2],labels=Variables,cex=.7) # add variable names


#  Factor analysis with varimax rotation

Factors_VariMax <- factanal(DaneStd, factors = 2, rotation = "varimax", na.action = na.omit)
Factors_VariMax$loadings


#  Visualization of loadings

plot(Factors_VariMax$loadings[,1:2], type="n") # set up plot 
text(Factors_VariMax$loadings[,1:2],labels=Variables,cex=.7) # add variable names



library(psych)
fa(DaneStd, nfactors = 5)





