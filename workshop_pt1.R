
library(tidyverse)
library(Information)
library(smbinning)
library(mlr)
library(caret)
library(xray)

####### loading data frames ##########

load("datasets_PWr.Rdata")


###### ekslporacyjna analiza danych ###

anom <- anomalies(events)

anom$problem_variables

#### 

raw.data <- events %>%
              group_by(., CaseId)%>%
              summarise(., 
                           if_conluded = sum(!is.na(NumberOfAgreementConcluded)),
                           if_signed = sum(!is.na(NumberOfAgreementSigned)),
                           if_to_legal = sum(!is.na(TransferToLegalProcess)),
                           sum_of_payments = sum(PaymentAmount,na.rm = TRUE)
                        )%>%
              mutate(.,
                          if_conluded = if_else(if_conluded>0, 1,0),
                          if_signed = if_else(if_signed>0, 1, 0),
                          if_to_legal = if_else(if_to_legal>0, 1, 0)
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
              



###### training in mlr #######

sukces <- "if_conluded"

data <- raw.data%>%
  select(., -if_signed, -if_conluded, -sum_of_payments, -CaseId)%>%
  mutate(., if_to_legal = as.factor(if_else(if_to_legal>0, 'bad', 'good')))

calssif.task <- makeClassifTask(
                                  data = data,
                                  target = "if_to_legal"
                                  )

lrn.rpart <- makeLearner("classif.rpart", par.vals = list(cp = 0.01, maxdepth = 20), minsplit = 1500,predict.type = "prob")

rdesc <- makeResampleDesc(method = "CV", stratify = TRUE)

r <- resample(lrn.rpart,calssif.task,rdesc, models = TRUE, measures = list(acc, auc))

m1 <- mlr::train(lrn.rpart, calssif.task)

pred.m1 <- predict(m1, newdata = data, type = "class", list = FALSE)$data

confusionMatrix(data = pred.m1$response, reference = pred.m1$truth)


