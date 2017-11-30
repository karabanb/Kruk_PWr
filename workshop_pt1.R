
library(tidyverse)
library(Information)
library(smbinning)
library(mlr)
library(caret)
library(xray)

####### loading data frames ##########

load("datasets_PWr.Rdata")

anom <- anomalies(events)
