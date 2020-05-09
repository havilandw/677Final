library(readxl)
library(tidyverse)
library(reshape2)
library(fitdistrplus)
library(MASS)
library(pwr)

data <- read_xlsx("Countries-Confirmed.xlsx")
deaths <- read_xlsx("Countries-Deaths.xlsx")

readcountry <- function(name){
  country <- t(data[data$CNTRY_NAME==name,2:ncol(data)])
  return(country)
}



ice <- readcountry("Iceland")
ice <- ice[ice>0]
icedata <- ice - lag(ice,default=0)

hist(icedata)
descdist(icedata,discrete=T,boot=500)

alpha = 1
beta = 0

set.seed(100)
icepost <- rgamma(1000,sum(icedata)+alpha, length(icedata)+ beta)
hist(icepost)
abline(v=mean(icepost),col='magenta4')
mean(icedata)-mean(icepost)



nz <- readcountry("New Zealand")
nz <- nz[nz>0]
#nz <- nz[-c(1:7)]
nzdata <- nz - lag(nz,default=0)
hist(nzdata)
descdist(nzdata,discrete=T,boot=500)

set.seed(100)
nzpost <- rgamma(1000,sum(nzdata)+alpha, length(nzdata)+ beta)



t.test(icedata,nzdata)


