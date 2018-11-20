rm(list=ls())
setwd("/Users/hee-wonchang/Downloads/all/housepriceskaggle/Esther")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(VIM)
library(mice)
library(VIM)
#library(car)

house <- read.csv("train.csv")
house.c <- read.csv("train.csv")
house.test <- read.csv("test.csv")
aggr(house)

#Missingness
nacount <- sapply(house, function(y) sum(length(which(is.na(y)))))
nacount <- data.frame(nacount[nacount>0]) 
nacount #19 variables with at least one NA

nacount.test <- sapply(house.test, function(y) sum(length(which(is.na(y)))))
nacount.test <- data.frame(nacount.test[nacount.test>0]) 
nacount.test #33 variables with at least one NA

#Variables Manipulation
for(col in rownames(nacount)){
  if(class(house.c[,col])=="numeric" | class(house.c[,col])=="integer") print(col)}

house.c$LotFrontage[is.na(house$LotFrontage)] <- 0
house.c$MasVnrArea[is.na(house$MasVnrArea)] <- 0

house.c$Alley  <- as.character(house$Alley)
house.c$Alley[is.na(house.c$Alley)] <- "None"
house.c$Alley  <- factor(house.c$Alley, levels=c("None","Grvl","Pave"))

house.c$MasVnrType[is.na(house.c$MasVnrType)] <- "None"

house.c$Alley  <- as.character(house$Alley)
house.c$Alley[is.na(house.c$Alley)] <- "None"
house.c$Alley  <- factor(house.c$Alley, levels=c("None","Grvl","Pave"))

house.c$BsmtQual  <- as.character(house$BsmtQual)
house.c$BsmtQual[is.na(house.c$BsmtQual)] <- "None"
house.c$BsmtQual <- factor(house.c$BsmtQual, levels=c("None","Po","Fa","TA","Gd","Ex"))

house.c$BsmtCond  <- as.character(house$BsmtCond)
house.c$BsmtCond[is.na(house.c$BsmtCond)] <- "None"
house.c$BsmtCond <- factor(house.c$BsmtCond, levels=c("None","Po","Fa","TA","Gd","Ex"))

house.c$BsmtExposure[is.na(house.c$BsmtExposure)] <- "No"
house.c$BsmtExposure <- factor(house.c$BsmtExposure, levels=c("No","Mn","Av","Gd"))

house.c$BsmtFinType1 <- as.character(house$BsmtFinType1)
house.c$BsmtFinType1[is.na(house.c$BsmtFinType1)] <- "None"
house.c$BsmtFinType1 <- factor(house.c$BsmtFinType1, levels=c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))

house.c$BsmtFinType2 <- as.character(house$BsmtFinType2)
house.c$BsmtFinType2[is.na(house.c$BsmtFinType2)] <- "None"
house.c$BsmtFinType2 <- factor(house.c$BsmtFinType2, levels=c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))

house.c$FireplaceQu  <- as.character(house$FireplaceQu)
house.c$FireplaceQu[is.na(house.c$FireplaceQu)] <- "None"
house.c$FireplaceQu <- factor(house.c$FireplaceQu, levels=c("None","Po","Fa","TA","Gd","Ex"))

house.c$GarageType  <- as.character(house$GarageType)
house.c$GarageType[is.na(house.c$GarageType)] <- "None"
house.c$GarageType <- factor(house.c$GarageType, levels=c("None","Detchd","CarPort","BuiltIn",
                                                          "Basment","Attchd","2Types"))

house.c$GarageFinish <- as.character(house$GarageFinish)
house.c$GarageFinish[is.na(house.c$GarageFinish)] <- "None"
house.c$GarageFinish <- factor(house.c$GarageFinish, levels=c("None","Unf","RFn","Fin"))

house.c$GarageQual  <- as.character(house$GarageQual)
house.c$GarageQual[is.na(house.c$GarageQual)] <- "None"
house.c$GarageQual <- factor(house.c$GarageQual, levels=c("None","Po","Fa","TA","Gd","Ex"))

house.c$GarageCond  <- as.character(house$GarageCond)
house.c$GarageCond[is.na(house.c$GarageCond)] <- "None"
house.c$GarageCond <- factor(house.c$GarageCond, levels=c("None","Po","Fa","TA","Gd","Ex"))

house.c$PoolQC <- as.character(house$PoolQC)
house.c$PoolQC[is.na(house.c$PoolQC)] <- "None"
house.c$PoolQC <- factor(house.c$PoolQC, levels=c("None","Po","Fa","TA","Gd","Ex"))
   
house.c$Fence  <- as.character(house$Fence)
house.c$Fence[is.na(house.c$Fence)] <- "None"
house.c$Fence  <- factor(house.c$Fence, levels=c("None","GdPrv","GdWo","MnPrv","MnWw"))

house.c$MiscFeature  <- as.character(house$MiscFeature)
house.c$MiscFeature[is.na(house.c$MiscFeature)] <- "None"
house.c$MiscFeature  <- factor(house.c$MiscFeature)

########################KENT & NUMERICAL#######################
houses.n = house
houses.n$BsmtCond = recode(house$BsmtCond,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) #37NAs
houses.n$BsmtExposure = ifelse(is.na(house$BsmtExposure), 0, recode(house$BsmtExposure,'No'=0,'Mn'=1,'Av'=2,'Gd'=3))
houses.n$BsmtFinType1 = recode(house$BsmtFinType1, 'Unf'=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6) #37NAs
houses.n$BsmtFinType2 = recode(house$BsmtFinType2, 'Unf'=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6) #38NAs
houses.n$BsmtQual = recode(house$BsmtQual,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) #37NAs
houses.n$ExterCond = recode(house$ExterCond, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.n$ExterQual = recode(house$ExterQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.n$FireplaceQu = recode(house$FireplaceQu, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.n$Functional = recode(house$Functional, 'Sal'=1,'Sev'=2,'Maj2'=3,'Maj1'=4,'Mod'=5,'Min2'=6,'Min1'=7,'Typ'=8)
houses.n$GarageCond = recode(house$GarageCond, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) #81NAs
houses.n$GarageQual = recode(house$GarageQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) #81NAs
houses.n$HeatingQC = recode(house$HeatingQC, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.n$KitchenQual = recode(house$KitchenQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.n$LandSlope = recode(house$LandSlope, 'Sev'=3,'Mod'=2,'Gtl'=1)
houses.n$PavedDrive = recode(house$PavedDrive, 'N'=0,'P'=1,'Y'=2)
#newcols for numerical
houses.n$OverallGrade <- houses.n$OverallCond*houses.n$OverallQual
houses.n$GarageGrade <- houses.n$GarageCond*houses.n$GarageQual
houses.n$ExterGrade <- houses.n$ExterCond*houses.n$ExterQual
houses.n$KitchenScore <- houses.n$KitchenQual*houses.n$KitchenAbvGr
houses.n$FireplaceScore <- houses.n$Fireplaces*houses.n$FireplaceQu
houses.n$GarageScore <- houses.n$GarageArea*houses.n$GarageQual
houses.n$TotalBath <- houses.n$BsmtFullBath+(.5*houses.n$BsmtHalfBath)+houses.n$FullBath+(.5*houses.n$HalfBath)
houses.n$TotalSF <- houses.n$TotalBsmtSF+houses.n$GrLivArea
########################KENT & NUMERICAL#######################
house.c.woSalePrice <- house.c[,-81]

house.c.woSalePrice$OverallGrade <- houses.n$OverallGrade
house.c.woSalePrice$GarageGrade <- houses.n$GarageGrade
house.c.woSalePrice$ExterGrade <- houses.n$ExterGrade
house.c.woSalePrice$KitchenScore <- houses.n$KitchenScore
house.c.woSalePrice$FireplaceScore <- houses.n$FireplaceScore
house.c.woSalePrice$GarageScore <- houses.n$GarageScore
house.c.woSalePrice$TotalBath <- houses.n$TotalBath
house.c.woSalePrice$TotalSF <- houses.n$TotalSF

house.c.woSalePrice$GarageGrade[is.na(house.c.woSalePrice$GarageGrade)] <- 0
house.c.woSalePrice$FireplaceScore[is.na(house.c.woSalePrice$FireplaceScore)] <- 0
house.c.woSalePrice$GarageScore[is.na(house.c.woSalePrice$GarageScore)] <- 0

nacount.c <- sapply(house.c.woSalePrice, function(y) sum(length(which(is.na(y)))))
nacount.c <- data.frame(nacount.c[nacount.c>0]) 
nacount.c 

nacount.c <- sapply(house.test, function(y) sum(length(which(is.na(y)))))
nacount.c <- data.frame(Variable =names(nacount.c)[nacount.c>0], No.NA=nacount.c[nacount.c>0]) 
write.csv(nacount.c,"natest.csv") 
aggr(house.c[,c("Electrical","GarageYrBlt")])
########################################TEST DATA##############################
house.test <- read.csv("test.csv")

nacount.test <- sapply(house.test, function(y) sum(length(which(is.na(y)))))
nacount.test <- data.frame(nacount.test[nacount.test>0]) 
nacount.test
not.include <- rownames(nacount.test)

#Variables Manipulation
for(col in rownames(nacount)){
  if(class(house.test[,col])=="numeric" | class(house.test[,col])=="integer") print(col)}

house.test$LotFrontage[is.na(house.test$LotFrontage)] <- 0
house.test$MasVnrArea[is.na(house.test$MasVnrArea)] <- 0

house.test$Alley  <- as.character(house.test$Alley)
house.test$Alley[is.na(house.test$Alley)] <- "None"
house.test$Alley  <- factor(house.test$Alley, levels=c("None","Grvl","Pave"))

house.test$MasVnrType[is.na(house.test$MasVnrType)] <- "None"

house.test$Alley  <- as.character(house.test$Alley)
house.test$Alley[is.na(house.test$Alley)] <- "None"
house.test$Alley  <- factor(house.test$Alley, levels=c("None","Grvl","Pave"))

house.test$BsmtQual  <- as.character(house.test$BsmtQual)
house.test$BsmtQual[is.na(house.test$BsmtQual)] <- "None"
house.test$BsmtQual <- factor(house.test$BsmtQual, levels=c("None","Po","Fa","TA","Gd","Ex"))

house.test$BsmtCond  <- as.character(house.test$BsmtCond)
house.test$BsmtCond[is.na(house.test$BsmtCond)] <- "None"
house.test$BsmtCond <- factor(house.test$BsmtCond, levels=c("None","Po","Fa","TA","Gd","Ex"))

house.test$BsmtExposure[is.na(house.test$BsmtExposure)] <- "No"
house.test$BsmtExposure <- factor(house.test$BsmtExposure, levels=c("No","Mn","Av","Gd"))

house.test$BsmtFinType1 <- as.character(house.test$BsmtFinType1)
house.test$BsmtFinType1[is.na(house.test$BsmtFinType1)] <- "None"
house.test$BsmtFinType1 <- factor(house.test$BsmtFinType1, levels=c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))

house.test$BsmtFinType2 <- as.character(house.test$BsmtFinType2)
house.test$BsmtFinType2[is.na(house.test$BsmtFinType2)] <- "None"
house.test$BsmtFinType2 <- factor(house.test$BsmtFinType2, levels=c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))

house.test$FireplaceQu  <- as.character(house.test$FireplaceQu)
house.test$FireplaceQu[is.na(house.test$FireplaceQu)] <- "None"
house.test$FireplaceQu <- factor(house.test$FireplaceQu, levels=c("None","Po","Fa","TA","Gd","Ex"))

house.test$GarageType  <- as.character(house.test$GarageType)
house.test$GarageType[is.na(house.test$GarageType)] <- "None"
house.test$GarageType <- factor(house.test$GarageType, levels=c("None","Detchd","CarPort","BuiltIn",
                                                                "Basment","Attchd","2Types"))

house.test$GarageFinish <- as.character(house.test$GarageFinish)
house.test$GarageFinish[is.na(house.test$GarageFinish)] <- "None"
house.test$GarageFinish <- factor(house.test$GarageFinish, levels=c("None","Unf","RFn","Fin"))

house.test$GarageQual  <- as.character(house.test$GarageQual)
house.test$GarageQual[is.na(house.test$GarageQual)] <- "None"
house.test$GarageQual <- factor(house.test$GarageQual, levels=c("None","Po","Fa","TA","Gd","Ex"))

house.test$GarageCond  <- as.character(house.test$GarageCond)
house.test$GarageCond[is.na(house.test$GarageCond)] <- "None"
house.test$GarageCond <- factor(house.test$GarageCond, levels=c("None","Po","Fa","TA","Gd","Ex"))

house.test$PoolQC <- as.character(house.test$PoolQC)
house.test$PoolQC[is.na(house.test$PoolQC)] <- "None"
house.test$PoolQC <- factor(house.test$PoolQC, levels=c("None","Po","Fa","TA","Gd","Ex"))

house.test$Fence  <- as.character(house.test$Fence)
house.test$Fence[is.na(house.test$Fence)] <- "None"
house.test$Fence  <- factor(house.test$Fence, levels=c("None","GdPrv","GdWo","MnPrv","MnWw"))

house.test$MiscFeature  <- as.character(house.test$MiscFeature)
house.test$MiscFeature[is.na(house.test$MiscFeature)] <- "None"
house.test$MiscFeature  <- factor(house.test$MiscFeature)

########################KENT & NUMERICAL#######################
houses.test.n = read.csv("test.csv")
houses.test.n$BsmtCond = recode(houses.test.n$BsmtCond,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) #37NAs
houses.test.n$BsmtExposure = ifelse(is.na(houses.test.n$BsmtExposure), 0, recode(houses.test.n$BsmtExposure,'No'=0,'Mn'=1,'Av'=2,'Gd'=3))
houses.test.n$BsmtFinType1 = recode(houses.test.n$BsmtFinType1, 'Unf'=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6) #37NAs
houses.test.n$BsmtFinType2 = recode(houses.test.n$BsmtFinType2, 'Unf'=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6) #38NAs
houses.test.n$BsmtQual = recode(houses.test.n$BsmtQual,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) #37NAs
houses.test.n$ExterCond = recode(houses.test.n$ExterCond, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.test.n$ExterQual = recode(houses.test.n$ExterQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.test.n$FireplaceQu = recode(houses.test.n$FireplaceQu, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.test.n$Functional = recode(houses.test.n$Functional, 'Sal'=1,'Sev'=2,'Maj2'=3,'Maj1'=4,'Mod'=5,'Min2'=6,'Min1'=7,'Typ'=8)
houses.test.n$GarageCond = recode(houses.test.n$GarageCond, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) #81NAs
houses.test.n$GarageQual = recode(houses.test.n$GarageQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) #81NAs
houses.test.n$HeatingQC = recode(houses.test.n$HeatingQC, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.test.n$KitchenQual = recode(houses.test.n$KitchenQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.test.n$LandSlope = recode(houses.test.n$LandSlope, 'Sev'=3,'Mod'=2,'Gtl'=1)
houses.test.n$PavedDrive = recode(houses.test.n$PavedDrive, 'N'=0,'P'=1,'Y'=2)
#newcols for numerical
houses.test.n$OverallGrade <- houses.test.n$OverallCond*houses.test.n$OverallQual
houses.test.n$GarageGrade <- houses.test.n$GarageCond*houses.test.n$GarageQual
houses.test.n$ExterGrade <- houses.test.n$ExterCond*houses.test.n$ExterQual
houses.test.n$KitchenScore <- houses.test.n$KitchenQual*houses.test.n$KitchenAbvGr
houses.test.n$FireplaceScore <- houses.test.n$Fireplaces*houses.test.n$FireplaceQu
houses.test.n$GarageScore <- houses.test.n$GarageArea*houses.test.n$GarageQual
houses.test.n$TotalBath <- houses.test.n$BsmtFullBath+(.5*houses.test.n$BsmtHalfBath)+houses.test.n$FullBath+(.5*houses.test.n$HalfBath)
houses.test.n$TotalSF <- houses.test.n$TotalBsmtSF+houses.test.n$GrLivArea
########################KENT & NUMERICAL#######################
house.test$OverallGrade <- houses.test.n$OverallGrade
house.test$GarageGrade <- houses.test.n$GarageGrade
house.test$ExterGrade <- houses.test.n$ExterGrade
house.test$KitchenScore <- houses.test.n$KitchenScore
house.test$FireplaceScore <- houses.test.n$FireplaceScore
house.test$GarageScore <- houses.test.n$GarageScore
house.test$TotalBath <- houses.test.n$TotalBath
house.test$TotalSF <- houses.test.n$TotalSF

aggr(house.test)

SalePrice <- c(house.c$SalePrice, rep(1,nrow(house.test)))
Indicator <- c(rep("Train",nrow(house.c)),rep("Test",nrow(house.test)))
fullhouse.c <- data.frame(rbind(house.c.woSalePrice,house.test), SalePrice, Indicator)

fullhouse.c <- fullhouse.c[,-60] #do it once
fullhouse.c <- fullhouse.c[-which(is.na(house.c$Electrical)),] #do it once

################################## EDA for ESTHER & CATEGORICAL #############################
colnames(house.c.modeldata)


simpleEDAtable <- function(data, expvarlist){
      AIC.simple <- c()
      BIC.simple <- c()
      R2.simple <- c()
      for(exp in expvarlist) {
        AIC.simple <- c(AIC.simple, AIC(lm(log(data$SalePrice) ~ data[,exp])))
        BIC.simple <- c(BIC.simple, BIC(lm(log(data$SalePrice) ~ data[,exp])))
        R2.simple <- c(R2.simple, summary(lm(log(data$SalePrice) ~ data[,exp]))$adj.r.squared)}
      EDAtable <- data.frame(Variable=expvarlist, AIC=AIC.simple,BIC=BIC.simple,R2adj=R2.simple)
      EDAtable <- EDAtable %>% arrange(AIC)
      return(EDAtable)}

EDAtable1 <- simpleEDAtable(house.c,colnames(house.c)[-c(1,81)])
EDAtable2 <- simpleEDAtable(fullhouse.c[1:1459,],colnames(fullhouse.c)[-c(1,88,89)])

ordnum <- c("BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "BsmtQual", "ExterCond", 
            "ExterQual", "FireplaceQu", "Functional", "GarageCond", "GarageQual", "HeatingQC", 
            "KitchenQual", "LandSlope", "PavedDrive")

EDAtable3 <- simpleEDAtable(house.c,"PoolQC")
EDAtable4 <- simpleEDAtable(houses.n[1:1459,],ordnum)

ifelse(EDAtable3$AIC < EDAtable4$AIC, "c","n")
fullhouse.m <- fullhouse.c
fullhouse.m$FireplaceQu <- houses.n$FireplaceQu
fullhouse.m$LandSlope <- houses.n$LandSlope
house.m.train <- fullhouse.m[1:1459,]
house.m.test <- fullhouse.m[1460:2918,]
house.n.train <- houses.n[1:1459,]
house.c.train <- fullhouse.c[1:1459,]

################################
houses.n = fullhouse.c
houses.n$BsmtCond = recode(houses.n$BsmtCond,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) #37NAs
houses.n$BsmtExposure = ifelse(is.na(houses.n$BsmtExposure), 0, recode(houses.n$BsmtExposure,'No'=0,'Mn'=1,'Av'=2,'Gd'=3))
houses.n$BsmtFinType1 = recode(houses.n$BsmtFinType1, 'Unf'=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6) #37NAs
houses.n$BsmtFinType2 = recode(houses.n$BsmtFinType2, 'Unf'=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6) #38NAs
houses.n$BsmtQual = recode(houses.n$BsmtQual,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) #37NAs
houses.n$ExterCond = recode(houses.n$ExterCond, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.n$ExterQual = recode(houses.n$ExterQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.n$FireplaceQu = recode(houses.n$FireplaceQu, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.n$Functional = recode(houses.n$Functional, 'Sal'=1,'Sev'=2,'Maj2'=3,'Maj1'=4,'Mod'=5,'Min2'=6,'Min1'=7,'Typ'=8)
houses.n$GarageCond = recode(houses.n$GarageCond, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) #81NAs
houses.n$GarageQual = recode(houses.n$GarageQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5) #81NAs
houses.n$HeatingQC = recode(houses.n$HeatingQC, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.n$KitchenQual = recode(houses.n$KitchenQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses.n$LandSlope = recode(houses.n$LandSlope, 'Sev'=3,'Mod'=2,'Gtl'=1)
houses.n$PavedDrive = recode(houses.n$PavedDrive, 'N'=0,'P'=1,'Y'=2)


countoutlier <- function(variable){
  if(class(variable) == "integer" | class(variable) == "numeric"){
    var.iqr = IQR(variable, na.rm = TRUE)
    no.outlier.low = sum(variable < (quantile(variable, na.rm = TRUE)[2] - 3*var.iqr))
    no.outlier.hi = sum(variable > (quantile(variable, na.rm = TRUE)[4] + 3*var.iqr))
    return(no.outlier.low + no.outlier.hi)}
  else{return(0)}}

no.outlier.train <- c()
for(exp in colnames(house.m.train)){
  no.outlier.train <- c(no.outlier.train, countoutlier(house.m.train[,exp]))
}
trainoutliermatrix <- data.frame(Variable=colnames(house.m.train),No.Outlier=no.outlier.train)
train.outlier <- trainoutliermatrix %>% filter(No.Outlier >0) %>% arrange(desc(No.Outlier))

no.outlier.test <- c()
for(exp in colnames(house.m.test)){
  no.outlier.test <- c(no.outlier.test, countoutlier(house.m.test[,exp]))
}
testoutliermatrix <- data.frame(Variable=colnames(house.m.test),No.Outlier=no.outlier.test)
test.outlier <- testoutliermatrix %>% filter(No.Outlier >0) %>% arrange(desc(No.Outlier))

write.csv(train.outlier,"trainoutlier.csv")
write.csv(test.outlier,"testoutlier.csv")

#Data wo outliers
sp.outlier <- which(house.c.train$SalePrice > quantile(house.c.train$SalePrice)[4] + 3*IQR(house.c.train$SalePrice))
sf.outlier <- which(house.c.train$TotalSF > quantile(house.c.train$TotalSF)[4] + 3*IQR(house.c.train$TotalSF))
outliersrm <- union(sp.outlier,sf.outlier)

sp.outlier <- which(house.c$SalePrice > quantile(house.c$SalePrice)[4] + 3*IQR(house.c$SalePrice))

fullhouse.m.or <- fullhouse.m[]
house.m.train.or <- fullhouse.m[1:1459,]
house.m.modeldata.or <- house.m.train.or[,-c(which(colnames(house.c) %in% not.include),89)]

par(mfrow=c(1,3))
hist(house.c$SalePrice/1000, xlab="Sale Price ($K)",main = "House Sale Price \n (N=1460)")
hist(house.c$SalePrice[-sp.outlier]/1000, xlab="Sale Price ($K)",
     main = "House Sale Price \n without Extreme Outlier \n (N=1448)")
hist(log(house.c$SalePrice[-sp.outlier]), xlab="Log Sale Price ($)",
     main = "Log House Sale Price \nwithout Extreme Outlier \n (N=1448)")

countoutlier(house.c$SalePrice)
countoutlier(house.c$SalePrice)
countoutlier(log(house.c$SalePrice[-sp.outlier]))


############## Variable Selection################
house.m.modeldata.or <- house.m.train.or[,-c(which(colnames(house.c) %in% not.include),89)]

#stepwise
fullmodel.m.or <- lm(log(SalePrice)~.,data = house.m.modeldata.or)
backwardstep.m.or = stepAIC(fullmodel.m.or, direction = "backward")
step.m.exp.var <- names(backwardstep.m.or$xlevels)
stepmodel.fullhouse.m.or <- fullhouse.m.or[,c(step.m.exp.var,"SalePrice")]

#correlation & vif
edatable <- simpleEDAtable(house.m.modeldata.or,colnames(house.m.modeldata.or))[-1,]
library(car)
vif(lm(log(SalePrice)~OverallQual+GrLivArea+ExterQual+TotalBath+GarageScore+
         X1stFlrSF+FullBath,
       data=house.m.modeldata.or))
cor.exp.var <- c("OverallQual","GrLivArea","ExterQual","TotalBath","GarageScore",
                 "X1stFlrSF","FullBath")
cormodel.fullhouse.m.or <- fullhouse.m.or[,c(cor.exp.var,"SalePrice")]

#no missing or outlier
excludevar <- union(test.outlier$Variable,train.outlier$Variable)
np.exp.var <- colnames(house.m.modeldata.or)[-which(colnames(house.m.modeldata.or) %in% excludevar)]
npmodel.fullhouse.m.or <- fullhouse.m.or[,c(np.exp.var,"SalePrice")]

#
