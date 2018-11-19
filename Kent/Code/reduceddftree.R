houses=read.csv('./train.csv', stringsAsFactors = FALSE)
test=read.csv('./test.csv', stringsAsFactors = FALSE)
test$SalePrice=5

houses$Id=NULL #delete Id column
houses$GarageYrBlt=NULL #removing GarageYrBlt
houses$SalePrice=log(houses$SalePrice) #taking the log of SalePrice
houses$Alley=ifelse(is.na(houses$Alley), 'None', houses$Alley)
houses$BedroomAbvGr=ifelse(is.na(houses$BedroomAbvGr), 0, houses$BedroomAbvGr)
houses$BsmtQual=ifelse(is.na(houses$BsmtQual), 'None', houses$BsmtQual)
houses$BsmtCond=ifelse(is.na(houses$BsmtCond), 'None', houses$BsmtCond)
houses$BsmtExposure=ifelse(is.na(houses$BsmtExposure), 'No', houses$BsmtExposure)
houses$BsmtFinType1=ifelse(is.na(houses$BsmtFinType1), 'None', houses$BsmtFinType1)
houses$BsmtFinType2=ifelse(is.na(houses$BsmtFinType2), 'None', houses$BsmtFinType2)
houses$BsmtFullBath=ifelse(is.na(houses$BsmtFullBath), 0, houses$BsmtFullBath)
houses$BsmtHalfBath=ifelse(is.na(houses$BsmtHalfBath), 0, houses$BsmtHalfBath)
houses$BsmtUnfSF=ifelse(is.na(houses$BsmtUnfSF), 0, houses$BsmtUnfSF)
houses$CentralAir=ifelse(is.na(houses$CentralAir), 'None', houses$CentralAir)
houses$Condition1=ifelse(is.na(houses$Condition1), 'Norm', houses$Condition1)
houses$Condition2=ifelse(is.na(houses$Condition2), 'Norm', houses$Condition2)
houses$EnclosedPorch=ifelse(is.na(houses$EnclosedPorch), 0, houses$EnclosedPorch)
houses$ExterCond=ifelse(is.na(houses$ExterCond), "TA", houses$ExterCond)
houses$ExterQual=ifelse(is.na(houses$ExterQual), "TA", houses$ExterQual)
houses$Fence=ifelse(is.na(houses$Fence), "None", houses$Fence)
houses$FireplaceQu=ifelse(is.na(houses$FireplaceQu), "None", houses$FireplaceQu)
houses$Fireplaces=ifelse(is.na(houses$Fireplaces), 0, houses$Fireplaces)
houses$Functional=ifelse(is.na(houses$Functional), "Typ", houses$Functional)
houses$GarageType=ifelse(is.na(houses$GarageType), "None", houses$GarageType)
houses$GarageFinish=ifelse(is.na(houses$GarageFinish), "None", houses$GarageFinish)
houses$GarageQual=ifelse(is.na(houses$GarageQual), "None", houses$GarageQual)
houses$GarageCond=ifelse(is.na(houses$GarageCond), "None", houses$GarageCond)
houses$GarageArea=ifelse(is.na(houses$GarageArea), 0, houses$GarageArea)
houses$GarageCars=ifelse(is.na(houses$GarageCars), 0, houses$GarageCars)
houses$HalfBath=ifelse(is.na(houses$HalfBath), 0, houses$HalfBath)
houses$HeatingQC=ifelse(is.na(houses$HeatingQC), "TA", houses$HeatingQC)
houses$KitchenAbvGr=ifelse(is.na(houses$KitchenAbvGr), 0, houses$KitchenAbvGr)
houses$KitchenQual=ifelse(is.na(houses$KitchenQual), "TA", houses$KitchenQual)
houses$LotFrontage=ifelse(is.na(houses$LotFrontage), 0, houses$LotFrontage)
houses$LotShape=ifelse(is.na(houses$LotShape), "Reg", houses$LotShape)
houses$MasVnrType=ifelse(is.na(houses$MasVnrType), "None", houses$MasVnrType)
houses$MasVnrArea=ifelse(is.na(houses$MasVnrArea), 0, houses$MasVnrArea)
houses$MiscFeature=ifelse(is.na(houses$MiscFeature), "None", houses$MiscFeature)
houses$MiscVal=ifelse(is.na(houses$MiscVal), 0, houses$MiscVal)
houses$OpenPorchSF=ifelse(is.na(houses$OpenPorchSF), 0, houses$OpenPorchSF)
houses$PavedDrive=ifelse(is.na(houses$PavedDrive), "N", houses$PavedDrive)
houses$PoolQC=ifelse(is.na(houses$PoolQC), "No", houses$PoolQC)
houses$PoolArea=ifelse(is.na(houses$PoolArea), 0, houses$PoolArea)
houses$SaleCondition=ifelse(is.na(houses$SaleCondition), "Normal", houses$SaleCondition)
houses$ScreenPorch=ifelse(is.na(houses$ScreenPorch), 0, houses$ScreenPorch)
houses$TotRmsAbvGrd=ifelse(is.na(houses$TotRmsAbvGrd), 0, houses$TotRmsAbvGrd)
houses$Utilities=ifelse(is.na(houses$Utilities), "AllPub", houses$Utilities)
houses$WoodDeckSF=ifelse(is.na(houses$WoodDeckSF), 0, houses$WoodDeckSF)
houses=houses[-which(is.na(houses$Electrical==TRUE)),]#remove row with missing electrical data

#numerical to categorical 
houses$MSSubClass = recode(houses$MSSubClass,'20'='Sub20','30'='Sub30','40'='Sub40','45'='Sub45','50'='Sub50','60'='Sub60','70'='Sub70','75'='Sub75','80'='Sub80','85'='Sub85','90'='Sub90','120'='Sub120','150'='Sub150','160'='Sub160','180'='Sub180','190'='Sub190')
houses$MoSold = recode(houses$MoSold, '1'='Jan','2'='Feb','3'='Mar','4'='Apr','5'='May','6'='Jun','7'='Jul','8'='Aug','9'='Sep','10'='Oct','11'='Nov','12'='Dec')

#categorcial to numerical
houses$Alley = recode(houses$Alley, 'None'=0,'Grvl'=1,'Pave'=2)
houses$BsmtCond = recode(houses$BsmtCond, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses$BsmtExposure = recode(houses$BsmtExposure, 'No'=0,'Mn'=1,'Av'=2,'Gd'=3)
houses$BsmtFinType1 = recode(houses$BsmtFinType1, 'None'=0,'Unf'=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6)
houses$BsmtFinType2 = recode(houses$BsmtFinType2, 'None'=0,'Unf'=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6)
houses$BsmtQual = recode(houses$BsmtQual, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses$ExterCond = recode(houses$ExterCond, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses$ExterQual = recode(houses$ExterQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses$FireplaceQu = recode(houses$FireplaceQu, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses$Functional = recode(houses$Functional, 'Sal'=1,'Sev'=2,'Maj2'=3,'Maj1'=4,'Mod'=5,'Min2'=6,'Min1'=7,'Typ'=8)
houses$GarageCond = recode(houses$GarageCond, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses$GarageQual = recode(houses$GarageQual, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses$HeatingQC = recode(houses$HeatingQC, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses$KitchenQual = recode(houses$KitchenQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
houses$LandSlope = recode(houses$LandSlope, 'Sev'=1,'Mod'=2,'Gtl'=3)
houses$LotShape = recode(houses$LotShape, 'IR3'=1,'IR2'=2,'IR1'=3,'Reg'=4)
houses$PavedDrive = recode(houses$PavedDrive, 'N'=0,'P'=1,'Y'=2)
houses$PoolQC = recode(houses$PoolQC, 'No'=0,'Fa'=1,'TA'=2,'Gd'=3,'Ex'=4)
houses$Street = recode(houses$Street, 'Grvl'=1,'Pave'=2)
houses$Utilities = recode(houses$Utilities, 'Elo'=1,'NoSeWa'=2,'NoSewr'=3,'AllPub'=4)

houses$OverallGrade=houses$OverallCond*houses$OverallQual
houses$GarageGrade=houses$GarageCond*houses$GarageQual
houses$ExterGrade=houses$ExterCond*houses$ExterQual
houses$KitchenScore=houses$KitchenQual*houses$KitchenAbvGr
houses$FireplaceScore=houses$Fireplaces*houses$FireplaceQu
houses$GarageScore=houses$GarageArea*houses$GarageQual
houses$PoolScore=houses$PoolArea*houses$PoolQC
houses$TotalBath=houses$BsmtFullBath+(.5*houses$BsmtHalfBath)+houses$FullBath+(.5*houses$HalfBath)
houses$TotalSF=houses$TotalBsmtSF+houses$GrLivArea

str(houses)
houses$MSSubClass=as.factor(houses$MSSubClass)
houses$MSZoning=as.factor(houses$MSZoning)
houses$LandContour=as.factor(houses$LandContour)
houses$LotConfig=as.factor(houses$LotConfig)
houses$Neighborhood=as.factor(houses$Neighborhood)
houses$Condition1=as.factor(houses$Condition1)
houses$Condition2=as.factor(houses$Condition2)
houses$BldgType=as.factor(houses$BldgType)
houses$HouseStyle=as.factor(houses$HouseStyle)
houses$RoofStyle=as.factor(houses$RoofStyle)
houses$RoofMatl=as.factor(houses$RoofMatl)
houses$Exterior1st=as.factor(houses$Exterior1st)
houses$Exterior2nd=as.factor(houses$Exterior2nd)
houses$MasVnrType=as.factor(houses$MasVnrType)
houses$Foundation=as.factor(houses$Foundation)
houses$Heating=as.factor(houses$Heating)
houses$CentralAir=as.factor(houses$CentralAir)
houses$Electrical=as.factor(houses$Electrical)
houses$GarageType=as.factor(houses$GarageType)
houses$GarageFinish=as.factor(houses$GarageFinish)
houses$Fence=as.factor(houses$Fence)
houses$MiscFeature=as.factor(houses$MiscFeature)
houses$MoSold=as.factor(houses$MoSold)
houses$SaleType=as.factor(houses$SaleType)
houses$SaleCondition=as.factor(houses$SaleCondition)
houses$HouseStyle=as.character(houses$HouseStyle)  



#######
test$Id=NULL #delete Id column
test$GarageYrBlt=NULL #removing GarageYrBlt
test$SalePrice=log(test$SalePrice) #taking the log of SalePrice
test$Alley=ifelse(is.na(test$Alley), 'None', test$Alley)
test$BedroomAbvGr=ifelse(is.na(test$BedroomAbvGr), 0, test$BedroomAbvGr)
test$BsmtQual=ifelse(is.na(test$BsmtQual), 'None', test$BsmtQual)
test$BsmtCond=ifelse(is.na(test$BsmtCond), 'None', test$BsmtCond)
test$BsmtExposure=ifelse(is.na(test$BsmtExposure), 'No', test$BsmtExposure)
test$BsmtFinType1=ifelse(is.na(test$BsmtFinType1), 'None', test$BsmtFinType1)
test$BsmtFinType2=ifelse(is.na(test$BsmtFinType2), 'None', test$BsmtFinType2)
test$BsmtFullBath=ifelse(is.na(test$BsmtFullBath), 0, test$BsmtFullBath)
test$BsmtHalfBath=ifelse(is.na(test$BsmtHalfBath), 0, test$BsmtHalfBath)
test$BsmtUnfSF=ifelse(is.na(test$BsmtUnfSF), 0, test$BsmtUnfSF)
test$CentralAir=ifelse(is.na(test$CentralAir), 'None', test$CentralAir)
test$Condition1=ifelse(is.na(test$Condition1), 'Norm', test$Condition1)
test$Condition2=ifelse(is.na(test$Condition2), 'Norm', test$Condition2)
test$EnclosedPorch=ifelse(is.na(test$EnclosedPorch), 0, test$EnclosedPorch)
test$ExterCond=ifelse(is.na(test$ExterCond), "TA", test$ExterCond)
test$ExterQual=ifelse(is.na(test$ExterQual), "TA", test$ExterQual)
test$Fence=ifelse(is.na(test$Fence), "None", test$Fence)
test$FireplaceQu=ifelse(is.na(test$FireplaceQu), "None", test$FireplaceQu)
test$Fireplaces=ifelse(is.na(test$Fireplaces), 0, test$Fireplaces)
test$Functional=ifelse(is.na(test$Functional), "Typ", test$Functional)
test$GarageType=ifelse(is.na(test$GarageType), "None", test$GarageType)
test$GarageFinish=ifelse(is.na(test$GarageFinish), "None", test$GarageFinish)
test$GarageQual=ifelse(is.na(test$GarageQual), "None", test$GarageQual)
test$GarageCond=ifelse(is.na(test$GarageCond), "None", test$GarageCond)
test$GarageArea=ifelse(is.na(test$GarageArea), 0, test$GarageArea)
test$GarageCars=ifelse(is.na(test$GarageCars), 0, test$GarageCars)
test$HalfBath=ifelse(is.na(test$HalfBath), 0, test$HalfBath)
test$HeatingQC=ifelse(is.na(test$HeatingQC), "TA", test$HeatingQC)
test$KitchenAbvGr=ifelse(is.na(test$KitchenAbvGr), 0, test$KitchenAbvGr)
test$KitchenQual=ifelse(is.na(test$KitchenQual), "TA", test$KitchenQual)
test$LotFrontage=ifelse(is.na(test$LotFrontage), 0, test$LotFrontage)
test$LotShape=ifelse(is.na(test$LotShape), "Reg", test$LotShape)
test$MasVnrType=ifelse(is.na(test$MasVnrType), "None", test$MasVnrType)
test$MasVnrArea=ifelse(is.na(test$MasVnrArea), 0, test$MasVnrArea)
test$MiscFeature=ifelse(is.na(test$MiscFeature), "None", test$MiscFeature)
test$MiscVal=ifelse(is.na(test$MiscVal), 0, test$MiscVal)
test$OpenPorchSF=ifelse(is.na(test$OpenPorchSF), 0, test$OpenPorchSF)
test$PavedDrive=ifelse(is.na(test$PavedDrive), "N", test$PavedDrive)
test$PoolQC=ifelse(is.na(test$PoolQC), "No", test$PoolQC)
test$PoolArea=ifelse(is.na(test$PoolArea), 0, test$PoolArea)
test$SaleCondition=ifelse(is.na(test$SaleCondition), "Normal", test$SaleCondition)
test$ScreenPorch=ifelse(is.na(test$ScreenPorch), 0, test$ScreenPorch)
test$TotRmsAbvGrd=ifelse(is.na(test$TotRmsAbvGrd), 0, test$TotRmsAbvGrd)
test$Utilities=ifelse(is.na(test$Utilities), "AllPub", test$Utilities)
test$WoodDeckSF=ifelse(is.na(test$WoodDeckSF), 0, test$WoodDeckSF)
#test=test[-which(is.na(test$Electrical==TRUE)),]#remove row with missing electrical data

#numerical to categorical 
test$MSSubClass = recode(test$MSSubClass,'20'='Sub20','30'='Sub30','40'='Sub40','45'='Sub45','50'='Sub50','60'='Sub60','70'='Sub70','75'='Sub75','80'='Sub80','85'='Sub85','90'='Sub90','120'='Sub120','150'='Sub150','160'='Sub160','180'='Sub180','190'='Sub190')
test$MoSold = recode(test$MoSold, '1'='Jan','2'='Feb','3'='Mar','4'='Apr','5'='May','6'='Jun','7'='Jul','8'='Aug','9'='Sep','10'='Oct','11'='Nov','12'='Dec')

#categorcial to numerical
test$Alley = recode(test$Alley, 'None'=0,'Grvl'=1,'Pave'=2)
test$BsmtCond = recode(test$BsmtCond, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
test$BsmtExposure = recode(test$BsmtExposure, 'No'=0,'Mn'=1,'Av'=2,'Gd'=3)
test$BsmtFinType1 = recode(test$BsmtFinType1, 'None'=0,'Unf'=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6)
test$BsmtFinType2 = recode(test$BsmtFinType2, 'None'=0,'Unf'=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6)
test$BsmtQual = recode(test$BsmtQual, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
test$ExterCond = recode(test$ExterCond, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
test$ExterQual = recode(test$ExterQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
test$FireplaceQu = recode(test$FireplaceQu, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
test$Functional = recode(test$Functional, 'Sal'=1,'Sev'=2,'Maj2'=3,'Maj1'=4,'Mod'=5,'Min2'=6,'Min1'=7,'Typ'=8)
test$GarageCond = recode(test$GarageCond, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
test$GarageQual = recode(test$GarageQual, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
test$HeatingQC = recode(test$HeatingQC, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
test$KitchenQual = recode(test$KitchenQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
test$LandSlope = recode(test$LandSlope, 'Sev'=1,'Mod'=2,'Gtl'=3)
test$LotShape = recode(test$LotShape, 'IR3'=1,'IR2'=2,'IR1'=3,'Reg'=4)
test$PavedDrive = recode(test$PavedDrive, 'N'=0,'P'=1,'Y'=2)
test$PoolQC = recode(test$PoolQC, 'No'=0,'Fa'=1,'TA'=2,'Gd'=3,'Ex'=4)
test$Street = recode(test$Street, 'Grvl'=1,'Pave'=2)
test$Utilities = recode(test$Utilities, 'Elo'=1,'NoSeWa'=2,'NoSewr'=3,'AllPub'=4)

test$OverallGrade=test$OverallCond*test$OverallQual
test$GarageGrade=test$GarageCond*test$GarageQual
test$ExterGrade=test$ExterCond*test$ExterQual
test$KitchenScore=test$KitchenQual*test$KitchenAbvGr
test$FireplaceScore=test$Fireplaces*test$FireplaceQu
test$GarageScore=test$GarageArea*test$GarageQual
test$PoolScore=test$PoolArea*test$PoolQC
test$TotalBath=test$BsmtFullBath+(.5*test$BsmtHalfBath)+test$FullBath+(.5*test$HalfBath)
test$TotalSF=test$TotalBsmtSF+test$GrLivArea

str(test)
test$MSSubClass=as.factor(test$MSSubClass)
test$MSZoning=as.factor(test$MSZoning)
test$LandContour=as.factor(test$LandContour)
test$LotConfig=as.factor(test$LotConfig)
test$Neighborhood=as.factor(test$Neighborhood)
test$Condition1=as.factor(test$Condition1)
test$Condition2=as.factor(test$Condition2)
test$BldgType=as.factor(test$BldgType)
test$testtyle=as.factor(test$testtyle)
test$RoofStyle=as.factor(test$RoofStyle)
test$RoofMatl=as.factor(test$RoofMatl)
test$Exterior1st=as.factor(test$Exterior1st)
test$Exterior2nd=as.factor(test$Exterior2nd)
test$MasVnrType=as.factor(test$MasVnrType)
test$Foundation=as.factor(test$Foundation)
test$Heating=as.factor(test$Heating)
test$CentralAir=as.factor(test$CentralAir)
test$Electrical=as.factor(test$Electrical)
test$GarageType=as.factor(test$GarageType)
test$GarageFinish=as.factor(test$GarageFinish)
test$Fence=as.factor(test$Fence)
test$MiscFeature=as.factor(test$MiscFeature)
test$MoSold=as.factor(test$MoSold)
test$SaleType=as.factor(test$SaleType)
test$SaleCondition=as.factor(test$SaleCondition)
test$HouseStyle=as.character(test$HouseStyle)

houses$ExterQual=NULL
houses$TotalBsmtSF=NULL
houses$GrLivArea=NULL
houses$FireplaceQu=NULL
houses$GarageCars=NULL
houses$GarageQual=NULL
houses$GarageGrade=NULL
houses$FireplaceScore=NULL
houses$GarageScore=NULL
houses$PoolScore=NULL
houses$TotalSF=NULL

test$ExterQual=NULL
test$TotalBsmtSF=NULL
test$GrLivArea=NULL
test$FireplaceQu=NULL
test$GarageCars=NULL
test$GarageQual=NULL
test$GarageGrade=NULL
test$FireplaceScore=NULL
test$GarageScore=NULL
test$PoolScore=NULL
test$TotalSF=NULL

str(houses)
library(tree)
set.seed(0)
treetrain = sample(1:nrow(houses), 7*nrow(houses)/10) #Training indices.
treehouses.test = houses[-treetrain, ]#Test dataset.
Saleprice.test = treehouses.test$SalePrice[-treetrain]#Test response.

tree.houses = tree(SalePrice ~ ., split = "deviance", data = houses, subset = treetrain)
summary(tree.houses)
plot(tree.houses)
text(tree.houses, pretty = 0)
tree.houses
tree.pred = predict(tree.houses, treehouses.test)
tree.pred
mean((tree.pred - treehouses.test$SalePrice)^2)#MSE 0.04809412 Test
#kagglepredict2=predict(tree.houses, test)
#submission = data.frame(SalePrice=kagglepredict2)
#write.csv(submission,"submit1.csv")

cv.treehouses = cv.tree(tree.houses, FUN = prune.tree)
names(cv.treehouses)

par(mfrow = c(1, 2))
plot(cv.treehouses$size, cv.treehouses$dev, type = "b",
     xlab = "Terminal Nodes", ylab = "RSS")
plot(cv.treehouses$k, cv.treehouses$dev, type  = "b",
     xlab = "Alpha", ylab = "RSS")

prune.houses = prune.tree(tree.houses, best = 10)
par(mfrow = c(1, 1))
plot(prune.houses)
text(prune.houses, pretty = 0)

prune.pred = predict(prune.houses, treehouses.test)
prune.pred
mean((prune.pred - treehouses.test$SalePrice)^2)#MSE 0.04834806

prune.houses4 = prune.tree(tree.houses, best = 4)
par(mfrow = c(1, 1))
plot(prune.houses4)
text(prune.houses4, pretty = 0)

prune.pred4 = predict(prune.houses4, treehouses.test)
prune.pred4
mean((prune.pred4 - treehouses.test$SalePrice)^2)#MSE 0.06104152
