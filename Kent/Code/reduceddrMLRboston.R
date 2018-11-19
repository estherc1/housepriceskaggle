houses=read.csv('./train.csv', stringsAsFactors = FALSE)
test=read.csv('./test.csv', stringsAsFactors = FALSE)
test$SalePrice=10
#houses$system='train'
#test$system='test'
dim(houses)
dim(test)
housestesttrain=rbind(houses,test)

housestesttrain$Id=NULL #delete Id column
housestesttrain$GarageYrBlt=NULL #removing GarageYrBlt
housestesttrain$SalePrice=log(housestesttrain$SalePrice) #taking the log of SalePrice
housestesttrain$Alley=ifelse(is.na(housestesttrain$Alley), 'None', housestesttrain$Alley)
housestesttrain$BedroomAbvGr=ifelse(is.na(housestesttrain$BedroomAbvGr), 0, housestesttrain$BedroomAbvGr)
housestesttrain$BsmtQual=ifelse(is.na(housestesttrain$BsmtQual), 'None', housestesttrain$BsmtQual)
housestesttrain$BsmtCond=ifelse(is.na(housestesttrain$BsmtCond), 'None', housestesttrain$BsmtCond)
housestesttrain$BsmtExposure=ifelse(is.na(housestesttrain$BsmtExposure), 'No', housestesttrain$BsmtExposure)
housestesttrain$BsmtFinType1=ifelse(is.na(housestesttrain$BsmtFinType1), 'None', housestesttrain$BsmtFinType1)
housestesttrain$BsmtFinType2=ifelse(is.na(housestesttrain$BsmtFinType2), 'None', housestesttrain$BsmtFinType2)
housestesttrain$BsmtFullBath=ifelse(is.na(housestesttrain$BsmtFullBath), 0, housestesttrain$BsmtFullBath)
housestesttrain$BsmtHalfBath=ifelse(is.na(housestesttrain$BsmtHalfBath), 0, housestesttrain$BsmtHalfBath)
housestesttrain$BsmtUnfSF=ifelse(is.na(housestesttrain$BsmtUnfSF), 0, housestesttrain$BsmtUnfSF)
housestesttrain$CentralAir=ifelse(is.na(housestesttrain$CentralAir), 'None', housestesttrain$CentralAir)
housestesttrain$Condition1=ifelse(is.na(housestesttrain$Condition1), 'Norm', housestesttrain$Condition1)
housestesttrain$Condition2=ifelse(is.na(housestesttrain$Condition2), 'Norm', housestesttrain$Condition2)
housestesttrain$EnclosedPorch=ifelse(is.na(housestesttrain$EnclosedPorch), 0, housestesttrain$EnclosedPorch)
housestesttrain$ExterCond=ifelse(is.na(housestesttrain$ExterCond), "TA", housestesttrain$ExterCond)
housestesttrain$ExterQual=ifelse(is.na(housestesttrain$ExterQual), "TA", housestesttrain$ExterQual)
housestesttrain$Fence=ifelse(is.na(housestesttrain$Fence), "None", housestesttrain$Fence)
housestesttrain$FireplaceQu=ifelse(is.na(housestesttrain$FireplaceQu), "None", housestesttrain$FireplaceQu)
housestesttrain$Fireplaces=ifelse(is.na(housestesttrain$Fireplaces), 0, housestesttrain$Fireplaces)
housestesttrain$Functional=ifelse(is.na(housestesttrain$Functional), "Typ", housestesttrain$Functional)
housestesttrain$GarageType=ifelse(is.na(housestesttrain$GarageType), "None", housestesttrain$GarageType)
housestesttrain$GarageFinish=ifelse(is.na(housestesttrain$GarageFinish), "None", housestesttrain$GarageFinish)
housestesttrain$GarageQual=ifelse(is.na(housestesttrain$GarageQual), "None", housestesttrain$GarageQual)
housestesttrain$GarageCond=ifelse(is.na(housestesttrain$GarageCond), "None", housestesttrain$GarageCond)
housestesttrain$GarageArea=ifelse(is.na(housestesttrain$GarageArea), 0, housestesttrain$GarageArea)
housestesttrain$GarageCars=ifelse(is.na(housestesttrain$GarageCars), 0, housestesttrain$GarageCars)
housestesttrain$HalfBath=ifelse(is.na(housestesttrain$HalfBath), 0, housestesttrain$HalfBath)
housestesttrain$HeatingQC=ifelse(is.na(housestesttrain$HeatingQC), "TA", housestesttrain$HeatingQC)
housestesttrain$KitchenAbvGr=ifelse(is.na(housestesttrain$KitchenAbvGr), 0, housestesttrain$KitchenAbvGr)
housestesttrain$KitchenQual=ifelse(is.na(housestesttrain$KitchenQual), "TA", housestesttrain$KitchenQual)
housestesttrain$LotFrontage=ifelse(is.na(housestesttrain$LotFrontage), 0, housestesttrain$LotFrontage)
housestesttrain$LotShape=ifelse(is.na(housestesttrain$LotShape), "Reg", housestesttrain$LotShape)
housestesttrain$MasVnrType=ifelse(is.na(housestesttrain$MasVnrType), "None", housestesttrain$MasVnrType)
housestesttrain$MasVnrArea=ifelse(is.na(housestesttrain$MasVnrArea), 0, housestesttrain$MasVnrArea)
housestesttrain$MiscFeature=ifelse(is.na(housestesttrain$MiscFeature), "None", housestesttrain$MiscFeature)
housestesttrain$MiscVal=ifelse(is.na(housestesttrain$MiscVal), 0, housestesttrain$MiscVal)
housestesttrain$OpenPorchSF=ifelse(is.na(housestesttrain$OpenPorchSF), 0, housestesttrain$OpenPorchSF)
housestesttrain$PavedDrive=ifelse(is.na(housestesttrain$PavedDrive), "N", housestesttrain$PavedDrive)
housestesttrain$PoolQC=ifelse(is.na(housestesttrain$PoolQC), "No", housestesttrain$PoolQC)
housestesttrain$PoolArea=ifelse(is.na(housestesttrain$PoolArea), 0, housestesttrain$PoolArea)
housestesttrain$SaleCondition=ifelse(is.na(housestesttrain$SaleCondition), "Normal", housestesttrain$SaleCondition)
housestesttrain$ScreenPorch=ifelse(is.na(housestesttrain$ScreenPorch), 0, housestesttrain$ScreenPorch)
housestesttrain$TotRmsAbvGrd=ifelse(is.na(housestesttrain$TotRmsAbvGrd), 0, housestesttrain$TotRmsAbvGrd)
housestesttrain$Utilities=ifelse(is.na(housestesttrain$Utilities), "AllPub", housestesttrain$Utilities)
housestesttrain$WoodDeckSF=ifelse(is.na(housestesttrain$WoodDeckSF), 0, housestesttrain$WoodDeckSF)
housestesttrain$BsmtFinSF1=ifelse(is.na(housestesttrain$BsmtFinSF1), 0, housestesttrain$BsmtFinSF1)
housestesttrain$BsmtFinSF2=ifelse(is.na(housestesttrain$BsmtFinSF2), 0, housestesttrain$BsmtFinSF2)
housestesttrain$TotalBsmtSF=ifelse(is.na(housestesttrain$TotalBsmtSF), 0, housestesttrain$TotalBsmtSF)

housestesttrain=housestesttrain[-which(is.na(housestesttrain$Electrical==TRUE)),]#remove row with missing electrical data

#numerical to categorical

housestesttrain$MSSubClass = recode(housestesttrain$MSSubClass,'20'='Sub20','30'='Sub30','40'='Sub40','45'='Sub45','50'='Sub50','60'='Sub60','70'='Sub70','75'='Sub75','80'='Sub80','85'='Sub85','90'='Sub90','120'='Sub120','150'='Sub150','160'='Sub160','180'='Sub180','190'='Sub190')
housestesttrain$MoSold = recode(housestesttrain$MoSold, '1'='Jan','2'='Feb','3'='Mar','4'='Apr','5'='May','6'='Jun','7'='Jul','8'='Aug','9'='Sep','10'='Oct','11'='Nov','12'='Dec')

#categorcial to numerical
housestesttrain$Alley = recode(housestesttrain$Alley, 'None'=0,'Grvl'=1,'Pave'=2)
housestesttrain$BsmtCond = recode(housestesttrain$BsmtCond, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
housestesttrain$BsmtExposure = recode(housestesttrain$BsmtExposure, 'No'=0,'Mn'=1,'Av'=2,'Gd'=3)
housestesttrain$BsmtFinType1 = recode(housestesttrain$BsmtFinType1, 'None'=0,'Unf'=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6)
housestesttrain$BsmtFinType2 = recode(housestesttrain$BsmtFinType2, 'None'=0,'Unf'=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6)
housestesttrain$BsmtQual = recode(housestesttrain$BsmtQual, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
housestesttrain$ExterCond = recode(housestesttrain$ExterCond, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
housestesttrain$ExterQual = recode(housestesttrain$ExterQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
housestesttrain$FireplaceQu = recode(housestesttrain$FireplaceQu, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
housestesttrain$Functional = recode(housestesttrain$Functional, 'Sal'=1,'Sev'=2,'Maj2'=3,'Maj1'=4,'Mod'=5,'Min2'=6,'Min1'=7,'Typ'=8)
housestesttrain$GarageCond = recode(housestesttrain$GarageCond, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
housestesttrain$GarageQual = recode(housestesttrain$GarageQual, 'None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
housestesttrain$HeatingQC = recode(housestesttrain$HeatingQC, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
housestesttrain$KitchenQual = recode(housestesttrain$KitchenQual, 'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)
housestesttrain$LandSlope = recode(housestesttrain$LandSlope, 'Sev'=1,'Mod'=2,'Gtl'=3)
housestesttrain$LotShape = recode(housestesttrain$LotShape, 'IR3'=1,'IR2'=2,'IR1'=3,'Reg'=4)
housestesttrain$PavedDrive = recode(housestesttrain$PavedDrive, 'N'=0,'P'=1,'Y'=2)
housestesttrain$PoolQC = recode(housestesttrain$PoolQC, 'No'=0,'Fa'=1,'TA'=2,'Gd'=3,'Ex'=4)
housestesttrain$Street = recode(housestesttrain$Street, 'Grvl'=1,'Pave'=2)
housestesttrain$Utilities = recode(housestesttrain$Utilities, 'Elo'=1,'NoSeWa'=2,'NoSewr'=3,'AllPub'=4)

housestesttrain$OverallGrade=housestesttrain$OverallCond*housestesttrain$OverallQual
housestesttrain$GarageGrade=housestesttrain$GarageCond*housestesttrain$GarageQual
housestesttrain$ExterGrade=housestesttrain$ExterCond*housestesttrain$ExterQual
housestesttrain$KitchenScore=housestesttrain$KitchenQual*housestesttrain$KitchenAbvGr
housestesttrain$FireplaceScore=housestesttrain$Fireplaces*housestesttrain$FireplaceQu
housestesttrain$GarageScore=housestesttrain$GarageArea*housestesttrain$GarageQual
housestesttrain$PoolScore=housestesttrain$PoolArea*housestesttrain$PoolQC
housestesttrain$TotalBath=housestesttrain$BsmtFullBath+(.5*housestesttrain$BsmtHalfBath)+housestesttrain$FullBath+(.5*housestesttrain$HalfBath)
housestesttrain$TotalSF=housestesttrain$TotalBsmtSF+housestesttrain$GrLivArea

housestesttrain$SaleType=Hmisc::impute(housestesttrain$SaleType, "random")
housestesttrain$Exterior1st=Hmisc::impute(housestesttrain$Exterior1st, "random")
housestesttrain$Exterior2nd=Hmisc::impute(housestesttrain$Exterior2nd, "random")
housestesttrain$MSZoning=Hmisc::impute(housestesttrain$MSZoning, "random")

housestesttrain$ExterQual=NULL
housestesttrain$TotalBsmtSF=NULL
housestesttrain$GrLivArea=NULL
housestesttrain$FireplaceQu=NULL
housestesttrain$GarageCars=NULL
housestesttrain$GarageQual=NULL
housestesttrain$GarageGrade=NULL
housestesttrain$FireplaceScore=NULL
housestesttrain$GarageScore=NULL
housestesttrain$PoolScore=NULL
housestesttrain$TotalSF=NULL



nacount=sapply(housestesttrain, function(y) sum(length(which(is.na(y)))))
housestesttrain[is.na(housestesttrain$TotalSF),]

library(glmnet)

grid = 10^seq(5, -2, length = 100) #creating Lambda grid

x=model.matrix(SalePrice~.-housestesttrain$SalePrice,data = housestesttrain)[, -1] 
y=housestesttrain$SalePrice
table(y)

apply(x[1:1460, ], 2, function(x) length(unique(x)))

x=x[,-1]

#####################################################################################################################


xtrain=x[1:1460,]
xtest=x[1460:2918,]
TotalY=housestesttrain$SalePrice[1:1460]

set.seed(0)
train = sample(1:nrow(xtrain), nrow(xtrain)*0.8)
test = (-train)
y.test = TotalY[test]
y.train=TotalY[train]

length(train)/nrow(xtrain)
length(y.test)/nrow(xtrain)

ridge.models = glmnet(xtrain[train, ], TotalY[train], alpha = 0, lambda = grid)#100 ridge models
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")
cv.ridge.out = cv.glmnet(xtrain[train, ], TotalY[train], alpha = 0, nfolds = 10, lambda = grid)
plot(cv.ridge.out, main = "Ridge Regression\n")
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge  #0.2205131
log(bestlambda.ridge)

ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = xtrain[train, ])
mean((ridge.bestlambdatrain - y.train)^2)#MSE 0.01404853 TRAINING
ridge.bestlambdatest = predict(ridge.models, s = bestlambda.ridge, newx = xtrain[test, ])
mean((ridge.bestlambdatest - y.test)^2)#MSE 0.3252751 Test
ridge.best_refit = glmnet(xtrain, TotalY, alpha = 0, lambda = bestlambda.ridge)#refitting on all training rows
ridge.bestlambda = predict(ridge.best_refit, s = bestlambda.ridge, newx = xtrain)
mean((ridge.bestlambda - TotalY)^2)#MSE 0.06917051
cv.ridgeoutprecict = predict(cv.ridge.out, s = bestlambda.ridge, newx = xtrain)
mean((cv.ridgeoutprecict - TotalY)^2)#MSE 0.07629385

lasso.models = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)#100 lasso models
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")
cv.lasso.out = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.ridge.out$lambda.min
bestlambda.lasso#0.2205131
log(bestlambda.lasso)

lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = xtrain[train, ])
mean((lasso.bestlambdatrain - y.train)^2)#0.09968051 TRAIN
lasso.bestlambdatest = predict(lasso.models, s = bestlambda.lasso, newx = xtrain[test, ])
mean((lasso.bestlambdatest - y.test)^2)#0.4084084 TEST
lasso.best_refit = glmnet(xtrain, TotalY, alpha = 1, lambda = bestlambda.lasso)#refitting on all training rows
lasso.bestlambda = predict(lasso.best_refit, s = bestlambda.lasso, newx = xtrain)
mean((lasso.bestlambda - TotalY)^2)#MSE 0.1627989
cv.lassooutprecict = predict(cv.lasso.out, s = bestlambda.lasso, newx = xtrain)
mean((cv.lassooutprecict - TotalY)^2)#MSE 0.1614261

predict(ridge.best_refit, type = "coefficients", s = bestlambda.ridge)
predict(lasso.best_refit, type = "coefficients", s = bestlambda.lasso)

##TEST FOR KAGGLE
kagglepredict = predict(ridge.best_refit, s = bestlambda.ridge, newx = xtest)
submission = data.frame(SalePrice=kagglepredict)
write.csv(submission,"submit1.csv")

colnames(housestesttrain)


