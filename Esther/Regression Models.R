################################ Modeling for ESTHER & CATEGORICAL #############################
library(MASS)
house.c.modeldata <- house.c.train[,-c(which(colnames(house.c) %in% not.include),89)]
house.n.modeldata <- house.n.train[,-c(which(colnames(house.c) %in% not.include),89)]
house.m.modeldata <- house.m.train[,-c(which(colnames(house.c) %in% not.include),89)]
house.m.modeldata.or <- house.m.train.or[,-c(which(colnames(house.c) %in% not.include),89)]

fullmodel.c <- lm(log(SalePrice)~.,data = house.c.modeldata)
fullmodel.n <- lm(log(SalePrice)~.,data = house.n.modeldata)
fullmodel.m <- lm(log(SalePrice)~.,data = house.m.modeldata)
fullmodel.m.or <- lm(log(SalePrice)~.,data = house.m.modeldata.or)

backwardstep.c = stepAIC(fullmodel.c, direction = "backward")
backwardstep.n = stepAIC(fullmodel.n, direction = "backward")
backwardstep.m = stepAIC(fullmodel.m, direction = "backward")
backwardstep.m.or = stepAIC(fullmodel.m.or, direction = "backward")

AIC(backwardstep.c)  #-2070.98
AIC(backwardstep.n)  #-2070.98
AIC(backwardstep.m)  #-2070.98

step.c.exp.var <- names(backwardstep.c$xlevels)
step.n.exp.var <- names(backwardstep.n$xlevels)
step.m.exp.var <- names(backwardstep.m$xlevels)

stepmodel.fullhouse.c <- fullhouse.c[,c(names(backwardstep.c$xlevels),"SalePrice")]
stepmodel.fullhouse.n <- houses.n[,c(names(backwardstep.n$xlevels),"SalePrice")]
stepmodel.fullhouse.m <- fullhouse.m[,c(names(backwardstep.m$xlevels),"SalePrice")]

#########################CROSS VALIDATION###################################
grid = 10^seq(5, -2, length = 1000)  
x=model.matrix(log(SalePrice)~.-1,data = stepmodel.fullhouse.m.or)[1:1444,]
y=log(stepmodel.fullhouse.m.or$SalePrice)[1:1444]
train = sample(1:nrow(x), 7*nrow(x)/10)
test = (-train)
y.train = y[train]
y.test = y[test]
cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)
ridge.models.train = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
ridge.bestlambdatrain.t = predict(ridge.models.train, s = bestlambda.ridge, newx = x[train, ])
mean((ridge.bestlambdatrain.t - y[train])^2) #0.04097878
ridge.bestlambdatrain = predict(ridge.models.train, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2) #0.04489113
plot(ridge.bestlambdatrain,y.test,xlab="Prediction",ylab="Log Sale Price", main="Ridge Regression Fit")
abline(0,1,col=2,lty=2)

cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)
lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
lasso.bestlambdatrain.t = predict(lasso.models.train, s = bestlambda.lasso, newx = x[train, ])
mean((lasso.bestlambdatrain.t - y[train])^2) #0.04849104
lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2) #0.04546142

cv.el.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0.25, nfolds = 10)
bestlambda.el = cv.el.out$lambda.min
bestlambda.el
log(bestlambda.el)
el.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
el.bestlambdatrain.t = predict(el.models.train, s = bestlambda.el, newx = x[train, ])
mean((el.bestlambdatrain.t - y[train])^2) #0.04849104
el.bestlambdatrain = predict(el.models.train, s = bestlambda.el, newx = x[test, ])
mean((el.bestlambdatrain - y.test)^2) #0.04546142

###################################Multiple Models#########################################
library(glmnet)

choosefeatures <- function(variables, nfeatures){
  x <- ncol(data) -1
  features <- sample(combn(x,nfeatures), 1)
  return(features)
}

xdata <- function(data, features){
  subsetdata = data[,features]
  x=model.matrix(log(SalePrice)~.-1, data=subsetdata)
  return(x)
}



performcv <- function(data, features){
  x = xdata(data, features)
  y = log(data$SalePrice)
  train = sample(1:nrow(x), 7*nrow(x)/10)
  test = (-train)
  grid = 10^seq(5, -2, length = 1000)
  
  cv.ridge.out = cv.glmnet(x[train, ], y[train],lambda = grid, alpha = 0, nfolds = 10)
  cv.lasso.out = cv.glmnet(x[train, ], y[train],lambda = grid, alpha = 1, nfolds = 10)
  bestlambda.ridge = cv.ridge.out$lambda.min
  bestlambda.lasso = cv.lasso.out$lambda.min
  
  ridge.models.train.t = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
  ridge.bestlambdatrain.t = predict(ridge.models.train, s = bestlambda.ridge, newx = x[train, ])
  ridge.train.mse <- mean((ridge.bestlambdatrain.t - y[train])^2) 
  ridge.bestlambdatrain = predict(ridge.models.train, s = bestlambda.ridge, newx = x[test, ])
  ridge.test.mse <- mean((ridge.bestlambdatrain - y.test)^2) 
  
  lasso.models.train.t = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
  lasso.bestlambdatrain.t = predict(lasso.models.train, s = bestlambda.lasso, newx = x[train, ])
  lasso.train.mse <- mean((lasso.bestlambdatrain.t - y[train])^2) 
  lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lasso, newx = x[test, ])
  lasso.test.mse <- mean((lasso.bestlambdatrain - y.test)^2) 
  
  return(list("RidgeTrainError" = ridge.train.mse, "RidgeTestError" = ridge.test.mse,
              "LassoTrainError" = lasso.train.mse, "LassoTestError" = lasso.test.mse))
}


########################################################################################




##predict test:
x.test=model.matrix(log(SalePrice)~.-1,data = stepmodel.fullhouse.m.or)[1444:2902,]

prediction1 = exp(predict(ridge.models.train, s = bestlambda.ridge, newx = x.test))
submission = data.frame(Id=house.test$Id, SalePrice=prediction1)
write.csv(submission,"submit1.csv", row.names = FALSE)

prediction1.1 = exp(predict(lasso.models.train, s = bestlambda.lasso, newx = x.test))
submission1.1 = data.frame(Id=house.test$Id, SalePrice=prediction1.1)
write.csv(submission1.1,"submit11.csv", row.names = FALSE)





################
grid = 10^seq(5, -2, length = 1000)  
x=model.matrix(log(SalePrice)~.-1,data = cormodel.fullhouse.m.or)[1:1444,]
y=log(cormodel.fullhouse.m.or$SalePrice)[1:1444]
train = sample(1:nrow(x), 7*nrow(x)/10)
test = (-train)
y.train = y[train]
y.test = y[test]
cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)
ridge.models.train = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
ridge.bestlambdatrain.t = predict(ridge.models.train, s = bestlambda.ridge, newx = x[train, ])
mean((ridge.bestlambdatrain.t - y[train])^2) #0.04097878
ridge.bestlambdatrain = predict(ridge.models.train, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2) #0.04489113

cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)
lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
lasso.bestlambdatrain.t = predict(lasso.models.train, s = bestlambda.lasso, newx = x[train, ])
mean((lasso.bestlambdatrain.t - y[train])^2) #0.04849104
lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2) #0.04546142

x.test=model.matrix(log(SalePrice)~.-1,data = cormodel.fullhouse.m.or)[1365:2823,]

prediction12 = exp(predict(ridge.models.train, s = bestlambda.ridge, newx = x.test))
submission = data.frame(Id=house.test$Id, SalePrice=prediction12)
write.csv(submission,"submit12.csv", row.names = FALSE)

prediction12.1 = exp(predict(lasso.models.train, s = bestlambda.lasso, newx = x.test))
submission12.1 = data.frame(Id=house.test$Id, SalePrice=prediction12.1)
write.csv(submission12.1,"submit121.csv", row.names = FALSE)

################
grid = 10^seq(5, -2, length = 1000)  
x=model.matrix(log(SalePrice)~.-1,data = npmodel.fullhouse.m.or)[1:1444,]
y=log(npmodel.fullhouse.m.or$SalePrice)[1:1444]
train = sample(1:nrow(x), 7*nrow(x)/10)
test = (-train)
y.train = y[train]
y.test = y[test]
cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)
ridge.models.train = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
ridge.bestlambdatrain.t = predict(ridge.models.train, s = bestlambda.ridge, newx = x[train, ])
mean((ridge.bestlambdatrain.t - y[train])^2) #0.04097878
ridge.bestlambdatrain = predict(ridge.models.train, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2) #0.04489113

cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)
lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
lasso.bestlambdatrain.t = predict(lasso.models.train, s = bestlambda.lasso, newx = x[train, ])
mean((lasso.bestlambdatrain.t - y[train])^2) #0.04849104
lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2) #0.04546142

x.test=model.matrix(log(SalePrice)~.-1,data = npmodel.fullhouse.m.or)[1365:2172,]

prediction12 = exp(predict(ridge.models.train, s = bestlambda.ridge, newx = x.test))
submission = data.frame(Id=house.test$Id, SalePrice=prediction12)
write.csv(submission,"submit12.csv", row.names = FALSE)

prediction12.1 = exp(predict(lasso.models.train, s = bestlambda.lasso, newx = x.test))
submission12.1 = data.frame(Id=house.test$Id, SalePrice=prediction12.1)
write.csv(submission12.1,"submit121.csv", row.names = FALSE)


