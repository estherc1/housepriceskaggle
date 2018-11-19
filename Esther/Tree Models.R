##########################
#####Regression Trees#####
##########################
#Inspecting the housing values in the suburbs of Boston.
library(MASS)
 

#Creating a training set on 70% of the data.
set.seed(0)

#Training the tree to predict the median value of owner-occupied homes (in $1k).
tree.house.c = tree(log(SalePrice) ~ ., cormodel.fullhouse.m.or, subset = train)
summary(tree.house.c)

#Visually inspecting the regression tree.
plot(tree.house.c)
text(tree.house.c, pretty = 0)

#Performing cross-validation.
set.seed(0)
cv.house.c = cv.tree(tree.house.c)
par(mfrow = c(1, 2))
plot(cv.house.c$size, cv.house.c$dev, type = "b",
     xlab = "Terminal Nodes", ylab = "RSS")
plot(cv.house.c$k, cv.house.c$dev, type  = "b",
     xlab = "Alpha", ylab = "RSS")

#Pruning the tree to have 4 terminal nodes.
prune.house.c = prune.tree(tree.house.c, best = 6)
par(mfrow = c(1, 1))
plot(prune.house.c)
text(prune.house.c, pretty = 0)

#Calculating and assessing the MSE of the test data on the overall tree.
y = log(cormodel.fullhouse.m.or$SalePrice)[-train]
yhat= predict(tree.house.c, newdata = cormodel.fullhouse.m.or[-train, ])
plot(yhat[1:434], y[1:434])
abline(0, 1)
mean((yhat[1:434] - y[1:434])^2, na.rm = TRUE)

prediction2 = exp(yhat[435:length(house.c.test)])
submission2 = data.frame(Id=house.test$Id, SalePrice=prediction2)
write.csv(submission2,"submit2.csv",row.names = FALSE)

#Calculating and assessing the MSE of the test data on the pruned tree.
yhat = predict(prune.house.c, newdata = cormodel.fullhouse.m.or[-train, ])
plot(yhat[1:434], y[1:434])
abline(0, 1)
mean((yhat[1:434] - y[1:434])^2, na.rm = TRUE)

prediction3 = exp(yhat[435:length(house.c.test)])
submission3 = data.frame(Id=house.test$Id, SalePrice=prediction2)
write.csv(submission3,"submit3.csv",row.names = FALSE)
