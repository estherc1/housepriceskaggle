##################################
#####Bagging & Random Forests#####
##################################
library(randomForest)

#Fitting an initial random forest to the training subset.
set.seed(0)
rf.house.c = randomForest(SalePrice ~ ., house.c.modeldata, subset = train, importance = TRUE)
rf.house.c

#The MSE and percent variance explained are based on out-of-bag estimates,
#yielding unbiased error estimates. The model reports that mtry = 4, which is
#the number of variables randomly chosen at each split. Since we have 13 overall
#variables, we could try all 13 possible values of mtry. We will do so, record
#the results, and make a plot.

#Varying the number of variables used at each step of the random forest procedure.
set.seed(0)
oob.err = numeric(13)
for (mtry in 1:13) {
  fit = randomForest(SalePrice ~ ., data = house.c.modeldata[train, ], mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}

#Visualizing the OOB error.
plot(1:13, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#Can visualize a variable importance plot.
importance(rf.house.c)
varImpPlot(rf.house.c)



##################
#####Boosting#####
##################
library(gbm)

#Fitting 10,000 trees with a depth of 4.
set.seed(0)
boost.house.c = gbm(SalePrice ~ ., data = house.c.modeldata[train, ],
                   distribution = "gaussian",
                   n.trees = 10000,
                   interaction.depth = 10)

#Inspecting the relative influence.
par(mfrow = c(1, 1))
summary(boost.house.c)

#Let’s make a prediction on the test set. With boosting, the number of trees is
#a tuning parameter; having too many can cause overfitting. In general, we should
#use cross validation to select the number of trees. Instead, we will compute the
#test error as a function of the number of trees and make a plot for illustrative
#purposes.
n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.house.c, newdata = house.c.modeldata[-train, ], n.trees = n.trees)

#Produces 100 different predictions for each of the 152 observations in our
#test set.
dim(predmat)

#Calculating the boosted errors.
par(mfrow = c(1, 1))
berr = with(house.c.modeldata[1:438, ], apply((predmat[1:438,] - SalePrice)^2, 2, mean))
plot(n.trees, berr, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

#Include the best OOB error from the random forest.
abline(h = min(oob.err), col = "red")

prediction4 = predmat[439:length(house.c.test),which(berr==min(berr))]
submission4 = data.frame(Id=house.test$Id, SalePrice=prediction4)
write.csv(submission4,"submit4.csv", row.names = FALSE)

#Increasing the shrinkage parameter; a higher proportion of the errors are
#carried over.
set.seed(0)
boost.house.c2 = gbm(SalePrice ~ ., data = house.c.modeldata[train, ],
                    distribution = "gaussian",
                    n.trees = 10000,
                    interaction.depth = 4,
                    shrinkage = 0.001)
predmat2 = predict(boost.house.c2, newdata = house.c.modeldata[-train, ], n.trees = n.trees)

berr2 = with(house.c.modeldata[1:438,], apply((predmat2[1:438,] - SalePrice)^2, 2, mean))
plot(n.trees, berr2, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

min(berr)
min(berr2)

prediction5 = predmat[439:length(house.c.test),which(berr2==min(berr2))]
submission5 = data.frame(Id=house.test$Id, SalePrice=prediction5)
write.csv(submission5,"submit5.csv", row.names = FALSE)
