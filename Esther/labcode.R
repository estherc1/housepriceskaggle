rm(list=ls())
setwd("/Users/hee-wonchang/Downloads/all/housepriceskaggle/Esther")
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(caret)
library(glmnet)

orders <- read.csv("Orders.csv")
returns <- read.csv("Returns.csv")

#PartI Problem1
class(orders$Profit)
orders$Profit <- as.numeric(orders$Profit)
orders$Sales <- as.numeric(orders$Sales)
summary(orders)
cor(orders[,19:23])

#PartI Problem 2: Inventory Management
orders$Order.Date <- as.Date(orders$Order.Date, "%m/%d/%y")
orders$Ship.Date <- as.Date(orders$Ship.Date, "%m/%d/%y")
orders$Order.YM <- format(as.Date(orders$Order.Date, "%Y-%m-%d"), "%Y%m")

inventory <- orders %>% group_by(YM = Order.YM) %>% summarise(Total = sum(Quantity), 
                                            Furniture = sum(Quantity[Category=="Furniture"]),
                                            OfficeSupplies = sum(Quantity[Category=="Office Supplies"]),
                                            Technology = sum(Quantity[Category=="Technology"])) %>%
                                            arrange(YM)

trend = inventory %>% gather(key, value, Total, Furniture, OfficeSupplies, Technology) %>%
  ggplot(aes(x=YM, y=value, colour=key, group=key)) + geom_point() + geom_line() +
  xlab("Month & Year") + ylab("Inventories") + 
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Inventory Trends") 
ggplotly(trend)

#1. Is there any seasonal trend of inventory in the company? 
### Yes - end of quarter peaks usually and climax being the Q4 and especially November and December
#2. Is the seasonal trend the same for different categories?
### Mostly same for Technology and Furniture. More fluctuation with Office Supplies

#PartI Problem 3: Inventory Management
orders$returned <- as.factor(ifelse(orders$Order.ID %in% returns$Order.ID, "Yes", "No"))

##1 How much profit did we lose due to returns each year?
orders %>% group_by(returned) %>% summarise(Total.Profit = sum(Profit))
  
##2 How many customer returned more than once? more than 5 times?
orders %>% filter(returned=="Yes") %>% group_by(Customer.ID) %>% summarise(count = n()) %>% 
  arrange(desc(count)) %>% filter(count > 1)

orders %>% filter(returned=="Yes") %>% group_by(Customer.ID) %>% summarise(count = n()) %>% 
  arrange(desc(count)) %>% filter(count > 5)

##3 Which regions are more likely to return orders?
orders %>% filter(returned=="Yes") %>% group_by(Region) %>% 
  summarise(count = n(), TotalProfit = sum(Profit), ratio = TotalProfit/count ) %>% 
  arrange(desc(count)) 
  
##4 Which categories (sub-categories) of products are more likely to be returned?
orders %>% filter(returned=="Yes") %>% group_by(Category, Sub.Category) %>% 
  summarise(count = n(), TotalProfit = sum(Profit), ratio = TotalProfit/count ) %>% 
  arrange(desc(count,ratio))   


#PartII Step 1
class(orders$returned)

#PartII Step 2
orders <- orders %>% mutate(Process.Time = as.numeric(Ship.Date - Order.Date))

#PartII Step 3
returned.products <- orders %>% filter(returned=="Yes") %>% group_by(Product.ID) %>% 
                    summarise(count = n() )
orders$timesreturned <- ifelse(orders$returned=="Yes" & orders$Product.ID %in% returned.products$Product.ID, 
                               returned.products$count, 0)

#Problem 5
model.data <- orders[,c('Region', 'Category','Sales','Quantity','Discount',
                        'Profit','Shipping.Cost','Process.Time','timesreturned','returned')]

logit.full <- glm(returned ~ . ,family = "binomial", data = model.data)
summary(logit.full)
step(logit.full, direction = "backward")

logit.sub1 <- glm(returned ~ Sales + Discount + Profit + Process.Time + timesreturned,family = "binomial", data = model.data)
summary(logit.sub1)

reduced.deviance = logit.sub1$deviance #Comparing the deviance of the reduced
reduced.df = logit.sub1$df.residual    #model (the one without rank) to...

full.deviance = logit.full$deviance #...the deviance of the full model (the
full.df = logit.full$df.residual    #one with the rank terms).

pchisq(reduced.deviance - full.deviance,
       reduced.df - full.df,
       lower.tail = FALSE)

logit.sub2 <- glm(returned ~ timesreturned,family = "binomial", data = model.data)
summary(logit.sub2)

reduced2.deviance = logit.sub2$deviance #Comparing the deviance of the reduced
reduced2.df = logit.sub2$df.residual    #model (the one without rank) to...

pchisq(reduced2.deviance - reduced.deviance,
       reduced2.df - reduced.df,
       lower.tail = FALSE)

X <- model.data[,c('Sales','Discount', 'Profit', 'Process.Time', 'timesreturned')]
Y <- model.data[,'returned']

set.seed(0)
grid = 10^seq(5, -2, length = 100)
train = sample(1:nrow(X), 8*nrow(X)/10)
test = (-train)
Y.test = Y[test]

train_control = trainControl(method = 'cv', number=10)
tune.grid = expand.grid(lambda = grid, alpha=c(0))
ridge.caret = train(X[train, ], Y[train],
                    method = 'glmnet',
                    trControl = train_control, tuneGrid = tune.grid)

### Plot the tuning object:
plot(ridge.caret, xTrans=log)

### We see caret::train returns a different result from the
### one by cv.glmnet. By comparing the ridge.caret$results
### and cv.ridge.out$cvm, it's most likely to be rounding and 
### averaging.

### Predicting with the final model
pred = predict.train(ridge.caret, newdata = X[test,])
mean((pred - y[test])^2)

### Note: there is a "finalModel in ridge.caret. But unfortunately, using it
###       for predicting often results in error.There is some issue with the 
###       compactibility of the "predict" function and the model from caret.train
predict(ridge.caret$finalModel, newdata = x[test,])


##########################
#####Lasso Regression#####
##########################
#Fitting the lasso regression. Alpha = 1 for lasso regression.
lasso.models = glmnet(x, y, alpha = 1, lambda = grid)

dim(coef(lasso.models)) #20 different coefficients, estimated 100 times --
#once each per lambda value.
coef(lasso.models) #Inspecting the various coefficient estimates.

#What do the estimates look like for a smaller value of lambda?
lasso.models$lambda[80] #Lambda = 0.2595.
coef(lasso.models)[, 80] #Most estimates not close to 0.
sum(abs(coef(lasso.models)[-1, 80])) #L1 norm is 228.1008.

#What do the estimates look like for a larger value of lambda?
lasso.models$lambda[15] #Lambda = 10,235.31.
coef(lasso.models)[, 15] #Estimates all 0.
sum(abs(coef(lasso.models)[-1, 15])) #L1 norm is essentially 0.

#Visualizing the lasso regression shrinkage.
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

#Can use the predict() function to obtain lasso regression coefficients for a
#new value of lambda, not necessarily one that was within our grid:
predict(lasso.models, s = 50, type = "coefficients")

#Let's attempt to fit a lasso regression using some arbitrary value of lambda;
#we still have not yet figured out what the best value of lambda should be!
#We will arbitrarily choose 5. We will now use the training set exclusively.
lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
lasso.lambda5 = predict(lasso.models.train, s = 5, newx = x[test, ])
mean((lasso.lambda5 - y.test)^2)

#Here, the MSE is approximately 107,660.

#Instead of arbitrarily choosing random lambda values and calculating the MSE
#manually, it's a better idea to perform cross-validation in order to choose
#the best lambda over a slew of values.

#Running 10-fold cross validation.
set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

#What is the test MSE associated with this best value of lambda?
lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)



data(oil)
createDataPartition(Y, 10)

x <- rgamma(50, 3, .5)
inA <- createDataPartition(Y, list = FALSE)

plot(density(x[inA]))
rug(x[inA])

points(density(x[-inA]), type = "l", col = 4)
rug(x[-inA], col = 4)

createResample(oilType, 2)

createFolds(oilType, 10)
createFolds(oilType, 5, FALSE)

createFolds(rnorm(21))

createTimeSlices(1:9, 5, 1, fixedWindow = FALSE)
createTimeSlices(1:9, 5, 1, fixedWindow = TRUE)
createTimeSlices(1:9, 5, 3, fixedWindow = TRUE)
createTimeSlices(1:9, 5, 3, fixedWindow = FALSE)

createTimeSlices(1:15, 5, 3)
createTimeSlices(1:15, 5, 3, skip = 2)
createTimeSlices(1:15, 5, 3, skip = 3)

set.seed(131)
groups <- sort(sample(letters[1:4], size = 20, replace = TRUE))
table(groups)
folds <- groupKFold(groups)
lapply(folds, function(x, y) table(y[x]), y = groups)
# }





