###################################################################
## B6. Tree Based Methods
###################################################################
## Libraries
library(tidyverse)
library(ISLR2)
library(MASS)
library(tree) # 
library(rpart) #
library(party) # 
library(BART) #
library(randomForest) #
library(gbm) #


## get the data
carseats_data <- Carseats %>% 
  as_tibble()

colnames(carseats_data) <- colnames(carseats_data) %>% tolower()

high <- factor(ifelse(carseats_data$sales <= 8 ,"No" ,"Yes"))

tree_carseats <- tree(high ~ . - sales ,data = carseats_data)

summary(tree_carseats)

plot(tree_carseats)

text(tree_carseats ,pretty = 0)

tree_carseats

set.seed(2)

train <- sample(1:nrow(carseats_data) ,200)

carseats_data_test <- carseats_data[-train ,]

high_test <- high[-train]

tree_carseats <- tree(high ~ . - sales 
                      ,data = carseats_data
                      ,subset = train
                      )

tree_pred <- predict(tree_carseats ,newdata = carseats_data_test ,type = "class")

table(tree_pred ,high_test)

(86 + 57) / 200

set.seed(7)

cv_carseats <- cv.tree(tree_carseats ,FUN = prune.misclass)

names(cv_carseats)

par(mfrow = c(1 ,2))

plot(cv_carseats$size ,cv_carseats$dev ,type = "b")

plot(cv_carseats$k ,cv_carseats$dev ,type = "b")

prune_carseats <- prune.misclass(tree_carseats ,best = 9)

plot(prune_carseats)

text(prune_carseats ,pretty = 0)

tree_pred <- predict(prune_carseats ,newdata = carseats_data_test ,type = "class")

table(tree_pred ,high_test)

(94 + 60) / 200

prune_carseats <- prune.misclass(tree_carseats , best = 14)

plot(prune_carseats)

text(prune_carseats ,pretty = 0)

tree_pred <- predict(prune_carseats ,newdata = carseats_data_test ,type = "class")

table(tree_pred ,high_test)

(86 + 67) / 200


## Regression Trees
boston_data <- Boston %>% 
  as_tibble()

set.seed(1)

train <- sample(1:nrow(boston_data) ,nrow(boston_data) / 2)

tree_boston <- tree(medv ~ . ,boston_data ,subset = train)

summary(tree_boston)

plot(tree_boston)

text(tree_boston ,pretty = 0)

cv_boston <- cv.tree(tree_boston)

plot(cv_boston$size ,cv_boston$dev ,type = "b")

yhat <- predict(tree_boston ,newdata = boston_data[-train ,])

boston_test <- boston_data[-train ,"medv"]

plot(yhat ,boston_test$medv)

abline(0 ,1)

mean((yhat - boston_test$medv)^2)

## Bagging & Random Forests
set.seed(1)

bag_boston <- randomForest(medv ~ . 
                           ,data = boston_data 
                           ,subset = train
                           ,mtry = 12
                           ,importance = TRUE
                           )

bag_boston

yhat_bag <- predict(bag_boston ,newdata = boston_data[-train ,])

plot(yhat_bag ,boston_test$medv)

abline(c(0 ,1))

mean((yhat_bag - boston_test$medv)^2)

rf_boston <- randomForest(medv ~ . 
                          ,data = boston_data 
                          ,subset = train
                          ,mtry = 6
                          ,importance = TRUE)


yhat_rf <- predict(rf_boston ,newdata = boston_data[-train ,])

plot(yhat_rf ,boston_test$medv)

mean((yhat_rf - boston_test$medv)^2)

importance(rf_boston)

varImpPlot(rf_boston)

## Boosting
set.seed(1)

boost_boston <- gbm(medv ~ . 
                    ,data = boston_data[train ,]
                    ,distribution = "gaussian"
                    ,n.trees = 5000
                    ,interaction.depth = 4
                    )

summary(boost_boston)

plot(boost_boston ,i = "rm")
plot(boost_boston ,i = "lstat")

yhat_boost <- predict(boost_boston
                      ,newdata = boston_data[-train ,]
                      ,n.trees = 5000
                      )

mean((yhat_boost - boston_test$medv)^2)

## BART
x <- Boston[ ,1:12]

y <- Boston[ ,"medv"]

xtrain <- x[train ,]
ytrain <- y[train]

xtest <- x[-train ,]
ytest <- y[-train]

set.seed(1)

bartfit <- gbart(xtrain ,ytrain ,x.test = xtest)

yhat_bart <- bartfit$yhat.test.mean

mean((ytest - yhat_bart)^2)

ord <- order(bartfit$varcount.mean ,decreasing = TRUE)

bartfit$varcount.mean[ord]










