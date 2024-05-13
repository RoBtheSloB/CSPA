###################################################################
## B4. Linear, Shrinkage and Regularization Methods
###################################################################
## Libraries
library(tidyverse)
library(ISLR2)
library(MASS)
library(leaps) # needed for best subset selection and cross-validation
## ... leaps actually wasn't needed for this section
library(glmnet) # needed for the lasso & ridge regressions

options(scipen = 999)

## load the dataset
hitters_data <- ISLR2::Hitters %>% 
  as_tibble() %>% 
  na.omit()

colnames(hitters_data) <- colnames(hitters_data) %>% tolower()

hitters_train <- sample(c(TRUE ,FALSE) ,nrow(hitters_data) ,replace = TRUE)

hitters_test <- (!hitters_train)

regfit_best <- regsubsets(salary ~ . ,data = hitters_data[hitters_train ,] ,nvmax = 19)

## Never mind ... that section above was for best subset selection 
## Going to stop that and skip to the lasso/ridge regression sections

hitters_x <- model.matrix(salary ~ . ,hitters_data)[,-1]

hitters_y <- hitters_data$salary

hitters_grid <- 10^seq(10 ,-2 ,length = 100)

## alpha = 0 means ridge regression (alpha = 1 means lasso)
hitters_ridge_mod <- glmnet(hitters_x ,hitters_y ,alpha = 0 ,lambda = hitters_grid)

dim(coef(hitters_ridge_mod))

names(hitters_ridge_mod)

hitters_ridge_mod$lambda
hitters_ridge_mod$lambda[50]

coef(hitters_ridge_mod)[,50]

sqrt(sum(coef(hitters_ridge_mod)[-1 ,50]^2))

hitters_ridge_mod$lambda[60]

coef(hitters_ridge_mod)[,60]

sqrt(sum(coef(hitters_ridge_mod)[-1 ,60]^2))

predict(hitters_ridge_mod ,s = 50 ,type = "coefficients")[1:20 ,]

set.seed(1)

hitters_train <- sample(1:nrow(hitters_x) ,nrow(hitters_x) / 2)

hitters_test <- -hitters_train

hitters_y_test <- hitters_y[hitters_test]

hitters_ridge_mod2 <- glmnet(hitters_x[hitters_train ,] 
                             ,hitters_y[hitters_train] 
                             ,alpha = 0 
                             ,lambda = hitters_grid
                             ,thresh = 1e-12)

mean((hitters_ridge_pred2 - hitters_y_test)^2)

hitters_ridge_pred2 <- predict(hitters_ridge_mod2 
                               ,s = 4 
                               ,newx = hitters_x[hitters_test ,])

mean((hitters_ridge_pred2 - hitters_y_test)^2)

mean((mean(hitters_y[hitters_train]) - hitters_y_test)^2)

set.seed(1)

hitters_cv_out <- cv.glmnet(hitters_x[hitters_train ,]
                            ,hitters_y[hitters_train]
                            ,alpha = 0
                            )

plot(hitters_cv_out)

best_lam <- hitters_cv_out$lambda.min

best_lam

log(best_lam)

## Lasso regression
hitters_lasso_mod <- glmnet(hitters_x[hitters_train ,]
                            ,hitters_y[hitters_train]
                            ,alpha = 1
                            )

plot(hitters_lasso_mod)

set.seed(1)

hitters_cv_out2 <- cv.glmnet(hitters_x[hitters_train ,]
                             ,hitters_y[hitters_train]
                             ,alpha = 1
                             )

plot(hitters_cv_out2)

hitters_best_lam <- hitters_cv_out2$lambda.min

hitters_lasso_pred <- predict(hitters_lasso_mod
                              ,s = hitters_best_lam
                              ,newx = hitters_x[hitters_test ,]
                              )

mean((hitters_lasso_pred - hitters_y[hitters_test])^2)

lasso_out <- glmnet(hitters_x ,hitters_y ,alpha = 1 ,lambda = hitters_grid)

lasso_coef <- predict(hitters_cv_out2 ,type = "coefficients" ,s = hitters_best_lam)[1:20 ,]

lasso_coef








