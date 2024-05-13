###################################################################
## B3. Resampling Method & Model Selection
###################################################################
## Libraries
library(tidyverse)
library(ISLR2)
library(MASS)
library(boot) ## needed for LOOCV

options(scipen = 999)

## load the data
auto_data <- ISLR2::Auto %>% 
  as_tibble()

set.seed(1)

auto_train <- sample(392 ,196)

lm_fit <- lm(mpg ~ horsepower ,data = auto_data ,subset = auto_train)

summary(lm_fit)

## calculating the MSE on the holdout set for various degrees of polynomial
mean((auto_data$mpg[-auto_train] - predict(lm_fit ,newdata = auto_data[-auto_train,]))^2)

lm_fit2 <- lm(mpg ~ poly(horsepower ,degree = 2) ,data = auto_data ,subset = auto_train)

mean((auto_data$mpg[-auto_train] - predict(lm_fit2 ,newdata = auto_data[-auto_train,]))^2)

summary(lm_fit2)

lm_fit3 <- lm(mpg ~ poly(horsepower ,degree = 3) ,data = auto_data ,subset = auto_train)

mean((auto_data$mpg[-auto_train] - predict(lm_fit3 ,newdata = auto_data[-auto_train,]))^2)

## same thing but with a new seed
set.seed(2)

auto_train <- sample(392 ,196)

lm_fit <- lm(mpg ~ horsepower ,data = auto_data ,subset = auto_train)

summary(lm_fit)

## calculating the MSE on the holdout set for various degrees of polynomial
mean((auto_data$mpg[-auto_train] - predict(lm_fit ,newdata = auto_data[-auto_train,]))^2)

lm_fit2 <- lm(mpg ~ poly(horsepower ,degree = 2) ,data = auto_data ,subset = auto_train)

mean((auto_data$mpg[-auto_train] - predict(lm_fit2 ,newdata = auto_data[-auto_train,]))^2)

summary(lm_fit2)

lm_fit3 <- lm(mpg ~ poly(horsepower ,degree = 3) ,data = auto_data ,subset = auto_train)

mean((auto_data$mpg[-auto_train] - predict(lm_fit3 ,newdata = auto_data[-auto_train,]))^2)

## Leave One Out Cross Validation (LOOCV)
glm_fit <- glm(mpg ~ horsepower ,data = auto_data)

summary(glm_fit)

## GLM & LM are the same if the family is not specified on the GLM
lm_fit <- lm(mpg ~ horsepower ,data = auto_data)

summary(lm_fit)

glm_fit2 <- glm(mpg ~ horsepower ,data = auto_data)

cv_err <- cv.glm(auto_data ,glm_fit2)

names(cv_err)

summary(cv_err)

cv_err$delta

set.seed(17)

## K fold cross-validation
## W/o specifying K = 2 in the loop below
## then LOOCV is performed
cv_error <- rep(0 ,10)

for (i in 1:10) {
  glm_fit3 <- glm(mpg ~ poly(horsepower ,i) ,data = auto_data)
  cv_error[i] <- cv.glm(auto_data ,glm_fit3 ,K = 10)$delta[1]
}

round(cv_error ,2)

which.min(cv_error)

## Bootstrap method
portfolio_data <- ISLR2::Portfolio %>% 
  as_tibble()

colnames(portfolio_data) <- colnames(portfolio_data) %>% tolower()

alpha_fn <- function(data ,index) {
  x <- data$x[index]
  y <- data$y[index]
  (var(y) - cov(x ,y)) / (var(x) + var(y) - 2 * cov(x ,y))
}

alpha_fn(portfolio_data ,1:100)

set.seed(7)

alpha_fn(portfolio_data ,sample(100 ,100 ,replace = TRUE))

boot(portfolio_data ,alpha_fn ,R = 1000)

boot_fn <- function(data ,index) {
  coef(lm(mpg ~ horsepower ,data = data ,subset = index))
}

boot_fn(auto_data ,index = 1:392)

set.seed(1)

boot_fn(auto_data ,sample(392 ,392 ,replace = TRUE))

boot(auto_data ,boot_fn ,1000)

summary(lm(mpg ~ horsepower ,data = auto_data))$coef

boot_fn2 <- function(data ,index) {
  coef(lm(mpg ~ horsepower + I(horsepower^2) ,data = data ,subset = index))
}

set.seed(1)

boot(auto_data ,boot_fn2 ,1000)

summary(lm(mpg ~ horsepower + I(horsepower^2) ,data = auto_data))$coef


