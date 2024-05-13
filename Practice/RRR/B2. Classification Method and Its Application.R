###################################################################
## B2. Classification Method and Its Application
###################################################################
## Libraries
library(tidyverse)
library(ISLR2)
library(MASS)
library(class) # Needed for KNN analysis

options(scipen = 999)

## Loading in the dataset
default_data <- ISLR2::Default %>% 
  as_tibble()

test_lm <- glm(default ~ balance ,data = default_data ,family = binomial)

summary(test_lm)

test_glm2 <- glm(default ~ student ,data = default_data ,family = binomial)

summary(test_glm2)

test_glm3 <- glm(default ~ balance + income + student ,data = default_data ,family = binomial)

summary(test_glm3)


## Linear Discriminant Analysis (LDA)
stock_market <- ISLR2::Smarket %>% 
  as_tibble()

colnames(stock_market) <- colnames(stock_market) %>% tolower()

stock_train <- stock_market %>% 
  filter(year < 2005)

stock_2005 <- stock_market %>% 
  filter(year == 2005)

stock_training <- (stock_market < 2005)

lda_fit <- lda(direction ~ lag1 + lag2 ,data = stock_market ,subset = stock_training)

summary(lda_fit)

plot(lda_fit)

lda_pred <- predict(lda_fit ,stock_2005)

names(lda_pred)

lda_class <- lda_pred$class

table(lda_class ,stock_2005$direction)

mean(lda_class == stock_2005$direction)

## Quadratic Discriminant Analysis (QDA)
qda_fit <- qda(direction ~ lag1 + lag2 ,data = stock_market ,subset = stock_training)

qda_fit

qda_pred <- predict(qda_fit ,stock_2005)

names(lda_pred)

qda_class <- qda_pred$class

table(qda_class ,stock_2005$direction)

mean(qda_class == stock_2005$direction)

train_x <- cbind(stock_train$lag1 ,stock_train$lag2)
test_x <- cbind(stock_2005$lag1 ,stock_2005$lag2)

train_direction <- stock_train$direction

set.seed(1)

knn_pred <- knn(train_x ,test_x ,train_direction ,k = 1)

table(knn_pred ,stock_2005$direction)

(83 + 43) / 252

knn_pred3 <- knn(train_x ,test_x ,train_direction ,k = 3)

table(knn_pred3 ,stock_2005$direction)

(48 + 87) / 252



