###################################################################
## A3. Linear Regression
###################################################################
## Libraries
library(tidyverse)
library(ISLR2)
library(readxl)
library(GGally) ## ggpairs
library(car) ## vif

options(scipen = 999)

## Advertisign dataset
file_path <- "C:/Users/riese/Downloads/Advertising.csv"

advertising <- read_csv(file_path)

colnames(advertising)[1] <- "row_num"
colnames(advertising) <- tolower(colnames(advertising))

sales_on_tv <- lm(sales ~ tv ,data = advertising)

advertising$estimated_sales_tv <- sales_on_tv$fitted.values

advertising %>% 
  ggplot(aes(x = tv ,y = sales)) +
    geom_point() +
    geom_line(aes(y = estimated_sales_tv))

predict(sales_on_tv ,advertising)

advertising %>% 
  mutate(test = predict(sales_on_tv ,advertising)
         ,diff = abs(estimated_sales_tv - test)) %>% 
  arrange(desc(diff))


summary(sales_on_tv)

sales_on_all_vars <- lm(sales ~ tv + radio + newspaper ,data = advertising)

summary(sales_on_all_vars)

advertising %>% 
  select(tv ,radio ,newspaper ,sales) %>% 
  cor()

advertising %>% 
  select(tv ,radio ,newspaper ,sales) %>% 
  pairs()

ISLR2::Credit %>% 
  as_tibble() %>% 
  select(Balance ,Age ,Cards ,Education ,Income ,Limit ,Rating) %>% 
  ggpairs()

balance_on_own <- lm(Balance ~ Own ,data = Credit)
balance_on_region <- lm(Balance ~ Region ,data = Credit)

summary(balance_on_own)
summary(balance_on_region)

sales_w_interaction <- lm(sales ~ tv + radio + tv:radio ,data = advertising)

summary(sales_w_interaction)

sales_w_interaction$coefficients

balance_w_interaction <- lm(Balance ~ Income + Student + Income:Student ,data = Credit)

summary(balance_w_interaction)

Credit %>% 
  mutate(prediction = predict(balance_w_interaction ,Credit)) %>% 
  ggplot(aes(x = Income ,y = prediction ,color = Student)) +
    geom_line()

## Lab
library(MASS)
library(ISLR2)

Boston %>% head()

attach(Boston)

boston_lm_fit <- lm(medv ~ lstat)

summary(boston_lm_fit)
names(boston_lm_fit)

confint(boston_lm_fit)

predict(boston_lm_fit 
        ,tibble(lstat = c(5 ,10 ,15))
        ,interval = "confidence"
        )

plot(lstat ,medv)
abline(boston_lm_fit)
abline(boston_lm_fit ,lwd = 3)
abline(boston_lm_fit ,lwd = 3 ,col = "red")
abline(boston_lm_fit ,pch = 20)
abline(boston_lm_fit ,pch = "+")
plot(1:20 ,1:20 ,pch = 1:20)

par(mfrow = c(2 ,2))
plot(boston_lm_fit)

plot(predict(boston_lm_fit) ,residuals(boston_lm_fit))
plot(predict(boston_lm_fit) ,rstudent(boston_lm_fit))
plot(hatvalues(boston_lm_fit))
which.max(hatvalues(boston_lm_fit))

## Multiple Linear Regression (part of Lab)
boston_mult_reg <- lm(medv ~ lstat + age)

summary(boston_mult_reg)
detach(Boston)
rm(list = ls())

boston_subset <- Boston %>% 
  as_tibble() %>% 
  select(everything())

boston_mult_reg <- lm(medv ~ . ,data = Boston)

summary(boston_mult_reg)

boston_mult_reg$coefficients

vif(boston_mult_reg)

boston_mult_reg2 <- lm(medv ~ . - age ,data = Boston)

summary(boston_mult_reg2)

boston_mult_reg3 <- update(boston_mult_reg ,~ . - age)

summary(boston_mult_reg3)

lm(medv ~ lstat * age ,data = Boston) %>% 
  summary()

boston_polynomial_fit <- lm(medv ~ lstat + I(lstat^2) ,data = Boston)

summary(boston_polynomial_fit)

boston_comparison_fit <- lm(medv ~ lstat ,data = Boston)

anova(boston_comparison_fit ,boston_polynomial_fit)

par(mfrow = c(2 ,2))
plot(boston_polynomial_fit)

boston_poly5 <- lm(medv ~ poly(lstat ,5) ,data = Boston)

summary(boston_poly5)

lm(medv ~ log(lstat) ,data = Boston) %>% 
  summary()

## Qualitative Predictors
head(Carseats)

Carseats %>% 
  ggpairs()

carseats_lm_fit <- lm(Sales ~ . + Income:Advertising + Price:Age ,data = Carseats)

summary(carseats_lm_fit)

contrasts(Carseats$ShelveLoc)

?contrasts
