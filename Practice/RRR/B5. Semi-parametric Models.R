###################################################################
## B5. Semi-parametric Models
###################################################################
## Libraries
library(tidyverse)
library(ISLR2)
library(MASS)
library(splines) # needed for splines
library(gam) # needed for fitting GAMs
library(akima) # needed for plotting a GAM with lo (local regression)
library(interp) # needed for plotting a GAM with lo (local regression)


options(scipen = 999)

## load the data
wage_data <- ISLR2::Wage %>% 
  as_tibble()


wage_data

## 7.8.1 Polynomial Regression & Step Functions
fit <- lm(wage ~ poly(age ,4) ,data = wage_data)

summary(fit)
coef(summary(fit))

fit2 <- lm(wage ~ poly(age ,4 ,raw = TRUE) ,data = wage_data)

coef(summary(fit2))

fit2b <- lm(wage ~ cbind(wage_data$age ,wage_data$age^2 ,wage_data$age^3 ,wage_data$age^4)
            ,data = wage_data)

coef(summary(fit2b))

summary(fit2b)

agelims <- range(wage_data$age)

age_grid <- seq(from = agelims[1] ,to = agelims[2])

preds <- predict(fit ,newdata = list(age = age_grid) ,se = TRUE)

se_bands <- cbind(preds$fit + 2 * preds$se.fit 
                  ,preds$fit - 2 * preds$se.fit)

se_bands

par(mfrow = c(1 ,2) ,mar = c(4.5 ,4.5 ,1 ,1) ,oma = c(0 ,0 ,4 ,0))

plot(wage_data$age ,wage_data$wage ,xlim = agelims ,cex = .5 ,col = "darkgrey")

title("Degree-4 Polynomial" ,outer = TRUE)

lines(age_grid ,preds$fit ,lwd = 2 ,col = "blue")

matlines(age_grid ,se_bands ,lwd = 1 ,col = "blue" ,lty = 3)

preds2 <- predict(fit2 ,newdata = list(age = age_grid) ,se.fit = TRUE)

max(abs(preds$fit - preds2$fit))

fit_1 <- lm(wage ~ age ,data = wage_data)
fit_2 <- lm(wage ~ poly(age ,2) ,data = wage_data)
fit_3 <- lm(wage ~ poly(age ,3) ,data = wage_data)
fit_4 <- lm(wage ~ poly(age ,4) ,data = wage_data)
fit_5 <- lm(wage ~ poly(age ,5) ,data = wage_data)

anova(fit_1 ,fit_2 ,fit_3 ,fit_4 ,fit_5)

coef(summary(fit_5))

fit_1 <- lm(wage ~ education ,data = wage_data)
fit_2 <- lm(wage ~ education + poly(age ,2) ,data = wage_data)
fit_3 <- lm(wage ~ education + poly(age ,3) ,data = wage_data)

anova(fit_1 ,fit_2 ,fit_3)

fit <- glm(I(wage_data$wage > 250) ~ poly(age ,4) 
           ,data = wage_data
           ,family = "binomial")

preds <- predict(fit ,newdata = list(age = age_grid) ,se.fit = TRUE)

pfit <- exp(preds$fit) / (1 + exp(preds$fit))

pfit

se_bands_logit <- cbind(preds$fit + 2 * preds$se.fit
                        ,preds$fit - 2 * preds$se.fit)

se_bands <- exp(se_bands_logit) / (1 + exp(se_bands_logit))

preds <- predict(fit ,newdata = list(age = age_grid) ,type = "response" ,se.fit = TRUE)

plot(wage_data$age ,I((wage_data$wage > 250) / 5) ,cex = .5 ,pch = "|" ,ylim = c(0 ,.2))

points(jitter(wage_data$age) ,I((wage_data$wage > 250) / 5) ,cex = .5 ,pch = "|" 
       ,col = "darkgrey")

lines(age_grid ,pfit ,lwd = 2 ,col = "blue")

matlines(age_grid ,se_bands ,lwd = 1 ,col = "blue" ,lty = 3)

table(cut(wage_data$age ,4))

fit <- lm(wage ~ cut(age ,4) ,data = wage_data)

coef(summary(fit))

## 7.8.2 Splines
fit <- lm(wage ~ bs(age ,knots = c(25 ,40 ,60)) ,data = wage_data)
pred <- predict(fit ,newdata = list(age = age_grid) ,se = TRUE)

plot(wage_data$age ,wage_data$wage ,col = "gray")

lines(age_grid ,pred$fit ,lwd = 2)
lines(age_grid ,pred$fit + 2 * pred$se ,lty = "dashed")
lines(age_grid ,pred$fit - 2 * pred$se ,lty = "dashed")

dim(bs(wage_data$age ,knots = c(25 ,50 ,60)))
dim(bs(wage_data$age ,df = 6))

attr(bs(wage_data$age ,df = 6) ,"knots")

fit2 <- lm(wage ~ ns(age ,df = 4) ,data = wage_data)

pred2 <- predict(fit2 ,newdata = list(age = age_grid) ,se = TRUE)

lines(age_grid ,pred2$fit ,col = "red" ,lwd = 2)

plot(wage_data$age ,wage_data$wage ,xlim = agelims ,cex = .5 ,col = "darkgrey")
title("Smoothing Spline")

fit <- smooth.spline(wage_data$age ,wage_data$wage ,df = 16)
fit2 <- smooth.spline(wage_data$age ,wage_data$wage ,cv = TRUE)

fit2$df

lines(fit ,col = "red" ,lwd = 2)
lines(fit2 ,col = "blue" ,lwd = 2)

legend("topright" ,legend = c("16 DF" ,"6.8 DF") ,,col = c("red" ,"blue") 
       ,lty = 1 ,lwd = 2 ,cex = .8)

## 7.8.3 GAMs
gam1 <- lm(wage ~ ns(year ,4) + ns(age ,5) + education
           ,data = wage_data)

gam_m3 <- gam(wage ~ s(year ,4) + s(age ,5) + education
              ,data = wage_data)

par(mfrow = c(1 ,3))

plot(gam_m3 ,se = TRUE ,col = "blue")

plot.Gam(gam1 ,se = TRUE ,col = "red")

gam_m1 <- gam(wage ~ s(age ,5) + education ,data = wage_data)
gam_m2 <- gam(wage ~ year + s(age ,5) + education ,data = wage_data)

anova(gam_m1 ,gam_m2 ,gam_m3)

summary(gam_m3)

preds <- predict(gam_m2 ,newdata = wage_data)

gam_lo <- gam(wage ~ s(year ,df = 4) + lo(age ,span = 0.7) + education
              ,data = wage_data)

plot(gam_lo)

gam_lo_i <- gam(wage ~ lo(year ,age ,span = .5) + education
                ,data = wage_data)

plot(gam_lo_i)

gam_lr <- gam(I(wage > 250) ~ year + s(age ,df = 5) + education
              ,family = binomial ,data = wage_data)

par(mfrow = c(1 ,3))
plot(gam_lr ,se = TRUE ,col = "green")

table(wage_data$education ,I(wage_data$wage > 250))

gam_lr_s <- gam(I(wage > 250) ~ year + s(age ,df = 5) + education 
                ,family = binomial ,data = wage_data
                ,subset = (education != "1. < HS Grad"))

plot(gam_lr_s ,se = TRUE ,col = "green")


