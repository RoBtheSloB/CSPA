###################################################################
## A4. Generalized Linear Models
###################################################################
## Libraries
library(tidyverse)
library(ISLR2)

options(scipen = 999)


Default %>% 
  as_tibble()

default_on_balance <- glm(default ~ balance 
                          ,data = Default
                          ,family = binomial)

default_on_student <- glm(default ~ student 
                          ,data = Default
                          ,family = binomial)

summary(default_on_student)
names(default_on_student)

default_on_student$coefficients[1]

odds <- exp(default_on_student$coefficients[1] + default_on_student$coefficients[2])

odds / (1 + odds)

Default %>% as_tibble()

default_all_vars <- glm(default ~ .
                        ,data = Default
                        ,family = binomial)

default_all_vars
default_all_vars$coefficients
names(default_all_vars)
summary(default_all_vars)

Default %>% 
  as_tibble() %>% 
  arrange(balance) %>% 
  group_by(student) %>% 
  mutate(cum_defaults      = cumsum(default == "Yes")
         ,cum_count        = cumsum(default %in% c("Yes" ,"No"))
         ,cum_default_rate = cum_defaults / cum_count
         ) %>% 
  ggplot(aes(x = balance ,y = cum_default_rate ,color = student)) +
    geom_line()

bikeshare_initial_lm <- lm(bikers ~ workingday + temp + weathersit + mnth + hr
                           ,data = Bikeshare
                           )
summary(bikeshare_initial_lm)

Bikeshare %>% 
  ggplot(aes(x = hr ,y = bikers)) +
    geom_point() +
    geom_jitter() + 
    geom_smooth()


bikeshare_initial_glm <- glm(bikers ~ workingday + temp + weathersit + mnth + hr
                             ,family = poisson 
                             ,data = Bikeshare)

summary(bikeshare_initial_glm)

## Lab
Smarket %>% 
  as_tibble()

Smarket %>% 
  select(-Direction) %>% 
  cor()

plot(Smarket$Volume)

Smarket %>% 
  ggplot(aes(x = Volume)) +
  geom_histogram()

smarket_glm_fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume
                       ,data = Smarket
                       ,family = binomial)

summary(smarket_glm_fit)
coef(smarket_glm_fit)
smarket_glm_fit$coefficients

summary(smarket_glm_fit)$coefficients

estimated_probabilities <- predict(smarket_glm_fit ,type = "response")

estimated_probabilities[1:10]
contrasts(Smarket$Direction)

estimated_probabilities <- estimated_probabilities %>% 
  enframe(name = "row_num" ,value = "probability") %>% 
  select(-row_num) %>% 
  mutate(direction = if_else(probability > .5 ,"Up" ,"Down") %>% as.factor())

?predict

table(estimated_probabilities$direction ,Smarket$Direction)

smarket_w_preds <- Smarket %>% 
  as_tibble() %>% 
  mutate(pred_direction  = predict(smarket_glm_fit ,type = "response")
         ,pred_direction = if_else(pred_direction > .5 ,"Up" ,"Down") %>% as.factor()
         )

colnames(smarket_w_preds) <- colnames(smarket_w_preds) %>% tolower()

table(smarket_w_preds$pred_direction ,smarket_w_preds$direction)

(145 + 507) / (1205)
mean(smarket_w_preds$pred_direction == smarket_w_preds$direction)

smarket_train <- Smarket %>% 
  as_tibble() %>% 
  filter(Year < 2005)

smarket_test <- Smarket %>% 
  as_tibble() %>% 
  filter(Year >= 2005)

colnames(smarket_train) <- colnames(smarket_train) %>% tolower()
colnames(smarket_test) <- colnames(smarket_test) %>% tolower()

smarket_glm_fit2 <- glm(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + volume
                        ,data = smarket_train
                        ,family = binomial)

summary(smarket_glm_fit2)
summary(smarket_glm_fit2)$coefficients

smarket_test <- smarket_test %>% 
  mutate(pred_direction  = predict(smarket_glm_fit2 ,newdata = smarket_test ,type = "response")
         ,pred_direction = if_else(pred_direction > .5 ,"Up" ,"Down") %>% as.factor()
         )

table(smarket_test$pred_direction ,smarket_test$direction)
mean(smarket_test$direction == smarket_test$pred_direction)

smarket_glm_fit3 <- glm(direction ~ lag1 + lag2
                        ,data = smarket_train
                        ,family = binomial)

smarket_test2 <- smarket_test %>% 
  mutate(pred_direction  = predict(smarket_glm_fit3 ,newdata = smarket_test ,type = "response")
         ,pred_direction = if_else(pred_direction > .5 ,"Up" ,"Down") %>% as.factor()
         )

table(smarket_test2$pred_direction ,smarket_test2$direction)
mean(smarket_test2$direction == smarket_test2$pred_direction)

106 / (106 + 76)

predict(smarket_glm_fit3
        ,newdata = tibble(lag1 = c(1.2 ,1.5)
                          ,lag2 = c(1.1 ,-0.8))
        ,type = "response"
        )

## Poisson Regression
bikeshare_lab_lm <- lm(bikers ~ workingday + temp + weathersit + mnth + hr
                       ,data = Bikeshare)

summary(bikeshare_lab_lm)
summary(bikeshare_lab_lm)$coefficients

contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)

bikeshare_lab_lm2 <- lm(bikers ~ workingday + temp + weathersit + mnth + hr
                        ,data = Bikeshare)

summary(bikeshare_lab_lm2)

bikeshare_month_coefs <- c(coef(bikeshare_lab_lm2)[7:17]
                           ,-sum(coef(bikeshare_lab_lm2)[7:17])
                           )

plot(bikeshare_month_coefs
     ,xlab = "Month"
     ,ylab = "Coefficient"
     ,xaxt = "n"
     ,col = "blue"
     ,pch = 19
     ,type = "o"
     )

axis(side = 1
     ,at = 1:12
     ,labels = c("J" ,"F" ,"M" ,"A" ,"M" ,"J" ,"J" ,"A" ,"S" ,"O" ,"N" ,"D")
     )

bikeshare_hour_coefs <- c(coef(bikeshare_lab_lm2)[18:40]
                          ,-sum(coef(bikeshare_lab_lm2)[18:40])
                          )

plot(bikeshare_hour_coefs
     ,xlab = "Hour"
     ,ylab = "Coefficient"
     ,xaxt = "n"
     ,col = "blue"
     ,pch = 19
     ,type = "o"
     )

bikeshare_lab_glm <- glm(bikers ~ mnth + hr + workingday + temp + weathersit
                         ,family = poisson 
                         ,data = Bikeshare)

summary(bikeshare_lab_glm)

bikeshare_glm_month_coefs <- c(coef(bikeshare_lab_glm)[2:12]
                               ,-sum(coef(bikeshare_lab_glm)[2:12]))

plot(bikeshare_glm_month_coefs
     ,xlab = "Month"
     ,ylab = "Coefficient"
     ,xaxt = "n"
     ,col = "blue"
     ,pch = 19
     ,type = "o"
)

axis(side = 1
     ,at = 1:12
     ,labels = c("J" ,"F" ,"M" ,"A" ,"M" ,"J" ,"J" ,"A" ,"S" ,"O" ,"N" ,"D")
)

bikeshare_glm_hour_coefs <- c(coef(bikeshare_lab_glm)[13:35]
                              ,-sum(coef(bikeshare_lab_glm)[13:35]))

plot(bikeshare_glm_hour_coefs
     ,xlab = "Hour"
     ,ylab = "Coefficient"
     ,xaxt = "n"
     ,col = "blue"
     ,pch = 19
     ,type = "o"
)

plot(predict(bikeshare_lab_lm2) ,predict(bikeshare_lab_glm ,type = "response"))
abline(0 ,1 ,col = 2 ,lwd = 3)



