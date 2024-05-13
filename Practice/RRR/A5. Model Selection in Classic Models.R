###################################################################
## A5. Model Selection in Classic Models
###################################################################
## Libraries
library(tidyverse)
library(ISLR2)
library(leaps) # subset selection

options(scipen = 999)

## Lab
hitters_no_na <- Hitters %>% 
  as_tibble() %>% 
  na.omit()

colnames(hitters_no_na) <- colnames(hitters_no_na) %>% tolower()

## Best Subset Selection
hitters_subset_model <- regsubsets(salary ~ . ,data = hitters_no_na)

summary(hitters_subset_model)

hitters_subset_model <- regsubsets(salary ~ . 
                                   ,data = hitters_no_na
                                   ,nvmax = 19
                                   )

summary(hitters_subset_model)

hitters_subset_summary <- summary(hitters_subset_model)

names(hitters_subset_summary)

hitters_subset_summary$rsq
which.max(hitters_subset_summary$rsq)

par(mfrow = c(2 ,2))

plot(hitters_subset_summary$rss 
     ,xlab = "Number of Variables"
     ,ylab = "RSS"
     # ,type = "1"
     )

plot(hitters_subset_summary$adjr2 
     ,xlab = "Number of Variables"
     ,ylab = "Adjusted RSq"
     # ,type = "1"
     )

which.max(hitters_subset_summary$adjr2)

points(11 
       ,hitters_subset_summary$adjr2[11]
       ,col = "red"
       ,cex = 2
       ,pch = 20
       )

plot(hitters_subset_summary$cp 
     ,xlab = "Number of Variables"
     ,ylab = "Cp"
     # ,type = "1"
     )

which.min(hitters_subset_summary$cp)

points(10 
       ,hitters_subset_summary$cp[10]
       ,col = "red"
       ,cex = 2
       ,pch = 20
       )

plot(hitters_subset_summary$bic 
     ,xlab = "Number of Variables"
     ,ylab = "BIC"
     # ,type = "1"
     )

which.min(hitters_subset_summary$bic)

points(6 
       ,hitters_subset_summary$bic[6]
       ,col = "red"
       ,cex = 2
       ,pch = 20
       )

plot(hitters_subset_model ,scale = "r2")
plot(hitters_subset_model ,scale = "adjr2")
plot(hitters_subset_model ,scale = "Cp")
plot(hitters_subset_model ,scale = "bic")

coef(hitters_subset_model ,6)

## Forward & Backward Selection
hitters_forward_model <- regsubsets(salary ~ . 
                                    ,data = hitters_no_na
                                    ,nvmax = 19
                                    ,method = "forward")

summary(hitters_forward_model)

hitters_backward_model <- regsubsets(salary ~ . 
                                     ,data = hitters_no_na
                                     ,nvmax = 19
                                     ,method = "backward")

summary(hitters_backward_model)

coef(hitters_subset_model ,7)
coef(hitters_forward_model ,7)
coef(hitters_backward_model ,7)














