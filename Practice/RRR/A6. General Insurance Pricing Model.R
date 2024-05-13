###################################################################
## A6. General Insurance Pricing Model
###################################################################
## Libraries
library(tidyverse)
library(ISLR2)
library(CASdatasets) # Needed for replicating CAS book datasets
library(mgcv) # Penalized regression splines
library(MASS) # negative binomial glm
library(AER) # used for an overdispersion test (i.e. to determine if the variance != mean
             # for Poisson regression)
library(gamlss) # Used for zero inflated Poisson or NB models
library(pscl) # Used for zero inflated Poisson or NB models
library(splines) # Used for creating splines
library(tweedie) # for tweedie distribution/modeling


options(scipen = 999)

## load the data
data(freMTPLfreq)
data(freMTPL2freq)
data("freMTPL2sev")
data("freMTPLsev")

contracts <- freMTPLfreq %>% as_tibble()

colnames(contracts) <- colnames(contracts) %>% tolower()

contracts_w_bands <- contracts %>% 
  mutate(driver_age_band = cut(driverage ,breaks= c(17 ,22 ,26 ,42 ,74 ,Inf))
         ,car_age_band   = cut(carage ,breaks= c(0 ,1 ,4 ,15 ,Inf) ,include.lowest = TRUE)
         ,density_band   = cut(density ,breaks = c(0 ,40 ,200 ,500 ,4500 ,Inf) 
                               ,include.lowest = TRUE ,dig.lab = 5)
         ,car_age_band2  = cut(carage ,breaks = c(0 ,15 ,Inf) ,include.lowest = TRUE)
         )

claim_severity <- freMTPLsev %>% 
  as_tibble()

colnames(claim_severity) <- colnames(claim_severity) %>% tolower()

mean_frequency <- contracts_w_bands %>% 
  summarise(sum(claimnb) / sum(exposure)) %>% 
  pull()

mean_frequency_by_region <- contracts_w_bands %>% 
  group_by(region) %>% 
  summarise(mean_regional_freq = sum(claimnb) / sum(exposure))

vY <- contracts_w_bands$claimnb
vE <- contracts_w_bands$exposure
m <- sum(vY) / sum(vE)
v <- sum((vY-m*vE)^2)/sum(vE)
str_c("avg = " ,m ," variance = " ,v ," phi = " ,v/m)

contracts_w_bands


contracts_w_bands %>% 
  mutate(expected_claim_count    = mean_frequency * exposure
         ,actual_minus_exp_cc    = claimnb - expected_claim_count
         ,actual_minus_exp_cc_sq = actual_minus_exp_cc^2
         ) %>% 
  summarise(claim_count       = sum(claimnb)
            ,exposure         = sum(exposure)
            ,mean             = claim_count / exposure
            ,implied_variance = sum(actual_minus_exp_cc_sq) / sum(exposure)
            ,phi              = implied_variance / mean
            )

contracts_w_bands %>% 
  left_join(mean_frequency_by_region ,by = c("region")) %>% 
  mutate(expected_claim_count    = mean_regional_freq * exposure
         ,actual_minus_exp_cc    = claimnb - expected_claim_count
         ,actual_minus_exp_cc_sq = actual_minus_exp_cc^2
  ) %>% 
  group_by(region) %>% 
  summarise(claim_count       = sum(claimnb)
            ,exposure         = sum(exposure)
            ,mean             = claim_count / exposure
            ,implied_variance = sum(actual_minus_exp_cc_sq) / sum(exposure)
            ,phi              = implied_variance / mean) %>% 
  arrange(region)

contracts_w_bands %>% 
  summarise(sum(claimnb) / sum(exposure))

auto_contract_glm <- glm(claimnb ~ gas + driver_age_band + density_band + offset(log(exposure))
                         ,data = contracts_w_bands
                         ,family = poisson
                         )

summary(auto_contract_glm)

contracts_w_bands %>% 
  group_by(gas) %>% 
  summarise(claims = sum(claimnb)
            ,freq  = sum(claimnb) / sum(exposure)
            )

auto_contract_glm2 <- glm(claimnb ~ 0 + gas + offset(log(exposure))
                          ,data = contracts_w_bands
                          ,family = poisson()
                          )

summary(auto_contract_glm2)

auto_contract_reg_spline <- gam(claimnb ~ s(driverage) + offset(log(exposure))
                                ,data = contracts_w_bands
                                ,family = poisson()
                                )

summary(auto_contract_reg_spline)

contracts_w_bands %>% 
  select(brand) %>% 
  distinct()

contracts_w_bands %>% 
  select(power) %>% 
  distinct()

## pg 499
tail(claim_severity)

claim_severity %>% 
  filter(IDpol == 303133)

summary(claim_severity$PolicyID)
summary(contracts_w_bands$policyid %>% as.numeric())

contracts_w_severity <- contracts_w_bands %>% 
  mutate(policyid = as.numeric(policyid)) %>% 
  left_join(claim_severity ,by = c("policyid"))

severity_initial_model_logn <- lm(log(claimamount) ~ car_age_band + gas
                                  ,data = contracts_w_severity %>% filter(claimamount < 15000)
                                  )

summary(severity_initial_model_logn)

severity_model_gamma <- glm(claimamount ~ car_age_band + gas
                            ,data = contracts_w_severity %>% filter(claimamount < 15000)
                            ,family = Gamma(link = "log")
                            )

summary(severity_model_gamma)


mean(severity_model_gamma$fitted.values)
mean(exp(severity_initial_model_logn$fitted.values))

severity_model_gamma <- glm(claimamount ~ car_age_band + gas
                            ,data = contracts_w_severity
                            ,family = Gamma(link = "log")
                            )

severity_model_logn <- lm(log(claimamount) ~ car_age_band + gas
                          ,data = contracts_w_severity
                          )

contracts_w_severity %>% 
  summarise(mean(claimamount ,na.rm = TRUE))
mean(severity_model_gamma$fitted.values)
logn_sigma <- summary(severity_model_logn)$sigma
mean(exp(predict(severity_model_logn ,type = "response"))*exp(logn_sigma^2/2))
mean(exp(severity_model_logn$fitted.values)*exp(logn_sigma^2/2))
mean(exp(severity_model_logn$fitted.values))

age_sample_data <- tibble(driverage = seq(18 ,100))
claim_standard <- 10e3
contracts_w_severity <- contracts_w_severity %>% 
  mutate(claim_under_std = as.numeric(claimamount<claim_standard))

claim_size_model <- glm(claim_under_std ~ bs(driverage)
                        ,data = contracts_w_severity
                        ,family = binomial
                        )

summary(claim_size_model)


age_sample_data_w_preds <- age_sample_data %>% 
  bind_cols(predict(claim_size_model 
                    ,newdata = age_sample_data
                    ,type = "response"
                    ,se = TRUE
                    )) %>% 
  as_tibble()

age_sample_data_w_preds %>% 
  ggplot(aes(x = driverage ,y = fit)) + 
    geom_line()

claim_size_model_output <- predict(claim_size_model 
                                   ,newdata = age_sample_data
                                   ,type = "response"
                                   ,se = TRUE
                                   )

plot(age_sample_data$driverage ,claim_size_model_output$fit 
     ,ylim=c(.95 ,1) 
     )

polygon(c(age_sample_data$driverage ,rev(age_sample_data$driverage))
        ,c(claim_size_model_output$fit + 2 * claim_size_model_output$se.fit
           ,rev(claim_size_model_output$fit - 2 * claim_size_model_output$se.fit)
           )
        ,col = "grey"
        ,border = NA
        )
abline(h = mean(contracts_w_severity$claim_under_std) ,lty = 2)

contracts_w_severity %>% 
  filter(!is.na(claim_under_std)) %>% 
  group_by(claim_under_std) %>% 
  summarise(mean(claimamount))

severity_model_under_std_gamma <- glm(claimamount ~ driverage
                                      ,data = contracts_w_severity %>% 
                                        filter(claim_under_std == 1)
                                      ,family = Gamma(link = "log")
                                      )

severity_model_under_std_preds <- predict(severity_model_under_std_gamma
                                          ,newdata = age_sample_data
                                          ,type = "response"
                                          ) %>% as_tibble()

severity_model_over_std_gamma <- glm(claimamount ~ driverage
                                      ,data = contracts_w_severity %>% 
                                        filter(claim_under_std == 0)
                                      ,family = Gamma(link = "log")
                                      )

severity_model_over_std_preds <- predict(severity_model_over_std_gamma
                                          ,newdata = age_sample_data
                                          ,type = "response"
                                          ) %>% as_tibble()

severity_model_all_claims_gamma <- glm(claimamount ~ driverage
                                      ,data = contracts_w_severity
                                      ,family = Gamma(link = "log")
                                      )

severity_model_all_claims_preds <- predict(severity_model_all_claims_gamma
                                           ,newdata = age_sample_data
                                           ,type = "response"
                                           ) %>% as_tibble()
plot(age_sample_data$driverage ,severity_model_all_claims_preds$value
     ,lwd=2
     ,ylab="Avg Cost"
     ,xlab="Age of Driver"
     )

lines(severity_model_over_std_preds$value * age_sample_data_w_preds$fit + 
        (1 - age_sample_data_w_preds$fit) * severity_model_under_std_preds$value
      ,type = "h"
      ,col = "grey"
      ,lwd = 6
      )

A <- tapply(contracts_w_severity$claimamount ,contracts_w_severity$policyid ,sum)
ADF <- data.frame(ID=names(A) ,INDEMNTIY=as.vector(A))
CT <- merge(contracts_w_severity ,ADF ,all.x = TRUE)

contracts_w_severity <- contracts_w_severity %>% 
  group_by(policyid) %>%
  summarise(n = n()) %>% 
  arrange(desc(n))
  mutate(total_indemnity = sum(claimamount ,na.rm = TRUE))

