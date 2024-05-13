###################################################################
## A7. Claim Reserving and IBNR Modeling
###################################################################
## Libraries
library(tidyverse)
library(ISLR2)

options(scipen = 999)

## Advertisign dataset
file_path <- "C:/Users/riese/Downloads/Advertising.csv"

advertising <- read_csv(file_path)

wage_data <- ISLR2::Wage %>% 
  as_tibble() 


## There really wasn't too much too learn or study here

## The one formula that's important can be seen below:

## E(y - f_hat(x_0))^2 = Var(f_hat(x_0)) + Bias(f_hat(x_0))^2 + Var(E)


## E(y - f_hat(x_0))^2 denotes the expected test MSE at x_0
## And refers to the avg test MSE that we would obtain 
## if we repeatedly estimated f using a large number of training sets
## and tested at each x_0


















