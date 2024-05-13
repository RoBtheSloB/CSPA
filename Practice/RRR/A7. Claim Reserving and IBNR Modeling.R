###################################################################
## A7. Claim Reserving and IBNR Modeling
###################################################################
## Libraries
library(tidyverse)
library(ISLR2)
library(CASdatasets) # Needed for replicating CAS book datasets
library(lattice) # For building plots
library(ChainLadder) # triangle stuff
library(AER) # dispersion test (Over dispersed Poisson)
library(fitdistrplus)


options(scipen = 999)

## creating the data
## Number of development periods
n <- 7 

claims <- tibble(originf = factor(rep(2007:2013 ,n:1))
                 ,dev = sequence(n:1)
                 ,inc_paid = c(3511 ,3215 ,2266 ,1712 ,1059 ,587
                               ,340 ,4001 ,3702 ,2278 ,1180 ,956
                               ,629 ,4355 ,3932 ,1946 ,1522 ,1238
                               ,4295 ,3455 ,2023 ,1320 ,4150 ,3747
                               ,2320 ,5102 ,4548 ,6283
                               )
                 )

inc_triangle <- with(claims ,{
  M <- matrix(nrow = n ,ncol = n
              ,dimnames = list(origin=levels(claims$originf) ,dev=1:n)
              )
  M[cbind(claims$originf ,claims$dev)] <- claims$inc_paid
  M
})

inc_triangle

inc_triangle_test <- claims %>% 
  pivot_wider(names_from = "dev" ,values_from = inc_paid) %>%
  column_to_rownames(var = "originf") %>% 
  as.matrix()

colnames(inc_triangle_test)
colnames(inc_triangle)

cum_triangle <- t(apply(inc_triangle ,1 ,cumsum))

latest_paid <- cum_triangle[row(cum_triangle) == n - col(cum_triangle) + 1]

claims$cum_paid <- cum_triangle[with(claims ,cbind(originf ,dev))]

op <- par(fig = c(0 ,0.5 ,0 ,1)
          ,cex = 0.8
          ,oma = c(0 ,0 ,0 ,0)
          )

with(claims ,{
  interaction.plot(x.factor = dev ,trace.factor = originf ,response = inc_paid
                   ,fun = sum ,type = "b" ,byt = 'n' ,legend = FALSE); axis(1 ,at=1:n)
  par(fig=c(0.45 ,1 ,0 ,1) ,new=TRUE ,cex = 0.8 ,oma = c(0,0,0,0))
  interaction.plot(x.factor = dev ,trace.factor=originf ,response = cum_paid ,fun = sum
                   ,type = "b" ,bty = 'n'); axis(1 ,at=1:n)
})

mtext("Incremental and cumulative claims developent"
      ,side = 3 ,outer=TRUE ,line = -3 ,cex = 1.1 ,font = 2)

par(op)

xyplot(cum_paid ~ dev | originf
       ,data = claims
       ,t = "b"
       ,layout = c(4,2)
       ,as.table = TRUE
       ,main = "cumulative claims development"
       )

f <- sapply((n-1):1 ,function(i) {
  sum(cum_triangle[1:i ,n-i+1]) / sum(cum_triangle[1:i ,n-i])
})

tail <- 1

f <- c(f ,tail)

f

full_triangle <- cum_triangle

for(k in 1:(n-1)) {
  full_triangle[(n-k+1):n ,k+1] <- full_triangle[(n-k+1):n ,k]*f[k]
}

full_triangle

ultimate_paid <- full_triangle[,n]

ultimate_paid

ldf <- rev(cumprod(rev(f)))

ldf

dev_pattern <- 1/ldf

dev_pattern

reserve <- sum(latest_paid * (ldf - 1))

reserve

a <- ultimate_paid

b <- c(dev_pattern[1] ,diff(dev_pattern))

x_hat <- a %*% t(b)

bf2013 <- ultimate_paid[n] * dev_pattern[1] + 20000 * (1 - dev_pattern[1])

bf2013

dat <- tibble(lf1=log(f[-c(1,n)]-1) ,dev = 2:(n-1))

m <- lm(lf1 ~ dev ,data = dat)

summary(m)

plot(lf1 ~ dev ,msin = 'log(f-1) ~ dev' ,data = dat ,bty = 'n')

abline(m)

sigma <- summary(m)$sigma

extrapolation <- predict(m ,data.frame(dev=n:100))

tail <- prod(exp(extrapolation + .5 * sigma^2) + 1)

tail

f

ata(cum_triangle)

names(claims)[3:4] <- c("inc_paid_k" ,"cum_paid_k")

ids <- with(claims ,cbind(originf ,dev))

ids

claims <- within(claims ,{
  cum_paid_kp1 <- cbind(cum_triangle[,-1] ,NA)[ids]
  inc_paid_kp1 <- cbind(inc_triangle[,-1] ,NA)[ids]
  devf <- factor(dev)
})

claims

delta <- 0:2

ata <- sapply(delta ,function(d) {
  coef(lm(cum_paid_kp1 ~ 0 + cum_paid_k : devf
          ,weights = 1/cum_paid_k^d
          ,data = claims))
})

ata

dimnames(ata)[[2]] <- paste("Delta " ,delta)

ata

mack <- MackChainLadder(cum_triangle ,weights = 1 ,alpha = 1
                        ,est.sigma = "Mack")

mack

plot(mack)

plot(mack ,lattice = TRUE ,layout = c(4 ,2))

preg <- glm(inc_paid_k ~ originf + devf
            ,data = claims
            ,family = poisson(link = "log")
            )

summary(preg)

all_claims <- tibble(origin = sort(rep(2007:2013 ,n))
                     ,dev = rep(1:n ,n)
                     )

all_claims <- within(all_claims ,{
  devf <- factor(dev)
  cal <- origin + dev - 1
  originf <- factor(origin)
})

pred_inc_tri <- t(matrix(predict(preg ,type = "response" ,newdata = all_claims)
                         ,n ,n))

pred_inc_tri

sum(predict(preg ,type = "response" ,newdata = all_claims %>% filter(cal > 2013)))

df <- c(0 ,coef(preg)[(n+1):(2*n-1)])

sapply(2:7 ,function(i) {
  sum(exp(df[1:i])) / sum(exp(df[1:(i-1)]))
})

dispersiontest(preg)

odpreg <- glm(inc_paid_k ~ originf + devf
              ,data = claims
              ,family = quasipoisson
              )

summary(odpreg)

mu_hat <- predict(odpreg ,newdata = all_claims ,type = "response")*(all_claims$cal>2013)

phi <- summary(odpreg)$dispersion

sigma <- vcov(odpreg)

model_formula <- as.formula(paste("~" ,formula(odpreg)[3]))

x <- model.matrix(model_formula ,data = all_claims)

x

cov_eta <- x %*% sigma %*% t(x)

sqrt(phi * sum(mu_hat) + t(mu_hat) %*% cov_eta %*% mu_hat)

op <- par(mfrow=c(2,2) ,oma = c(0,0,3,0))

plot(preg)

par(op)

odp <- glmReserve(as.triangle(inc_triangle) ,var.power=1 ,cum=FALSE)

odp

set.seed(1)

b <- BootChainLadder(cum_triangle ,R = 1000 ,process.distr = "od.pois")

b

plot(b)

quantile(b ,c(0.75 ,0.95 ,0.99 ,0.995))

fit <- fitdist(b$IBNR.Totals[b$IBNR.Totals>0] ,"lnorm")

fit

plot(fit)

qlnorm(0.995 ,meanlog = fit$estimate['meanlog'] ,sdlog = fit$estimate['sdlog'])

ny <- col(inc_triangle) == nrow(inc_triangle) - row(inc_triangle) + 2

paid_ny <- apply(b$IBNR.Triangles ,3 ,function(x) {
  next_year_paid <- x[col(x) == nrow(x) - row(x) + 2]
  sum(next_year_paid)
})

paid_ny_995 <- b$IBNR.Triangles[,,order(paid_ny)[round(b$R*0.995)]]

inc_triangle_ny <- inc_triangle

inc_triangle_ny[ny] <- paid_ny_995[ny]

inc_triangle_ny[ny]

claims <- within(claims ,{
  log_inc <- log(inc_paid_k)
  cal <- as.numeric(levels(originf))[originf] + dev - 1
})

claims

claims <- within(claims ,{
  d1 <- ifelse(dev < 2 ,1 ,0)
  d27 <- if_else(dev < 2 ,0 ,dev - 1)
})

fit1 <- lm(log_inc ~ originf + d1 + d27 
           ,data = claims)

summary(fit1)

claims <- within(claims ,{
  a6 <- ifelse(originf == 2012 ,1 ,0)
  a7 <- ifelse(originf == 2013 ,1 ,0)
})

claims

fit2 <- lm(log_inc ~ d1 + d27 + a6 + a7
           ,data = claims)

summary(fit2)

op(mfrow = c(2,2) ,oma = c(0 ,0 ,3 ,0))

plot(fit2)

shapiro.test(fit2$residuals)

res_plot <- function(model ,data) {
  xvals <- list(
    fitted = model[['fitted.values']]
    ,origin = as.numeric(levels(data$originf))[data$originf]
    ,cal=data$cal
    ,dev=data$dev
  )
}

op <- par(mfrow=c(2,2) ,oma = c(0 ,0 ,3 ,0))

for(i in 1:4){
  plot.default(rstandar(model) ~ xvals[[i]])
}













































