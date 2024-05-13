# Incremental claims
load("sgautoBI9301.rda")
summary(sgautoBI9301)

inc.triangle <- sgautoBI9301

# Create a triangle of cumulative claims
cum.triangle <- t(apply(inc.triangle, 1, cumsum))
cum.triangle

# Assume there is no further development after year 9
# Compute the age-to-age link ratios for the Chain Ladder method
n <- 9
f <- sapply((n-1):1, function(i) {
  sum( cum.triangle[1:i, n-i+1] ) / sum( cum.triangle[1:i, n-i] )
})
tail <- 1
f <- c(f, tail)
f

# Calculate the ultimate loss cost using the Chain Ladder age-to-age factors computed
full.triangle <- cum.triangle
for(k in 1:(n-1)){
  full.triangle[(n-k+1):n, k+1] <- full.triangle[(n-k+1):n,k]*f[k]
}
ultimate.paid <- full.triangle[,n]
ultimate.paid

# Comment on the why the first age-to-age development factor is highly leveraged
## Textbook: As the chain-ladder method is a deterministic algorithm and does not 
## regard the observations as realizations of random variables but absolute values, 
## the forecast of the most recent origin periods can be quite unstable.

## Alternative answer: Claims are immature at this age as it is early in the development.

# Suppose the expected loss cost for the 2001 origin year is 1,500,000, 
# what is the BF Method estimate of the ultimate loss cost
ldf <- rev(cumprod(rev(f)))
dev.pattern <- 1/ldf
BF2001 <- ultimate.paid[n] * dev.pattern[1] + 1500000 * (1 - dev.pattern[1])
BF2001

# By comparing the BF method ultimate loss cost for 2001 to that of the Chain Ladder,
# comment on which method the user should select and why
## Choose the BF method since the BF Method result is closer to 1993's loss cost
## at development year 9 where the claims are most likely to be fully 
## developed and can be used as a benchmark of the ultimate loss cost for
## subsequent years.
