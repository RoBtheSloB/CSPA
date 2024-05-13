#' CSPA Exam 3 - Predictive Modeling - Methods & Techniques
#' Practice Exam Fall 2022
#' Question R1


#' Below are any statements necessary to load in data or libraries 
#' which you will need to answer the question. Should you accidentally 
#' modify any of this code, you may recover it from the file called 
#' 'question1_read_only.R'. Changes should be made in this file only.

#' Note:
#' Question reviewers will only rely on information contained in your script 
#' to grade your answer.  The grader must be able to run the script to recreate 
#' your answer so be sure your script records every relevant action you have taken.  
#' If you execute lines at the console, be sure to copy them to your script if the 
#' commands are necessary in order for your script to work properly.  For example, 
#' if you create an object or variable from the console and then reference that 
#' variable in your script, the script will not run for the grader, since that object 
#' or variable are not created in the script.  When you have completed the question 
#' save your R script (*.R) and R Project (*.Rproj) files.

#' Clear the R Environment

rm(list=ls())

#' Set working directory

setwd("~/Question_1")

#' Load dataset

load("MotorcycleData2.rda")

#' Libraries used in analysis

library(MASS)
library(car)
library(boot)

##################
# Start analysis #
##################

#
# Part A.
#
## Produce descriptive statistics of losses.

str(MotorcycleData2)

summary(MotorcycleData2$losses)

#
# Part B.
#
## What proportion of policyholders in the data have a claim?

table(MotorcycleData2$ClaimIndicator)

# The proportion of policyholders with a claim is equal to

(table(MotorcycleData2$ClaimIndicator) / length(MotorcycleData2$ClaimIndicator))[2]

# that is 5% of policyholders have claims.

#
# Part C.
#
## Using the random seed  of 2, create a training and test 
## sample, using 50% of the data for each.

set.seed(2)
idx <- sample(1:20000, 10000, replace = FALSE)
train <- MotorcycleData2[idx,]
test  <- MotorcycleData2[-idx,]


#
# Part D.
#
## Using the training sample, Fit a tree to the dependent variable 
## claims using the predictor variables, but excluding the other 
## dependent variables. Set the minimum deviance parameter to  
## 0.005 using mindev = 0.005 in your tree function. 
## Print output from tree fit. 

fm <- tree(claims ~ age + gender + zone + 
             engine.class + car.age + bonus.class +
             policy.duration + CreditScore2,
           data = train,
           mindev = 0.005)
summary(fm)

#
# Part E.
#
##  Plot the tree and comment on the plot 

plot(fm)
text(fm, pretty = 0)
fm

# The most important variables are:
#   1. Age
#   2. Zone
#   3. Policy duration
#   4. Car Age

# The tree has 13 terminal nodes.
# For policyholders with age < 30.5 the mean claims are 0.10260
# compared with policyholders with age > 30.5 with a mean
# claims of 0.03376


#
# Part F.
#
## Use set.seed(100007) and the cv.tree function to determine the 
## best pruned tree, then produce the pruned tree.

set.seed(100007)
cv.fm <- cv.tree(fm)
plot(cv.fm, type = "b")
idx <- which.min(cv.fm$dev)
cv.fm$size[idx]
cv.fm

# The best pruned tree has a size of 2

best <- prune.tree(fm, best = 2)
best
plot(best)
text(best, pretty = 0)

