###############################################################################################################################################
###############################################################################################################################################
##' This is a read-only copy of the 'Question_1.R' script.  Please only use this script if you are not able to use the 'Question_1.R' script.##
###############################################################################################################################################
###############################################################################################################################################



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
library(tree)

##################
# Start analysis #
##################

#
# Part A.
#
## Produce descriptive statistics of losses.



#
# Part B.
#
## What proportion of policyholders in the data have a claim?



#
# Part C.
#
## Using the random seed  of 2, create a training and test 
## sample, using 50% of the data for each.



#
# Part D.
#
## Using the training sample, Fit a tree to the dependent variable 
## claims using the predictor variables, but excluding the other 
## dependent variables. Set the minimum deviance parameter to  
## 0.005 using mindev = 0.005 in your tree function. 
## Print output from tree fit. 



#
# Part E.
#
##  Plot the tree and comment on the plot 



#
# Part F.
#
## Use set.seed(100007) and the cv.tree function to determine the 
## best pruned tree, then produce the pruned tree.



