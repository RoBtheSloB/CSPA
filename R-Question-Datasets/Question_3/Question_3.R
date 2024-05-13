#
# CSPA Exam 3
#
# R Question #3
#
#
# Note:
# Question reviewers will only rely on information contained in your script to grade your answer.  The grader
# must be able to run the script to recreate your answer so be sure your script records every relevant action you
# have taken.  If you execute lines at the console, be sure to copy them to your script if the commands are necessary
# in order for your script to work properly.  For example if you create an object or variable from the console and 
# then reference that variable in your script, the script will not run for the grader, since that object or variable
# are not created in the script.  When you have completed the question save your R script (*.R) and R Project (*.Rproj)
# files.
#



#
# Clear the R Environment

rm(list=ls())

#
# Set working directory

setwd("~/Question_3")

#
# Load the data

df <- read.csv("SAheart.data"
               , row.names = 1)

#
# Libraries used in analysis
#

library(MASS)

#################
# Start analysis#
#################

#Part A. Using linear discriminant analysis on the training dataset, construct
#        a model to predict whether an individual has coronary heart disease.
#        Produce summary output for the model.


#Part B. Using the model created in Part A produce a prediction for each record
#        and plot the prediction on a histogram by the values of ‘chd’.



#Part C. Score the hold out data set using the model in Part A.  Create a
#        confusion matrix and determine the sensitivity and specificity
#        of the model.


