#' CSPA Exam 3 - Predictive Modeling - Methods & Techniques
#' Practice Exam
#' Question 2


#' Below are any statements necessary to load in
#' data or libraries which you will need to answer
#' the question. Should you accidentally modify
#' any of this code, you may recover it from the
#' file called 'question2_read_only.R'. Changes
#' should be made in this file only.

#' Note:
#' Question reviewers will only rely on information contained in your script to grade your answer.  The grader
#' must be able to run the script to recreate your answer so be sure your script records every relevant action you
#' have taken.  If you execute lines at the console, be sure to copy them to your script if the commands are necessary
#' in order for your script to work properly.  For example if you create an object or variable from the console and 
#' then reference that variable in your script, the script will not run for the grader, since that object or variable
# are not created in the script.  When you have completed the question save your R script (*.R) and R Project (*.Rproj)
#' files.

#' Clear the R Environment

rm(list=ls())

#' Set working directory

setwd("~/Question_2")

#' Load Training and Test Data

df_train <- read.csv("ClassificationDataTrain.csv", header=TRUE, row.names = NULL)
df_test <- read.csv("ClassificationDataTest.csv", header=TRUE, row.names = NULL)

#' Libraries used in analysis

library(MASS)
library(class)

#################
# Start analysis#
#################

# Part A.	Fit the data with a generalized linear model with family
#         binomial and link logit.
#  a) Use the model to make predictions for the test set.
#  b) How many observations in the test set were predicted correctly?
#  c) What is the accuracy of the model when applied to the test set?

# part a)
fm <- glm(Y ~ .,
          data = df_train,
          family = binomial(link = "logit"))
summary(fm)

# Selecting a threshold of 0.5 for classification
prob <- predict(fm,
                newdata = df_test,
                type = "response")
pred <- ifelse(prob > 0.5, 1, 0)

# part b)
cm.glm <- xtabs(~ Y + pred,
            data = df_test)
cm.glm

c("Accuracy" = sum(diag(cm.glm))/sum(cm.glm))

#Part B. Fit the data with a k-nearest neighbour classification model,
#        with number of nearest neighbors equal to 3.
#   Before constructing the model, run set.seed(1).
#   a)	Use the model to make predictions for the test set.
#       for the knn function, make sure class variable is in matrix,
#       with code such as "train.class=as.matrix(train$Y)"
#   b) How many observations in the test set were predicted correctly?
#   c) What is the accuracy of the model when applied to the test set?

# part a)
set.seed(1)
km <- knn(df_train[,-1], df_test[,-1], df_train[,1], k = 3)

# part b)
cm.knn <- xtabs(~ df_test$Y + km)
cm.knn

c("# Pred. Correctly" = sum(diag(cm.knn)))

# part c)

c(accuracy = sum(diag(cm.knn))/sum(cm.knn))

# Part C. Based on the results above, which of the two models would
#         you recommend?  Why?

# The GLM model has an accuracy of just 0.482, about as bad
# as random guessing.

# The 3-nearest neighbour has an accuracy above 95%,
# therefore, I would choose this model.
