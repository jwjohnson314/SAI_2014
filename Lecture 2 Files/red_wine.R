########################################################################
# Multinomial regression using glmnet                                  #
# Created by Jeremiah W. Johnson for the                               #
# 2014 UNH Summer Analytics Institute                                  #
########################################################################


# This file demonstrates the use of glmnet for multinomial logistic regression
# (classification with more than two classes). There are other packages in R 
# that do this (or you can do it yourself using a one-vs-many argument), but
# glmnet will do it and will incorporate the penalties and cross-validation
# for tuning lambda that we've been discussing.

# Load the red wine data. First navigate the file pane (bottom right) to wherever you've put the red wine file.
# Then in the menu bar above, click on Session -> Set Working Directory -> To Files Pane Location.
#  The next command will then load the file. Alternatively, you can specify the path directly, something like
# reds <- read.csv("~/Desktop/SAIL2/winequality.red.csv", sep = ";", header = TRUE) for me right now 
# (note that the above assumes that you are using Rstudio)

reds <- read.csv("winequality-red.csv", sep = ";", header = TRUE)

# if you get an error, it may be due to the semicolon separator. Try using csv2 instead:
reds2<- read.csv2("winequality-red.csv")

str(reds)

# separate out a test set
index <- 1:nrow(reds)
trainindex <- sample(index, 1400, replace = FALSE)
train <- reds[trainindex,]
test <- reds[-trainindex,]

# use build.x to get the design matrices for glmnet
library(useful)
trainX <- build.x(quality~. - 1, data = train)
trainY <- build.y(quality~. -1, data = train)
testX <- build.x(quality~. -1, data = test)

# generate model. The option type.multinomial = "grouped" tells glmnet to keep the same coefficients
# in each of the models it generates. If you leave this out, it may choose different coefficients in
# the various models (not necessarily a bod thing. Experiment!) 

library(glmnet)
redCV <- cv.glmnet(trainX, trainY, family = "multinomial") # or,
# redCV <- cv.glmnet(trainX, trainY, family = "multinomial", type.multinomial = "grouped")


# plot multinomial deviance against log(lambda)
plot(redCV)

# plot coefficients against lambda
plot(redCV$glmnet.fit, xvar = "lambda")

# What coefficient is in red?
plot(redCV$glmnet.fit, xvar = "lambda", label = TRUE)

# add vertical lines at lambda.min and lambda.1se (lty is line type, 2 is dashed)
abline(v = log(c(redCV$lambda.min, redCV$lambda.1se)), lty = 2)

coef(redCV, s = redCV$lambda.1se)

# set type equal to class to get the class that the model places the highest probability on
# remember lambda.1se is the default here. Note that this isn't an option in glm for ordinary 
# logistic regression
prediction <- predict(redCV, testX, type = "class")

# An annoyance - class produces the predicted classes, but stored as characters. 
# the following command changes the characters to integers.
# Without this change, we'll get an error when we try to estimate the error in our prediction
prediction <- as.integer(prediction)

# The next command calculates the mean absolute error. For multiclass problems like this one, this is a 
# reasonable choice for an error metric. It's better than simply calculating how many you predicted correctly
# vs. how many you predicted incorrectly, because it penalizes large predictive errors more than small predictive
# errors (for instance, if you predicted a 2 for a wine ranked a 7, the error would be greater than if you had predicted
# an 8).
redCVmad <- (1/nrow(test))*sum(abs(prediction - test$quality)) 
redCVmad

# A ridge version - alpha = 1 is lasso (default), alpha = 0 is ridge
# ridge shrinks all coefficients but doesn't kill off any of them
redRidge <- cv.glmnet(trainX, trainY, family = "multinomial", type.multinomial = "grouped", alpha = 0)

# Notice ridge kept all coefficients (top of plot)
plot(redRidge)
coef(redRidge, s = "lambda.1se")

ridgepredict <- predict(redRidge, testX, type = "class")
ridgepredict <- as.integer(ridgepredict)
ridgemad <- (1/nrow(test))*sum(abs(ridgepredict - test$quality))

# ridge beats lasso this time
ridgemad

# Exercise 1: Make a prediction using the lasso with lambda.min instead of the default
# lambda.1se. How does the mad on the test set compare?
# Your code here:




# Exercise 2: Experiment with different values of alpha. Can you improve on ridgemad?
# Your code here:







###############################################END FILE###################################################
