#################################################################
# Starter Script for Regularized Regression using glmnet        #
# Created by Jeremiah W. Johnson for the                        #
# 2014 UNH Summer Analytics Institute                           #
#################################################################

# Load ggplot2 so we can access the diamonds data
# We won't be able to improve much on this model, but we're familiar with the dataset
# and know what to expect
# Intall ggplot2 if you haven't already
# install.packages("ggplot2")
library(ggplot2)

# Split the data for cross-validation purposed. We'll just make a training and a test set this time. 
# 40000 obs. in the training set, 13940 in the test set
index <- 1:nrow(diamonds)
trainindex <- sample(index, 40000, replace = FALSE)
train <- diamonds[trainindex,]
test <- diamonds[-trainindex,]


# Uncomment the next line if you haven't installed the package glmnet (you probably haven't yet)
install.packages("glmnet")

# Then load the library
library(glmnet)

# the name glmnet contains two references: one to generalized linear models, and the other to 'nets.'
# these 'nets' are going to help us produce good regression-type models with less likelihood of overfitting

# useful is a package that contains two functions built specifically to prep your data for use in glmnet
# it's not mandatory to use it, but it is a handy timesaver. Uncomment below to add, then load
install.packages("useful")
library(useful)

# glmnet uses sophisticated algorithms to run quickly. As such it requires you to tweak a few things
# before you get started. First, your data frame must be coerced into a 'design matrix' instead of a data frame.
# The build.x command does that, and setting contrasts = FALSE gives us levels for every factor, which is a 
# 'best practice' when using this algorithm. Note that I've input the formula that we gave R previously in the 
# lm and glm commands, except for the -1, which removes the intercept term. glmnet puts the intercept in on its own.

trainX <- build.x(price~carat+cut+color+clarity - 1, data = train, contrasts = FALSE)
testX <- build.x(price~carat+cut+color+clarity - 1, data = test, contrasts = FALSE)

# The matrices have the same number of rows as our sets, but more columns due to the factor variables
dim(trainX)
dim(testX)

# Look at the first few rows of trainX - see the 'dummy variable' coding for the factors?
head(trainX) 

# Now build the output design matrix - these are just column vectors
trainY <- build.y(price~carat+cut+color+clarity - 1, data = train)
testY <- build.y(price~carat+cut+color+clarity - 1, data = test)

# Fit a glmnet model with default settings (this is called a Lasso)
# this is not a command you will often use - it's here simply to 
# illustrate what cv.glmnet does at each step
diamondCV <- glmnet(trainX, trainY)

# A summary of fit is uninformative here (run it and see). glmnert was designed so that all of the
# useful information could be extracted by its few functions (such as plot, coef, predict).
# Plot defaults to show how the coefficients grow in size as the penalty is allowed to grow 
plot(diamondCV)

# With lambda as x variable - this is more useful for visualizing what the penalty parameter does to the 
# coefficients
plot(diamondCV, xvar = "lambda")

# With labels on the coefficients
plot(diamondCV, xvar = "lambda", label = TRUE)

# Before we look at the coefficients, look at the following: we've actually created many
# models at once (the default is 100, but glmnet exits earlier if it decides not much is
# happening, as it did here). In the output that the command below produces, Df is the 
# number of variables in the model and %Dev is the percentage of null deviance explained
# (higher = better)
print(diamondCV)

# lambda is called s in this command; quirky, I know
# The dots are coefficients that equal zero for the current choice of lambda
# the lasso penalty tends to eliminate highly correlated variables
coef(diamondCV, s = 12.660)

# How to choose the best lambda to use? k-fold cross-validation.
# Fortunately the software will do the hard work for us
# The command cv.glmnet performs k-fold cross validation with a default of k = 10
# (you can change that with the nfolds =  ... option) to select the lambda that minimizes 
# an appropriate error metric (for this example, glmnet will automatically choose the
# mean squared error).
# cv.glmnet is the command in this package that you will most often use to build a model
diamondCV2 <- cv.glmnet(trainX, trainY)

# The default plot of a cv.glmnet object is NOT the coefficient plot - it's a
# plot of how the mean squared error changes as lambda (more precisely, log(lambda)) 
# increases
plot(diamondCV2)

# cv.glmnet stores the value of lambda that produces minimum predicted error as lambda.min
diamondCV2$lambda.min

# let's look at the coefficients that correspond to that particular lambda
coef(diamondCV2, s = "lambda.min")

# lmabda.1se is generally considered a better choice (bet on sparsity, use a more 'parsimonious' mode)
coef(diamondCV2, s = "lambda.1se")

# On the plot, the dashed lines indicate where lambda.min and lambda.1se lie

# Make a prediction - if you don't specify s, the default is lambda.1se
prediction <- predict(diamondCV2, testX, s = diamondCV2$lambda.min)

# Let's do a comparison using mse
net.mse <- 1/(nrow(testX))*sum((prediction - test$price)^2)
net.mse

# basic linear model from last time, built on our training data (no tranformations)
lm.fit <- lm(price~carat+cut+clarity+color, data = train)
lm.prediction <- predict(lm.fit, test)
lm.mse <- 1/(nrow(test))*sum((lm.prediction - test$price)^2)
lm.mse

# How do the results compare? (remember that we've asked R to choose several things randomly, so your
# results may look different than mine)

# By default, glmnet uses lambda.1se for predictions, unless you tell it otherwise
prediction_default <- predict(diamondCV2, testX)
mse_default <- 1/nrow(test)*sum((prediction_default - test$price)^2)
mse_default

# Exercise 1: what if we transformed price and carat before running cv.glmnet? We probably
# should have - we're still fitting a line, we're just using a more complicated cost function to decide
# the coefficients. Should we remove or keep those high leverage outliers that we spotted last time?
# Your code here:





# glmnet sets alpha to 1 by default, which produces a lasso
# alpha = 0 produces what is called 'ridge' regression
# ridge shrinks all coefficients, but doesn't eliminate any them like the lasso does
diamondCV3 <- cv.glmnet(trainX, trainY, alpha = 0)

# Note the top axis in the plot: regardless of lambda, the number of coefficients is the same
plot(diamondCV3)

# tuning your model wtih alpha requires cross-validating twice: for each choice of alpha, cross-validate
# to pick a lambda. Then, pick the lambda and alpha that together give you the best performance.
# Unfortunately, glmnet does not automatically do this. If you want to try many values
# of alpha, you need to write the code to do it. Loops can take a long time to run with a big dataset

# Setting alpha below 0.5 leans toward the ridge, setting it above leans toward the lasso. Most of the time
# the lasso performs better ('bet on sparsity' - if the truth is sparse, the lasso will work well, if it's not, 
# nothing will do well!). alpha around 0.95 or so is a good start. Experiment!

# For further reading:
# - The Elements of Statistical Learning, by Hastie, Tibshirani, and Friedman 
# free download at http://statweb.stanford.edu/~tibs/ElemStatLearn/
# It's very good and you can get quite a bit out of it even if you skip
# the more mathematically heavy sections. Lots of advice on best practices for model
# selection and tuning. 
# - The glmnet package documentation - it is also very good. See
# http://cran.r-project.org/web/packages/glmnet/glmnet.pdf

# Also see http://www.stanford.edu/~hastie/sldm.html

###################################END FILE######################################################################