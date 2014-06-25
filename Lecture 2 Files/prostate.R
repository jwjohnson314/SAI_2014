#####################################################################
# More regularized regression using glmnet                          #
# Created by Jeremiah W. Johnson for the                            #
# 2014 UNH Summer Analytics Institute                               #
#####################################################################


# This file contains an analysis of prostate data with basic linear and lasso regression models.
# We incorporate scaling of predictors. glmnet scales automatically, but it returns unscaled
# results. If you want scaled outputs for comparison with other models, you need to process
# inputs in advance). These results match those shown in ElemStatLearn.
# Uncomment the next line and run to install the ElemStatLearn package, which contains the data 
install.packages("ElemStatLearn")

# load the ElemStatLearn library
library(ElemStatLearn)

# get the data and take a look
data(prostate)
head(prostate)

# A little preprocessing - we should standardize the predictors to have unit variance. 
# Also, in a typical linear regression one wouldn't standardize factors,
# but for theoretical reasons we do when we use elastic nets, so we will here
# First, a summary of the data
summary(prostate)

# apply is a useful function. Below, it 'applies' an input function (the 'scale' 
# function, which subtracts mean and divides by sd) to columns 1:8 of prostate. You have to
# tell it whether to apply scale by row or by column. That's what the '2' does:
# 1 indicates by row, 2 indicates by column. We're saving the result in a new dataframe 
prstd <- data.frame(apply(prostate[,1:8],2,scale))

# The new dataframe doesn't have an lpsa or a train variable (we don't need to standardize outputs, and train
# is not part of the model), so let's move those over from the original dataframe
prstd$lpsa <- prostate$lpsa
prstd$train <- prostate$train

# let's make the names match the names in the original data frame
names(prstd) <- c("lcavol", "lweight", "age", "lbph", "svi", "lcp", "gleason", "pgg45", "lpsa", "train")

#  Let's look at the sd of one column and get a general summary after that
sd(prstd$lcavol)
summary(prstd)

# All looks good
# separate into test and training sets - note that we omit column 10. 
# This makes the formula in the model step easier to write
train <- prstd[which(prostate$train == TRUE),1:9]
test <- prstd[which(prostate$train == FALSE),1:9]

head(train)
# Here we build our linear model. lpsa~. tells lm to use as predictors everything 
# in the dataframe except the lpsa column
lmfit <- lm(lpsa~., data = train)
summary(lmfit)

# make a prediction on the test set
lmpredict <- predict(lmfit, test)

# calculate mse (note that the errors are small here. rmse exxagerates them. glmnet uses mse for cross-validation.
# We'll do the same for our calculations. 
lmmse <- (1/nrow(test))*sum((lmpredict - test$lpsa)^2)
lmmse

# load glmnet and useful
library(glmnet)
library(useful)

# build the design matrix, excluding intercept and including a level for every factor
trainX <- build.x(lpsa~. - 1, data = train, contrasts = FALSE)
trainY <- build.y(lpsa~. -1, data = train)

# Take a look at the first few rows of the design matrix
head(trainX)

# fit a lasso (the default is alpha = 1, which is a lasso; alpha = 0 is the ridge)
lassofit <- cv.glmnet(trainX, trainY)

# check the CV plot
plot(lassofit)

# view the coefficients and compare with the linear model's coefficients
# note that we didn't set a seed, so your coefficients might differ slightly from
# mine, as a result of different subsets of the data being chosen in our respective 
# cross-validations
coef(lassofit, s = "lambda.1se")
coef(lmfit)

# build the design matrix for the test set
testX <- build.x(lpsa~. -1, data = test, contrasts = FALSE)

# default for predictions is to use lambda.1se, so we won't specify it
lassopredict <- predict(lassofit, testX)

# get the mse
lassomse <- (1/nrow(test))*sum((lassopredict - test$lpsa)^2)
lassomse # lasso should almost always outperform the basic linear model for this example


# compare with results using lambda.min
lassopredict2 <- predict(lassofit, testX, x = "lambda.min")
lassomse2 <- (1/nrow(test))*sum((lassopredict2 - test$lpsa)^2)
lassomse2

# when we use lambda.min, the rmse is the same, but the number of coefficients is higher, which means that the model is
# more complicated
coef(lassofit, s = "lambda.min")

# plot of when the coefficients exit by log(lambda)
plot(lassofit$glmnet.fit, xvar = "lambda")

# Exercise 3: Now try this with some ridging. Create a new model using cv.glmnet. Choose a couple values for alpha that are relatively close to 1, but not equal to 1 (say 0.95 to start). Make a couple predictions and see how the results compare. How many coefficients do you end up with in each model?
# Your code here:






####################################END FILE##########################################################