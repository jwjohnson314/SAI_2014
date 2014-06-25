##############################################################
# Logistic Regression, Trees, and Random Forests             #
# Created by Jeremiah W. Johnson for the                     #
# 2014 UNH Summer Analytics Institute                        #
##############################################################

# This script will give a little more practice with logistic regression and 
# penalized logistic regression, then will introduce trees and forests

# load the ElemStatLearn Library to access the spam dataset
# you probably installed this last week. If not, uncomment the next line
# install.packages("ElemStatLearn")
library(ElemStatLearn)

# load the spam database 
data(spam)

# view its structure
str(spam)

# most of the columns correspond to frequencies of specific words/punctuation symbols,
# though the last few have to do with strings of capital letters, exclamation points, etc.
# see the spambase.names.txt supporting file for the details

# Separate out training and test data
# set a seed so our results match. Otherwise, you've seen this code before
index <- 1:nrow(spam)
set.seed(7)
trainindex <- sample(index, 4000, replace = FALSE) 
train <- spam[trainindex,]
test <- spam[-trainindex,]

# Fit a logistic regression model
spam_logistic_model <- glm(spam~., family = "binomial", data = train)
# The warning message is not problematic - the logistic model has 
# assigned very high/low probabilities of spam to some observations
summary(spam_logistic_model)

# make a prediction - glm will not return classes, only probabilities
glmprediction <- predict(spam_logistic_model, test, type = "response")

# put test$spam and glmprediction together for comparison
glm_dataframe <- data.frame(test$spam, glmprediction)
head(glm_dataframe)

# add a class column
glm_dataframe$predictedclass <- "email" 
glm_dataframe$predictedclass[glm_dataframe$glmprediction >= 0.5] <- "spam"
head(glm_dataframe)

# calculate error on test set using (number misclassified)/(all classified)
# the symbols != mean `not equal to' 
glm_err <- sum(glm_dataframe$test.spam != glm_dataframe$predictedclass)/nrow(test)
glm_err

# A quick graphical interlude
# Shrink the training set a little bit for better visualization
train_shrink <- train[-which(train$A.52 > 2),]
train_shrink <- train_shrink[-which(train_shrink$A.56 > 500), ]

# plot the data using only A.52 and A.56 as predictors (Number of exclamation 
# points and average run length of strings of capital letters, resp).
# Color the points by spam/email

library(ggplot2)
spamplot <- ggplot(data = train_shrink) + geom_point(aes(x = A.52, y = A.56, color = spam))
spamplot

# linear regression using only those two predictor variables
lreg <- glm(spam~A.52+A.56, data = train_shrink, family = "binomial")
coef(lreg)

# plot the line derived using the coefficients from lreg
spamplot + geom_abline(intercept = (1.86431708/0.02103327), slope = -(3.99646684/0.02103327), lty = 2)

# How about a plot with our test data (Note extreme cases not removed from graphic)
spamplot2 <- ggplot(data = test) + geom_point(aes(x = A.52, y = A.56, color = spam)) + geom_abline(intercept = (1.86431708/0.02103327), slope = -(3.99646684/0.02103327), lty = 2)
spamplot2

######################## USING GLMNET ######################################## 

# load glmnet and useful
library(glmnet)
library(useful)

# build design matrices. Don't forget the -1 to take out the intercept, since glmnet adds it back
trainX <- build.x(spam~.-1, data = train)
trainY <- build.y(spam~.-1, data = train)
testX <- build.x(spam~.-1, data = test)
testY <- build.y(spam~.-1, data = test)

# build the model as lasso for now (i.e. default alpha = 1)
# Note: If one of these models is taking too long to run, you can interupt the 
# R process by moving down to the console and hitting the esc key.
set.seed(17)
spam_glmnet_model <- cv.glmnet(trainX, trainY, family = "binomial")

# take a look at the pictures
plot(spam_glmnet_model)
plot(spam_glmnet_model$glmnet.fit, xvar = "lambda")

#let's add in lines on the last plot indicating lambda.min and lambda.1se
abline(v = log(c(spam_glmnet_model$lambda.min, spam_glmnet_model$lambda.1se)), lty = 2)

# I haven't entered lambda below, so the command defaults to return the set of
# coefficients corresponding to lambda.1se
coef(spam_glmnet_model)

# let's make a prediction with this model at that value of lambda and see how it compares
# glmnet saves us a little work here by returning the classification on its own
glmnet_prediction <- predict(spam_glmnet_model, testX, type = "class")

# let's get the error rate
glmnet_err <- sum(glmnet_prediction != test$spam)/nrow(test)
glmnet_err

# Let's try with a little ridging - this is the elastic net
set.seed(25)
elast_fit <- cv.glmnet(trainX, trainY, family = "binomial", alpha  =0.95)

plot(elast_fit)
elast_prediction <- predict(elast_fit, testX, type = "class")
elast_err <- sum(elast_prediction != test$spam)/nrow(test)
elast_err

# Let's go all the way to the ridge
set.seed(14)
ridge_fit <- cv.glmnet(trainX, trainY, family = "binomial", alpha = 0)
plot(ridge_fit)
ridge_prediction <- predict(ridge_fit, testX, type = "class")
ridge_err <- sum(ridge_prediction != test$spam)/nrow(test)
ridge_err

########################END LOGISTIC - BEGIN TREES############################

# Uncomment the next few lines to install several useful packages if you haven't installed
# them previously (you probably have not)
# install.packages("rpart")
# install.packages("rattle")
# install.packages("RColorBrewer")
# install.packages("rpart.plot")

# Load the packages
library(rpart)
library(rattle)
library(RColorBrewer)
library(rpart.plot)

# spam with an rpart model - default parameters. CV happening internally, so set.seed
set.seed(1888)
rpart_fit <- rpart(spam~., data = train)

# plot it - the default plots are awful, which is why we loaded rattle et. al. Now we can make a:
fancyRpartPlot(rpart_fit) # this type of plot neatly summarizes much useful information

# make a prediction - rpart also kindly provides the "class" option
rpart_prediction <- predict(rpart_fit, test, type = "class")

# error rate
tree1_err <- sum(rpart_prediction != test$spam)/nrow(test)
tree1_err

# Let's try to improve the tree - first let's look at cptable. This is 
# a table contained inside the rpart object, which tells us how the
# relative error and the cross-validation error estimate change as the
# complexity of the tree increase. Relative error is akin to training
# set error - it will always decrease as the tree grows. xerror, on the
# other hand, won't
rpart_fit$cptable

# let's lower the cp (and thus make the tree a bit bigger, since the cross-val error appears to
# have still been decreasing)
set.seed(1999)
rpart_fit2 <- rpart(spam~., data = train, cp = 0.005)
fancyRpartPlot(rpart_fit2)
rpart_prediction2 <- predict(rpart_fit2, test, type = "class")
tree2_err <- sum(rpart_prediction2 != test$spam)/nrow(test)
tree2_err

# Again look at cptable: cross - validation error is decreasing, so complexity is not hurting us (yet)
rpart_fit2$cptable


# Let's keep going cp = 0.001
set.seed(1492)
rpart_fit3 <- rpart(spam~., data = train, cp = 0.001)
# at this point the plots may take a little while - skip the next command if you'd like - 
# the tree is so big as to be uninformative
fancyRpartPlot(rpart_fit3)
rpart_fit3$cptable

rpart_prediction3 <- predict(rpart_fit3, test, type = "class")
tree3_err <- sum(rpart_prediction3 != test$spam)/nrow(test)
tree3_err

# Let's keep going - cp = 0
set.seed(1866)
rpart_fit4 <- rpart(spam~., data = train, cp = 0)

# skip the plot, unless you're dying to see it
# fancyRpartPlot(rpart_fit4)
rpart_fit4$cptable

rpart_prediction4 <- predict(rpart_fit4, test, type = "class")
tree4_err <- sum(rpart_prediction4 != test$spam)/nrow(test)
tree4_err

# let's try pruning the tree. We're going to use a complicated command to find
# the tree in the cptable with minimum xerror. 'rpart_fit4$cptable' brings up 
# the cptable, as we've seen, which is an array. Adding [,"xerror"] says in that array,
# access the xerror column. Leaving the row index blank says 'all rows'.
# Finally, which.min is applied to that column of the cptable, to return
# the row in which the minimum entry was found

which.min(rpart_fit4$cptable[,"xerror"])
rpart_fit4$cptable[18,]
rpart_fit4$cptable
bestcp <- rpart_fit4$cptable[18, "CP"]
bestcp
pruned_rpart_fit4 <- prune(rpart_fit4, cp = bestcp)

# How does the pruned tree perform on the test set?
pruned_prediction <- predict(pruned_rpart_fit4, test, type = "class")
sum(pruned_prediction != test$spam)/nrow(test)

# Other parameters we can override are many. Some to try are minsplit, minbucket
# minsplit is the minimum number of observations in a node in order to attempt a split.
# The default is 20. Let's try 10. When we want to adjust multiple parameters, we use
# the control option and set it to rpart.control

set.seed(1323)
rpart_fit5 <- rpart(spam~., data = train, control = rpart.control(cp = 0, minsplit = 10))

# skip the plot, unless you really want ot see it
# fancyRpartPlot(rpart_fit5)
rpart_fit5$cptable

# check on our test set
rpart_prediction5 <- predict(rpart_fit5, test, type = "class")
sum(rpart_prediction5 != test$spam)/nrow(test)

# Let's see if we can improve by pruning

# The next command is a complicated command. rpart_fit5 contains an object called a 
# cptable. 'rpart_fit5$cptable' brings up that object, as we've seen.
# That object is an array. adding [,"xerror"] says in that array,
# access the xerror column. Leaving the row index blank says 'all rows'.
# Finally, which.min is applied to that column of the cptable, to return
# the row in which the minimum entry was found
which.min(rpart_fit5$cptable[,"xerror"])

# Here, I'm accessing the cptable part of rpart_fit5 and asking for the entry in
# row 22, column "xerror". I'm naming it bestcp
bestcp <- rpart_fit5$cptable[22,"xerror"]
bestcp

# The prune command cuts back the tree we've created to the level that matches our choice of complexity parameter
pruned_best <- prune(rpart_fit5, cp = bestcp)

# It's still a big tree - run at your own risk
# fancyRpartPlot(pruned_best)
pruned_prediction <- predict(pruned_best, test, type = "class")
pruned_err <- sum(pruned_prediction != test$spam)/nrow(test)
pruned_err

################################ END TREES BEGIN FORESTS ######################################

# uncomment the next line to install the random forest package
# install.packages("randomForest")
library(randomForest)

# Random forest with default parameters
# some options set, though: importance = TRUE assesses predictor importance, do.trace = 50 gives
# output every 50 trees (we're making 500 by default, so this will let you know its progressing)
set.seed(846)
spam_forest <- randomForest(spam~., data = train, importance = TRUE, do.trace = 50)

# the print command contains lots of useful information, including a confusion matrix
print(spam_forest)

# make a prediction
forest_prediction <- predict(spam_forest, test, type = "class")
forest_err <- sum(forest_prediction != test$spam)/nrow(test)
forest_err

# mtry is usually the most important parameter to adjust in a random forest
# for classification, the default is sqrt(p), for regression, p/3, where p = the
# number of predictors. The list of things you can adjust is very long, though. See
# http://cran.r-project.org/web/packages/randomForest/randomForest.pdf for an
# in-depth look at all aspects of the package.

# the forest is good at assessing variable importance. This is neatly summarized in a plot
varImpPlot(spam_forest, pch = 19, col = "magenta")


# though if you want the numbers, they can be accessed with the command:
importance(spam_forest)

# importance is measured by how much adding a split to a tree using the variable is seen to
#decrease average oob error/Gini impurity (averaged over all the trees with that variable)

# Exercise: Experiment with varying mtry and ntree in this model. Do you get any improvement
# in accuracy on the test set?
# Your code here:






# Exercise: Load the imports85 data with the following command and look at its structure
# (This data is part of the randomForest package)
data(imports85)
str(imports85)

# Exercise:
# Some sample code for building a random forest to predict price is contained in the file at
# http://cran.r-project.org/web/packages/randomForest/randomForest.pdf on page 8. Starting with 
# this code, build your own model. What variables are important? 
# Your code here:








#################################END FILE###########################################################
