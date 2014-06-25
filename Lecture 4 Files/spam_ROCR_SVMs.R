####################################################
# An in-depth analysis of the spam data            #
# Created by Jeremiah W. Johnson for the           #
# 2014 UNH Summer Analytics Institute              #
####################################################

# Here we analyze the spam data more closely - with lasso, randomForest, and ROCR
# you will need to install ROCR, e1071, and nnet - do so in the next few lines
install.packages("ROCR")
install.packages("e1071")

install.packages("nnet")

# load several libraries
library(ElemStatLearn)
library(randomForest)
library(useful)
library(glmnet)
library(ROCR)
library(e1071)
library(nnet)

# load the data; review its structure
data(spam)
str(spam)

# separate out a test set and a training set
index <- 1:nrow(spam)
set.seed(8675309)
trainindex <- sample(index, 3800, replace = FALSE)
train <- spam[trainindex,]
test <- spam[-trainindex,]

# randomForest runs faster with design matrices as inputs - let's use
# that approach this time
trainX <- model.matrix(spam~., data = train)
testX <- model.matrix(spam~., data = test)

# may take a minute to run
rf <- randomForest(trainX, train$spam, do.trace = 50, importance = TRUE)
print(rf)

# precision based on OOB
rf.precision <- 1398/(66+1398); rf.precision
# recall based on OOB
rf.recall <- 1398/(113+1398); rf.recall
# F1 based on OOB
rf.F1 <- (2*rf.precision*rf.recall)/(rf.precision+rf.recall); rf.F1

# Exercise: For practice, implement the calculation of F2 here:




# Exercise: Try running a second random forest. Include the parameter mtry, and set it 
# to a different value (something other than 7). Does it improve the model?
# Your code here: 



# Random Forest plot with legend (not included by default, not explained in documentation)
# for the record, green is by default the error on the positives (i.e. false negatives), 
# red is error on the email (false positives), and black is the total OOB error.
# don't pay any attention to this code - you get the same plot, minus the legend, with the single
# command plot(yourrandomForest) - just remember that green always represents error on positive, 
# black represents OOB error, and red is error on negative
layout(matrix(c(1,2), nrow = 1), width = c(4,1))
par(mar = c(5, 4, 4, 0))
plot(rf)
par(mar = c(5, 0, 4, 2))
plot(c(0,1), type = "n", axes = FALSE, xlab = "", ylab = "")
legend("top", colnames(rf$err.rate), col = 1:4, cex = 0.8, fill = 1:4)

# For this problem, precision is much more important than recall: we want that value as
# close as possible to 1. 

# For comparison, here is the penalized logistic regression model
# first we build (slightly different) design matrices with build.x
trainX2 <- build.x(spam~. - 1, data = train, contrasts = FALSE)
trainY2 <- build.y(spam~. - 1, data = train)
testX2 <- build.x(spam~. - 1, data = test)

# lasso with defaults
set.seed(11235813)
lasso <- cv.glmnet(trainX2, trainY2, family = "binomial")

# predictions, first as classes, then as probabilities
lasso.pred <- predict(lasso, testX2, type = "class")
lasso.prob <- predict(lasso, testX2, type = "response")

# error and accuracy
lasso.err <- sum(lasso.pred != test$spam)/nrow(test); lasso.err
lasso.acc <- 1 - lasso.err; lasso.acc

# glmnet doesn't automatically produce a confusion matrix for us in its cross-validation
# process, so we'll create one using our test data.
ltp <- sum(lasso.pred == "spam" & test$spam == "spam"); ltp
lfp <- sum(lasso.pred == "spam" & test$spam == "email"); lfp
ltn <- sum(lasso.pred == "email" & test$spam == "email"); ltn
lfn <- sum(lasso.pred == "email" & test$spam == "spam"); lfn
l.precision <- ltp/(ltp+lfp); l.precision
l.recall <- ltp/(ltp+lfn); l.recall

# In fairness, we should be comparing these results with those of the random forest on the test
# data. We're fudging a bit by relying on the OOB numbers.

# thresholding
library(ROCR)

# The predicted probabilities on the test data from our random forest model
rf.prob <- predict(rf, testX, type = "prob")

# Confusing syntax: a 'prediction object' is an object containing a prediction from some model
# and the actual values from that model. These objects are inputs to ROCR for thresholding.
l.predobj <- prediction(lasso.prob, test$spam) # the lasso prediction object
rf.predobj <- prediction(rf.prob[,2], test$spam) # the random forest prediction object


# graphical parameter switch - bad margins
par(mfrow = c(1,1), mar = c(5, 5, 4, 3))
# Receiver Operating Characteristic (ROC) curves
lasso.roc <- performance(l.predobj, 'tpr', 'fpr')
rf.roc <- performance(rf.predobj, 'tpr', 'fpr')

rf.pr <- performance(rf.predobj, 'prec', 'rec')
plot(rf.pr, colorized = TRUE)

plot(lasso.roc, colorize = TRUE, main = "lasso")
plot(rf.roc, colorize = TRUE, main = "forest")

?performance

# graphical parameter adjustment so that plots can be compared
# side-by-side
par(mfrow = c(2,1), mar = c(5, 5, 4, 3)) # compare side by side
plot(lasso.roc, colorize = TRUE, main = "lasso")
plot(rf.roc, colorize = TRUE, main = "forest")

# with labels printed - hard to see without increasing plot size
par(mfrow = c(1,1))
plot(performance(l.predobj, 'tpr', 'fpr'), colorize = TRUE, print.cutoffs.at = seq(0,1, by = 0.1), main = "lasso")
plot(performance(rf.predobj, 'tpr', 'fpr'), colorize = TRUE, print.cutoffs.at = seq(0, 1, by = 0.1), main = "forest")

# testing the result - our random forest probabilities are stored in rf.prob
# let's look back at it quickly
head(rf.prob)
# new class predictions for the forest - only classify as spam IF prob spam > .9.
# We can't use the default setting here.
# create a row of emails
rf.strict <- c(rep("email", nrow(test)))
# assign spam to those rows for which rf.prob exceeds .9
rf.strict[rf.prob[,2]>.9] <- "spam"
head(rf.strict)

# create a confusion matrix and compare results
rfstp <- sum(rf.strict == "spam" & test$spam == "spam"); rfstp 
rfsfp <- sum(rf.strict == "spam" & test$spam == "email"); rfsfp
rfstn <- sum(rf.strict == "email" & test$spam == "email"); rfstn
rfsfn <- sum(rf.strict == "email" & test$spam == "spam"); rfsfn
rfs.precision <- rfstp/(rfstp+rfsfp); rfs.precision
rfs.recall <- rfstp/(rfstp+rfsfn); rfs.recall

###########################Support Vector Machine#######################################################

# A support vector machine is a model similiar in some ways to a logistic regression model
# It uses a slightly different cost function, and can learn nonlinear boundaries

# cost is the C parameter (see slides). Often needs adjustment. There are no good heuristics here. 
# The decision values and probability options tell the software to save the information that it
# uses for classification - necessary for making predictions with probabilities
# default is radial basis (Gaussian) with gamma = 1/ncols. gamma also often needs adjustment. 
# There are no good heuristics for tuning gamma either.
sv <- svm(spam~., data = train, cost = 0.1, decision.values = TRUE, probability = TRUE)
summary(sv)

sv.pred <- predict(sv, test)
summary(sv.pred)

# the error rate
svm.err <- sum(sv.pred != test$spam )/nrow(test); svm.err

# Exercise: For this SVM, what are the precision and recall?
# Your code below:





# the syntax for this package is awkward (the package is actually an interface 
# to a C++ library not originally written for R). To get a probability prediction, 
# and to plot an ROC curve, you need to extract certain 'attributes' of the 
# prediction object. (bio response groups, you may want to recycle some of the 
# prediction code below if you would like to take your data for a test drive on an SVM)
sv.pred2 <- predict(sv, test[,1:57], decision.values = TRUE, probability = TRUE)
# the prediction contains labels only
head(sv.pred2)
# however, one of its hidden 'attributes' is a probabilities matrix
svm.predobj <- prediction(attributes(sv.pred2)$probabilities[,1], test[,58])
plot(performance(svm.predobj, 'tpr', 'fpr'), colorize = TRUE)

# Exercise: Tune the SVM model. Copy  and paste the code from line 173, but try different 
# values of  cost. Try including and adjusting gamma. Can you improve on the error rate? 
# (calculated in lines 176 - line 180)
# For example (this does not perform very well):
svm2 <- svm(spam~., data = train, cost = 0.05, gamma = .1, decision.values = TRUE, probability = TRUE)

# make a prediction and measure the error rate
svm2.pred <- predict(svm2, test)
summary(svm2.pred)
sum(sv.pred != test$spam)/nrow(test)

# Your code here:



# Exercise: Find the precision and recall of your best SVM model. Create a prediction object
# using ROCR (refer back to lines 95 - 105) and plot the ROC curve.
# Your code here: 




# Exercise: Tune is a helper function: it will scan over a wide range of C's and gammas
# and will return the best choices as estimated by, you guessed it, 10-fold cross-validation.
# Experiment with it if you'd like. Sample code below:

tuned <- tune(svm, spam~., data = train)
########################################END####################################################
