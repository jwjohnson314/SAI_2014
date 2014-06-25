########################################################
# Introduction to CARTs                                #
# Created by Jeremiah W. Johnson for the               #
# 2014 UNH Summer Analytics Institute                  #
########################################################

# rpart implements the CART algorithm. 
# node impurity is measured by Gini impurity (by default)
# rattle, RColorBrewer, and rpart.plot combine to make nice pictures
library(rpart)
library(rattle)
library(RColorBrewer)
library(rpart.plot)

# Unpublished set of data from a study of Bahamas fisheries. See Zuur et. al., 2009.
# The goal here is to develop a predictive model for parrotfish densities
Bahama <- read.table("http://www.unc.edu/courses/2010spring/ecol/562/001/data/lab11/Bahama.txt", header = TRUE)
str(Bahama)

# separate out training and test data
index <- 1:nrow(Bahama)
# set.seed will insure we all get the same training index set
set.seed(8675309)
trainindex <- sample(index, 300, replace = FALSE)
train <- Bahama[trainindex,]
test <- Bahama[-trainindex,]


# Month, Station, Method, and CoralRichness should be treated as factors. They're stored in the data frame as integers. We set.seed first, because cross-validation is a random process and we want reproducible results
set.seed(8675309)
parrot <- rpart(Parrot~CoralTotal+factor(CoralRichness)+factor(Month)+factor(Station)+factor(Method), data = train)

# plot the tree
fancyRpartPlot(parrot, main = "Parrot")

# cp is the complexity parameter. It controls how deep the tree goes.
# default is cp = 0.1. At 1, the tree has only one node. Since trees are prone to 
# overfitting the data, this parameter needs careful attention. You can see how the
# cross-validation error changes with cp by either of the two following commands:
printcp(parrot)
# or
parrot$cptable

# This command returns the minimum cross-validation error
min(parrot$cptable[,"xerror"])

# let's pick out that tree
mincvtree <- which.min(parrot$cptable[,"xerror"])
parrot$cptable[mincvtree,]

# extract its cp
bestcp <- parrot$cptable[mincvtree,"CP"]

# Let's cut the tree back to that complexity level
set.seed(8675309)
pruned_parrot <- prune(parrot, cp = bestcp)
fancyRpartPlot(pruned_parrot, main = "Pruned")

# According to this model, the method of data collection is the most important predictor
# of the observed parrotfish density (perhaps why this data was never published?). 
# If method = 1, then the two biological factors, Coral Richness and Coral Total don't play
# any role.

# make predictions with each tree
parrot_predict <- predict(parrot, test)
pruned_parrot_predict <- predict(pruned_parrot, test)

# estimate errors on the test set with mse
(1/nrow(test))*sum((test$Parrot - parrot_predict)^2)
(1/nrow(test))*sum((test$Parrot - pruned_parrot_predict)^2)

# You can vary the complexity parameter at your discretion. For instance, setting cp = 0
# builds the tree as far as possible (to the point where each node contains 20 or less observations,
# the minimum allowed to attempt a split by default, though you can also vary that). This is 
# quite likely to have very small relative error, but also to have terrible estimated cv error 
# (i.e. likely to perform terribly on any new data)
set.seed(8675309)
parrot2 <- rpart(Parrot~CoralTotal+factor(CoralRichness)+factor(Month)+factor(Station)+factor(Method), data = Bahama , cp = 0)
# the result
fancyRpartPlot(parrot2)

# let's look at the table
parrot2$cptable

# Exercise: Experiment with cp. First create a model with cp = 1. Then try a few values 
# between 0 and 1. 
# Your code here:





#################################END FILE######################################################