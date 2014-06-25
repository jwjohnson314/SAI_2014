###############################################################
# A Logistic Regression Example                               #
# Created by Jeremiah W. Johnson for the                      #
# 2014 UNH Summer Analytics Institute                         #
###############################################################

# Read in the data from statweb website - note that you can use this command to pull data from all over the web
sa<- read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data", sep=",",head=T,row.names=1)

# Take a look at it
head(sa)

# chd stands for coronary heart disease (1 = positive, 0 = negative). Let's build a logistic model.
# glm is short for "generalized linear model." There are many variations on this, the logistic model is just one,
# which is why we include family = "binomial" to tell glm that we want logistic regression
fit <- glm(chd ~ sbp + tobacco + ldl + famhist + obesity + alcohol + age, data = sa, family = "binomial")
summary(fit)

# this plot will help us spot correlations between predictors
pairs(sa[,c(1:3,5,7:9)])

# Try running the model with different choice of predictors and see if you get more logical results
# For example, copy and paste the code above, then take out one of obesity or sbp and run it. What happens?
# Your code here:





##################################################END FILE#########################################################