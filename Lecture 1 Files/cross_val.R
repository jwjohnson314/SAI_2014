###############################################################
# Introduction to Cross-Validation                            #
# Created by Jeremiah W. Johnson for the                      #
# 2014 UNH Summer Analytics Institute                         #
###############################################################


# Uncomment the next line if you do not have the South Africa dataset loaded
# sa<- read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data", sep=",",head=T,row.names=1)

# Split the data into three blocks. R defaults to sampling columns of dataframes (because of the way that it
# stores dataframes), so we create a new index with as many entries as the number of rows in sa, then sample from
# that
index <- 1:nrow(sa)
trainindex <- sample(index, 300, replace = FALSE)

# Here we're assigning to sa.train the rows of sa indexed by our trainindex. to sa.test, we're assigning all the others
# (the minus indicates 'everything but' to R)
sa.train <- sa[trainindex, ]
sa.nottrain <- sa[-trainindex, ]

nottrainindex <- 1:nrow(sa.nottrain)
cv.index <- sample(nottrainindex, 100, replace = F)

sa.cv <- sa.nottrain[cv.index, ]
sa.test <- sa.nottrain[-cv.index, ]

# Now you have three sets: sa.train, sa.cv, and sa.test
# Create a logistic regression model on sa.train. Call it trainfit
# Your code below:



# predict will make predictions on new data using your model. The type option tells R to return the probability of a 
# positive outcome (i.e. the probability of a 1 - not actually positive in the usual sense of the word, in this case)
# we're going to make predictions on sa.cv.
predictions <- predict(trainfit, sa.cv, type = "response")

# Compare the predictions with the actual values, summing to see how many you got right - run the next command:
sum(abs(sa.cv$chd - predictions) <= 0.5)

# Tweak your model. When you are satisfied, run on your test set and see how you did
# Your code here:





################################################END FILE######################################################
