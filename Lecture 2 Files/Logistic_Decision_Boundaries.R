############################################################
# Logistic Regression and Decision Boundaries              #
# Created by Jeremiah W. Johnson for the                   #
# 2014 UNH Summer Analytics Institute                      #
############################################################


# This file contains the code to generate a plot of a logistic regression 
# decision boundary on some synthetic data. Needs ggplot2 - uncomment next line if not already loaded
# library(ggplot2)

# load colorblind-friendly palette that includes black
cbbP <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Before we start on the pictures, let's compare probability, odds, and log-odds
# seq creates a sequence of numbers; in this case from 0 to 1 in steps of size 0.1  
prob <- seq(from = 0, to = 1, by = 0.1)
odds <- prob/(1-prob)
logodds <- log(odds)

# Let's put the values into a dataframe and display them
oddsdf <- data.frame(prob, odds, logodds)
oddsdf

# synthetic data for visualization - 4 vectors of 100 values each from normal 
# distributions with means and sds as indicated
x <- rnorm(100, mean = 5, sd = 2)
y <- rnorm(100, mean = 1, sd = 2)
x1 <- rnorm(100, mean = 2, sd = 3)
y1 <- rnorm(100, mean = 5, sd = 2)

# layer one initializes the plot object and then plots points by pairing up x's and y's from above vectors
# the cbind command makes a matrix by combining columns. ggplot prefers dataframes, though, so we make it one.
# x,y's are colored gold
p1 <- ggplot(data = data.frame(cbind(x,y))) + geom_point(aes(x = x, y = y), color = cbbP[2], size = 4)

# Uncomment and run the next line if you'd like to see the partially constructed picture
# p1

# layer two adds points by pairing up x1's and y1's, but colors them blue
p1 <- p1 + geom_point(data = data.frame(cbind(x1, y1)), aes(x = x1, y = y1), color = cbbP[3], size = 4)

#Take a look
p1

# combine prev. data into a single data frame w/ two columns
xbig <- c(x, x1)
ybig <- c(y, y1)
df <- data.frame(xbig, ybig)

# This adds a response column, which is simply 1 for the first 100 rows and 0 for the second 100 rows
df$response <- c(rep(1,100), rep(0,100))

# fit a logistic model
fit <- glm(response~xbig+ybig, data = df, family = "binomial")

# summary info
summary(fit)

# coefficients 
coef(fit)

# This layer adds a line where the log-odds = 0. Points above the line were given negative log-odds by our
# model and points below the line were given positive log-odds. This line is the 'decision boundary.'
p1 <- p1+geom_abline(slope = -coef(fit)[2]/coef(fit)[3], intercept = -coef(fit)[1]/coef(fit)[3], color = cbbP[6])
p1


# by not specifying type = "response" below, the predict function returns log-odds instead of probabilities.
# positive log-odds mean probability > 50%, negative log-odds mean probability < 50%
# I've created a new column in the dataframe to store my predictions. 
df$prediction <- predict(fit, df)

# Let's see a few of those we missed - the ones our model gave positive log-odds when it should have given negative log-odds
mistakes <- which(df$response == 1 & df$prediction < 0)

# Add a layer circling those points
p1 <- p1 + geom_point(data = df[mistakes,], aes(x = xbig, y = ybig), color = cbbP[1], size = 8, shape = 1)
p1

# Exercise 1: Add a layer to p1 which circles the points that we missed on the other side of the line.
# Your code here:

# Read in the South African heart data from statweb website - note that you can use this command to pull data from all over the web
sa<- read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data", sep=",",head=T,row.names=1)

# Here is what a plot for the sa data looks like with factors. The jitter option spreads the points out so they don't overlap
saplot <- ggplot(data = sa) + geom_point(aes(x = famhist, y = age), color = cbbP[factor(sa$chd+2)], size = 6, position = "jitter")
saplot
fit <- glm(chd~famhist+age, data= sa, family = "binomial")
saplot+geom_abline(slope = -coef(fit)[2]/coef(fit)[3], intercept = -coef(fit)[1]/coef(fit)[3], color = cbbP[6])

# Exercise 2 (optional): Can you create a similar plot for the SA heart data? (remember that in order to be
# able to plot in two dimensions the number of predictor variables must be 2 or less. Try it with nonfactors as 
# predictors).
# Your code here:





###########################################END FILE########################################################