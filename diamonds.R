##################################################################
# Intro to Linear Regression using Diamonds Data for ggplot2     #
# Created by Jeremiah W. Johnson for the                         #
# 2014 UNH Summer Analytics Institute                            #
##################################################################

# uncomment line 9 if you don't have the ggplot2 package installed (you probably don't 
# if you have just installed R for the first time)
#install.packages("ggplot2")

# The library command is used to load files which are contained in a package
# In this case, we're loading ggplot2, a graphics package which includes a dataset called 'diamonds'
library(ggplot2)

# Show the first few lines of the dataset
head(diamonds)

# Creat a plot of price as a function of carat. ggplot2 syntax is hard to get used to, but it makes for nice graphs
plot1 <- ggplot(data = diamonds) + geom_point(aes(x = carat, y = price), color = "green") + labs(x = "Carat", y = "Price")

# Display the plot
plot1

# R also has a 'base plot' package that you can use instead of ggplot
# uncomment the next line to see the plot generated from the base package
# plot(diamonds$carat, diamonds$price, col = "green")

# You can also use the 'qplot' command, which is part of ggplot2, but more like base graphics
# It is quicker and more intuitive initially but gives you less control over the end result. 
# Uncomment the next line to test it out. This time it's colored by clarity
# qplot(data = diamonds, x = carat, y = price, xlab = "Carat", ylab = "Price", color = clarity)

# The plot demonstrates an obvious nonlinear pattern, and variance increases as carat increases. Both are red flags
# for linear regression, but let's fit a line to the data anyway and display summary
fit1 <- lm(diamonds$price~diamonds$carat)
summary(fit1)

# Add the line to the plot
plot1 <- plot1 + geom_abline(intercept = -2256.36, slope = 7756.43, color = "red")
plot1

# Get confidence intervals for the coefficients - the first two commands return the same results
confint(fit1)
confint(fit1, level = 0.95)
confint(fit1, level = 0.01)

# Confidence interval for just the slope
confint(fit1, "diamonds$carat")

# Diagnostic Plots
# Residuals vs. Fitted tells us whether errors are normally distributed around 0, clearly not true here!
# Normal QQ Plot also tells us whether errors are normally distributed. If so, they lie on a straight line
# Scale-Location tells us the same thing as Residuals vs. Fitted and should also display no pattern
# Residuals vs Leverage draws attention to points whose inclusion may affect the regression relationship
# any numbered value (27416), for example, has high leverage and may warrant an inspection
par(mfrow = c(2,2))
plot(fit1)

# Perhaps a transform is order
logdiamonds <- data.frame(log(diamonds$carat), log(diamonds$price))

# Change the default column names
names(logdiamonds) <- c("logCarat", "logPrice")

# Create a plot of the tranformed data and display it
plot2 <- ggplot(data = logdiamonds) + geom_point(aes(x = logCarat, y = logPrice), color = "green")
plot2

# Look at diamond 27416
diamonds[27416, ]

# Compare w/ Summary Stats - it has the highest carat
summary(diamonds)

# Not many diamonds with carat > 1
big <- diamonds[diamonds$carat >= 1,]
rbig <- diamonds[diamonds$carat >= 2,]
rrbig <- diamonds[diamonds$carat >= 3,]
rrrbig <- diamonds[diamonds$carat >= 4,]
rrrbig

# Should these be excluded from your model? It's a judgement call. 
# Exercise: Uncomment the next snippet to produce the logged dataset with carat > 4 diamonds removed
# Then modify the code above to fit a linear model. How does it compare?
# logdiamonds2 <- logdiamonds[-which(diamonds$carat >= 4),]

# Your code here:






# See http://solomonmessing.wordpress.com/2014/01/19/visualization-series-the-scatterplot-or-how-to-use-data-so-you-dont-get-ripped-off/
# for more background on diamond pricing using R and more plots, (it is the source of today's case study data) 

# I've saved the diamond file on Blackboard in case you'd like to work with it in another program,  
# You can also try uncommenting the following command  and filling in a filepath to save it in an easily accessible
# location (for example, on a mac you might use "~/Desktop/diamonds.csv", on Windows, "C:\Users\Public\Desktop...")
# This will be good practice for the future

# write.csv(diamonds, file = "put the location you would like the file to be at here")

###################################END FILE#######################################################