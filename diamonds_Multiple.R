###########################################################################
# Intro to Multiple Linear Regression using Diamonds Data for ggplot2     #
# Created by Jeremiah W. Johnson for the                                  #
# 2014 UNH Summer Analytics Institute                                     #
###########################################################################


# Note: This file follows diamonds.R and assumes you've already loaded ggplot2. If not,
# you'll need to load it before proceeding

# Set the contrast method as treatment (dummy variable); default is polynomial
options(contrasts=c("contr.treatment","contr.treatment"))

# This time we'll regress price on carat, cut, clarity, and color
fit2 <- lm(diamonds$price~diamonds$carat+diamonds$clarity + diamonds$cut + diamonds$color)
summary(fit2)

# Let's transform 'price' and carat' using log
# These commands add columns containing logged price and carat resp., to the diamond dataframe
diamonds$logPrice <- log(diamonds$price)
diamonds$logCarat <- log(diamonds$carat)

# Run it again with the logged price and carat variables
fit3 <- lm(diamonds$logPrice~diamonds$logCarat + diamonds$clarity  + diamonds$cut + diamonds$color)
summary(fit3)

# Here are some plots in which to look for correlations. If we didn't already know what to expect, 
# we would do this first instead of last
# Uncomment line 31 and run it if you haven't installed GGally (again, you probably haven't if this is
# your first day using R)
# install.packages("GGally")
library(GGally)

# The syntax below tells R to create the plot(s) using columns 1 - 4 and column 7 of the diamonds dataframe
ggpairs(diamonds[, c(1:4,7)])




##########################################END FILE##########################################################