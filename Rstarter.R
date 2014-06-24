#######################################################
# This is a starter script to introduce R             #
# Created by Jeremiah W. Johnson for the              #
# 2014 UNH Summer Analytics Institute                 #
#######################################################

# Arithmetic
4 + 5*6

# variable assignment
x <- 2
x

# R likes to think of things as vectors - when we entered x <- 2 above, R created a vector with one entry
x <- c(1,2,3) # c is short for 'concatenate'. This creates a vector of length 3 containing the entries 1, 2, 3
x
length(x)

# arithmetic with vectors
y <- c(-1, 0, 1)
x+y
x^2
x^2 - y^2

# data frames are structures used to store data. R thinks of a data frame as a vector of column vectors
# (for the experts: that's not technically correct, a data frame is actually a list of vectors)
df <- data.frame(x,y)
head(df)
names(df)

# Data frames have names associated with each column, and you can change those
# Rename the columns
names(df) <- c("Frogs", "Squirrels")

# Did it work? Let's look at the first few lines
head(df)

# Extract a column - the $ tells R to select the Frogs column of the data frame df
df$Frogs

# Extract a particular entry
df[2,2]

# Extract a row
df[2,]

# Create a new column and append it to df
Leopards <- c(3, 4, 5)
df$Leopards <- Leopards # Note that df doesn't yet have a Leopards column

head(df)

# Summary stats
summary(df)

# An extraction using a logical statement
# First, look at this command - it identifies the position of the squirrel entries in the data frame df that are equal to 0 
which(df$Squirrels == 0)

# Putting a minus sign in front of that command and using it as an index for df tells R to remove those rows
# We then assign the result to what I've imaginatively called df_nozeros
df_nozeros <- df[-which(df$Squirrels == 0),]

# A slightly bigger example - set.seed insures we all get the same results
set.seed(45); x <- rnorm(100)
set.seed(45); y <- runif(100)
set.seed(45); z <- rnorm(100, mean = 4, sd = 2)

# Exercise: create a data frame df with these three columns, then remove all rows in which the entry in column z is bigger than 4
# Save these in a data frame called df4
# The first few rows should look like this

# x         y        z
# 2 -0.7033403 0.3175366 2.593319
# 3 -0.3795377 0.2409218 3.240925
# 4 -0.7460474 0.3784141 2.507905
# 5 -0.8981073 0.3521443 2.203785
# 6 -0.3347941 0.2977585 3.330412
# 7 -0.5013782 0.2278194 2.997244

# Your code below: