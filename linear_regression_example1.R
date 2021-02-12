# linear_regression_example1.R
# A script file used in the turtle R workshop.

# Tomo Eguchi
# 28 May 2015


# 1. Import data
datCmCleaned <- read.table("Growth data Nov 2008 cleaned.csv", 
                           header = TRUE,
                           sep = ",")

# look at data:
summary(datCmCleaned)

#2. Build statistical models on mass and length 
#3. Fit the models to the data
lm1 <- lm(log(Weight) ~ SCL, data = datCmCleaned)

#4. Look at the results
# you can look at residual by plotting the output:
plot(lm1)


#5. Plot the data and the best model
