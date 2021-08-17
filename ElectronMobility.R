tabl <- read.table("ElectronMobility.txt", header=TRUE)
tabl

# 1. Create a scatter plot of the two variables. 
# Describe whether the nature of relationship can be considered linear
# Answer: No, it's not linear. It looks like it is logistic. 
# x = density, y = mobility

splot <- plot(tabl$x, tabl$y)

# 2. Next, split the dataset into two parts; the first part has all the rows 
# where Mobility < 1000, and the second part has the remainder of the rows.

rlt1000 <- tabl[tabl$y<1000,]
rgt1000 <- tabl[tabl$y>1000,]

# 3. Next fit a linear regression model on the first part, where Mobility is the 
# predictor and Density.Ln is the outcome. What is the adjusted R^2? 
# Answer: Adjusted R-squared:  0.9188 

linreg <- lm(x~y, data = rgt1000)
summary(linreg)

# 4. Then, using the predict() function, use this model to predict the value of 
# Density.Ln in the second part of the dataset. Store the predicted values of the 
# outcome in a vector, called predvals.

pred.vals <- predict.lm(linreg, rgt1000)

# 5. Next, compute the errors of prediction, using the observed values (from 
# Density.Ln in the second half of the dataset) and the predicted values of the outcome

error <- rgt1000$x - pred.vals

# 6. Create two bivariate plots: 1) between observed and predicted values (from the 
# second half of the dataset), 2) between predicted values and the errors.

bplot1 <- plot(rgt1000$x, pred.vals)
bplot2 <- plot(pred.vals, error)

# 7. Comment on the plots created in part 6, describing the relationships between observed 
# and predicted values, and between predicted and outcome values.
# Answer: For bplot1: (predicted and observed values) Since the plot shows a pattern curving in in a way that demonstrates higher 
# predicted values than observed values, it might suggest a bias for the predicted values.
# If the predicted and observed were the same, then we would expect the points to ordinate
# linearlly. For bplot2: (Difference between predicted and observed errors) A good error
# distribution would have values as close to zero as possible. That is not the case here,
# which suggests there is a bias. 

# 8. Next, take into account the plot created for addressing part 1, and combine it with 
# the plots created in part 6. Comment on a), whether the pattern observed between the 
# predictor and the outcome is the same throughout the entire range of predictor values;
# (b) how, using a model fit on the first half has produced a certain pattern of errors 
# of prediction made when using the model fit on the first part of the dataset for making 
# predictions on the second part of the dataset.
# Answer: a) No, it's not the same throughout. There pattern for the early part of the
# dataset is different from the rest of it. b) I'm not sure, but I think the model fit
# based on the data split around 1000 contributted to a pattern of errors, because the
# data itself does not have differentiated pattens at 1000. I think having it split
# around ~400 might have fit better. 













