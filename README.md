# Session-16.1-assignment-rahul-sharma
Acagild session 16.1 assignment 

1. Use the below given data set - Data Set

2. Perform the below given activities:

a. Predict the no of comments in next H hrs

Note:-
1. Use LASSO, Elastic Net and Ridge and other regression techniques that are covered in the module

2. Report the training accuracy and test accuracy

3. compare with linear models and report the accuracy

4. create a graph displaying the accuracy of all models

Ans 2 a ->

library(tidyverse)

library(caret)

library(glmnet)

# Load the data

data("Boston", package = "MASS")

# Split the data into training and test set

set.seed(777)

training.samples <- Boston$medv %>%

createDataPartition(p = 0.8, list = FALSE)

train.data  <- Boston[training.samples, ]

test.data <- Boston[-training.samples, ]

glmnet(x, y, alpha = 1, lambda = NULL)

**Compute lasso regression:

# Build the model

set.seed(777)

lasso <- train(

medv ~., data = train.data, method = "glmnet",

trControl = trainControl("cv", number = 5),

tuneGrid = expand.grid(alpha = 1, lambda = lambda))

# Model coefficients

coef(lasso$finalModel, lasso$bestTune$lambda)

# Make predictions

predictions <- lasso %>% predict(test.data)

# Model prediction performance

data.frame(

RMSE = RMSE(predictions, test.data$medv),

Rsquare = R2(predictions, test.data$medv))

**Elastic net regression:

# Build the model

set.seed(777)

elastic <- train(

medv ~., data = train.data, method = "glmnet",

trControl = trainControl("cv", number = 5),

tuneLength = 5

)

# Model coefficients

coef(elastic$finalModel, elastic$bestTune$lambda)

# Make predictions

predictions <- elastic %>% predict(test.data)

# Model prediction performance

data.frame(

RMSE = RMSE(predictions, test.data$medv),

Rsquare = R2(predictions, test.data$medv))

**Compute ridge regression:

set.seed(777)

ridge <- train(

medv ~., data = train.data, method = "glmnet",

trControl = trainControl("cv", number = 5),

tuneGrid = expand.grid(alpha = 0, lambda = lambda))

# Model coefficients

coef(ridge$finalModel, ridge$bestTune$lambda)

# Make predictions

predictions <- ridge %>% predict(test.data)

# Model prediction performance

data.frame(
  
RMSE = RMSE(predictions, test.data$medv),

Rsquare = R2(predictions, test.data$medv))

models <- list(ridge = ridge, lasso = lasso, elastic = elastic)

resamples(models) %>% summary( metric = "RMSE")


*** for graph of lasso

library(glmnet)

set.seed(777)

fit.lasso=glmnet(x,y,alpha=1)

plot(fit.lasso,xvar="lambda",label=TRUE)

*** for graph of ridge

library(glmnet)

set.seed(777)

cv.ridge=cv.glmnet(x,y,alpha=0)

plot(cv.ridge)
