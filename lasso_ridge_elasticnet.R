####################################################
########## Ridge, Lasso and Elastic-Net Regression
######################################################

rm(list=ls(all=TRUE))
library(tidyverse)
library(caret)
library(glmnet)

data("Boston", package = "MASS")
head(Boston)

set.seed(123)
training.samples <- Boston$medv %>%
createDataPartition(p = 0.8, list = FALSE)
train.data  <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

#predictor variables
x= model.matrix(medv~., train.data)[,-1]
summary(x)

y=train.data$medv

glmnet(x,y,alpha = 1,lambda = NULL)

####################################
### RIDGE REGRESSION
###################################

# Find the best lambda using cross-validation
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 0)

# Display the best lambda value
cv$lambda.min

# Fit the model on the training data
model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)

# Display regression coefficients
coef(model)

# Make predictions on the test data
x.test <- model.matrix(medv ~., test.data)[,-1]
predictions=as.vector(predict(model,x.test))

# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)

#############################################
#### LASSO REGRESSION
########################################

# Find the best lambda using cross-validation
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 1)

# Display the best lambda value
cv$lambda.min

# Fit the model on the training data
model2=glmnet(x,y,alpha = 1,lambda = cv$lambda.min)

# Display regression coefficients
coef(model2)

# Make predictions on the test data
x.test <- model.matrix(medv ~., test.data)[,-1]
predictions=as.vector(predict(model2,x.test))

# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)

########################################
##### Elastic Net
#########################################

# Build the model using the training set
set.seed(123)
model <- train(
  medv ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

# Best tuning parameter
model$bestTune

model$bestTune$lambda

# Coefficient of the final model. You need
# to specify the best lambda
coef(model$finalModel, model$bestTune$lambda)

# Make predictions on the test data
x.test <- model.matrix(medv ~., test.data)[,-1]
predictions <- model %>% predict(x.test)

# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)
 
# The different models performance metrics are comparable.
# Using lasso or elastic net regression set the coefficient of the predictor variable age to zero,
# leading to a simpler model compared to the ridge regression,
# which include all predictor variables.
