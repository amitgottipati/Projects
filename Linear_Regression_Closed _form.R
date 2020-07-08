#############################################
########## Linear Regression - Closed Form
#############################################

library(MASS)
str(Boston)

## y = BX + e
## y is vector of response variable
## X is a matrix of feature variables
## B is vector of parameters we want to estimate
## e is the error term

head(Boston)

typeof(Boston)

y = Boston$medv ## target vector

# Matrix of feature variables from Boston
X = as.matrix(Boston[-ncol(Boston)])

# vector of ones with same length as rows in Boston
int <- rep(1, length(y))
install.packages('pacman')
# Add intercept column to X
X <- cbind(int, X)

## β=((X^T*X)^−1)*X^T*y

## Implementing closed form solutions

betas = solve( t(X) %*% X ) %*% t(X) %*% y

# Round for easier viewing
betas <- round(betas, 2)

print(betas)


## Now getting betas from lm model

model = lm(medv ~ . , data = Boston)

model_coeff = round(model$coefficients, 2)

print(model_coeff)

## Comparing betas of linear regression from lm function and closed form using matrix multiplication
results = data.frame(model_coeff, betas)

results












