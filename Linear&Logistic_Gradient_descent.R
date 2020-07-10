########################################
######### Linear Regression
######################################
rm(list=ls(all=TRUE))

data = read.csv("C:\\Users\\19726\\Desktop\\sgemm_product_dataset\\sgemm_product.csv")

sum(is.na(data))

str(data)

data$avg=rowMeans(data[,15:18])

head(data)

## finding correlation between the numerical variables
library(Hmisc)
library(corrplot)

data_corr=rcorr(as.matrix(data[,15:19]))
data.coeff = data_corr$r
data.coeff
corrplot(as.matrix(data.coeff))

## We see that all the runtimes are highly correlated to the average run times so it is better to remove these variables 
## for predicting the average run time

## removing the 4 different run times
data$Run1..ms.=NULL
data$Run2..ms.=NULL
data$Run3..ms.=NULL
data$Run4..ms.=NULL

# Performing Goodman-Kruskal test for ordinal variables for checking multicollinearity in the data
# Goodman and Kruskal’s τ(tau) is a measure of association between two variables, x and y
# K is the number of levels for each variable

library(GoodmanKruskal)
categorical_var_names=c("MWG","NWG","KWG","MDIMC","NDIMC","MDIMA","NDIMB","KWI","VWM","VWN","STRM","STRN","SA","SB")
categorical_data = subset(data, select = categorical_var_names)
GKmatrix= GKtauDataframe(categorical_data)
plot(GKmatrix, corrColors = "blue")

## we see no strong associations between any variables, 
## hence there is no Multicollinearity in the independent variables

# library(tidyverse)
# library(ggplot2)
# str(data)
# 
# ggplot(data, aes(MWG, avg, color=MWG)) +
#     geom_jitter(height = 0) + 
#     ggtitle("Average Run Time vs. MWG ")
# 
# ggplot(data, aes( SA,avg, color=SA)) +
#     geom_jitter(height = 0) + 
#     ggtitle("Average Run Time vs. SA ")



## checking distribution and looking for skew in the data

library(ggpubr)
ggdensity(data$avg, 
          main = "Density plot of run times",
          xlab = "run")
 
#log transformation for skewed data
data$avg=log(data$avg)

#looking for the skew after the log transformations
ggdensity(data$avg,
          main = "Density plot of run times",
          xlab = "run")

str(data)
names(data)
# Checking for outliers in the data
par(mfrow=c(4,4))
boxplot(avg ~ MWG , data, main="average run times over MWG")
boxplot(avg ~ NWG , data, main="average run times over NWG")
boxplot(avg ~ KWG , data, main="average run times over KWG")
boxplot(avg ~ MDIMC , data, main="average run times over MDIMC")
boxplot(avg ~ NDIMC , data, main="average run times over NDIMC")
boxplot(avg ~ MDIMA , data, main="average run times over MDIMA")
boxplot(avg ~ NDIMB , data, main="average run times over NDIMB")
boxplot(avg ~ KWI , data, main="average run times over KWI")
boxplot(avg ~ VWM , data, main="average run times over VWM")
boxplot(avg ~ VWN , data, main="average run times over VWN")
boxplot(avg ~ STRM , data, main="average run times over STRM")
boxplot(avg ~ STRN , data, main="average run times over STRN")
boxplot(avg ~ SA , data, main="average run times over SA")
boxplot(avg ~ SB , data, main="average run times over SB")
boxplot(data$avg, main="average run times")
par(mfrow=c(1,1))

## Normalizing the data
for (i in 1:15) {
    data[,i] <- (data[,i] - mean(data[,i])) / sd(data[,i])
}

str(data)

## splitting the data set into test and train
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(data), size = floor(.70*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

head(train)

x=as.matrix(train[,1:14])
x
X=cbind(rep(1,nrow(x)),x)
X
y=c(train$avg)
y
m=length(y)

beta=c(rep(0,15))
beta


## cost function
Mean_squared_error_cost_function=function(X, y, beta){
                                                      cost = sum((X%*%beta- y)^2)/(2*m)
                                                      return(cost)
                                                      }

## gradient descent

gradDescent=function(X, y, beta, alpha, iterations){
    
    m = length(y)
    #cost_hist = rep(0, num_iters)
    cost_hist=list()
    for(i in 1:iterations) {
    
                            beta = beta - alpha*(1/m)*(t(X)%*%(X%*%beta - y))

                            cost_hist[i]  = Mean_squared_error_cost_function(X, y, beta)
                            
                            
                            if((i>=2) && (cost_hist[[i-1]] - cost_hist[[i]]<0.0001) ){
                               
                              break
                              }
                            
                            }

    results=list(beta, cost_hist)
    return(results)
    
}

## creating the test dataset
average_test=test$avg
test$avg  = NULL
test_matrix=as.matrix(test)
test_matrix=cbind(rep(1,nrow(test_matrix)),test_matrix)

##setting the hyperparamters
alpha = 0.01
iterations= 10000

results = gradDescent(X, y, beta, alpha, iterations)

beta_results = results[[1]]
cost_results = results[[2]]

print(beta_results)
min(unlist(list(cost_results)))
length(cost_results)

plot(1:length(cost_results),cost_results,type="b",col="blue",lwd=1,cex.lab=1.5,ylab = "Cost",xlab="iterations",main = "Alpha = 0.1")

betas_matrix=as.matrix(beta_results)
betas_matrix

## Predicting on the test dataset
y_preds = test_matrix%*%(betas_matrix)

## cost of test error
cost_test=sum((y_preds - average_test)^2)/(2*length(average_test))
cost_test

error = y_preds - average_test
error

head(y_preds)
head((average_test))

## For checking homoskedasticty we plot our residuals against our predicted values
plot(y_preds,error,main = "Residuals vs Predicted values for Alpha =1.2")
## we see that there is no constant variance.

ggqqplot(error, main="Q-Q plot for Alpha = 1.2")
## we can see from the Q-Q plot that the residuals are not normally distributed as they dont follow along the line

## plotting Train vs Test error

threshold=c(10,1,0.01,0.001,0.0001,0.00001,0.0000001,0.0000000001)
train_error =c(0.4103538,0.4103538,0.2819965,0.2279243, 0.220464,0.2196875,0.2196078,0.219607)
test_error = c(0.4097237,0.4097237,0.2812822,0.2271263,0.2197232,0.2189735,0.2189042,0.2189043)

df.cost_threshold = data.frame(threshold , train_error, test_error)

require("reshape")
require("ggplot2")

mdf <- melt(df.cost_threshold, id="threshold")  # convert to long format
mdf

## plotting train vs test error for different thresholds at same alpha
ggplot(mdf, aes(x=threshold, y=value, colour=variable)) +
    geom_line() + 
    theme_bw()

######################################
## experiment 3
## selecting 8 random parameters to train our linear regression model

names(data)

rv=c("MWG","NWG","MDIMA","NDIMB","VWM","VWN","SA","SB","avg")
data_rv=data[,rv]
head(data_rv)

## splitting the data set into test and train
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample_rv <- sample.int(n = nrow(data_rv), size = floor(.70*nrow(data_rv)), replace = F)
train_rv <- data_rv[sample_rv, ]
test_rv  <- data_rv[-sample_rv, ]

head(train_rv)
head(test_rv)

x=as.matrix(train_rv[,1:8])
head(x)
X=cbind(rep(1,nrow(x)),x)
X
y=c(train_rv$avg)
y
m=length(y)

beta=c(rep(0,9))
beta

average_test=test_rv$avg
test_rv$avg  = NULL
test_matrix=as.matrix(test_rv)
test_matrix=cbind(rep(1,nrow(test_matrix)),test_matrix)

alpha = 1
iterations= 10000

results = gradDescent(X, y, beta, alpha, iterations)

beta_results = results[[1]]
cost_results = results[[2]]

print(beta_results)
min(unlist(list(cost_results)))
length(cost_results)

plot(1:length(cost_results),cost_results,type="b",col="blue",lwd=1,cex.lab=1.5,ylab = "Cost",xlab="iterations",main = "Alpha = 1")

betas_matrix=as.matrix(beta_results)
betas_matrix

## Predicting on the test dataset
y_preds = test_matrix%*%(betas_matrix)

## cost of test error
cost_test=sum((y_preds - average_test)^2)/(2*length(average_test))
cost_test

error = y_preds - average_test
error

#######################################################
## experiment 4
## selecting 8 significant parameters to train our linear regression model

names(data)

sv=c("MWG","NWG","KWG","KWI","STRM","STRN","SA","SB","avg")
data_sv=data[,sv]
head(data_sv)

## splitting the data set into test and train
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample_sv <- sample.int(n = nrow(data_sv), size = floor(.70*nrow(data_sv)), replace = F)
train_sv <- data_sv[sample_sv, ]
test_sv  <- data_sv[-sample_sv, ]

head(train_sv)
head(test_sv)

x=as.matrix(train_sv[,1:8])
head(x)
X=cbind(rep(1,nrow(x)),x)
X
y=c(train_sv$avg)
y
m=length(y)

beta=c(rep(0,9))
beta

average_test=test_sv$avg
test_sv$avg  = NULL
test_matrix=as.matrix(test_sv)
test_matrix=cbind(rep(1,nrow(test_matrix)),test_matrix)

alpha = 1
iterations= 10000

results = gradDescent(X, y, beta, alpha, iterations)

beta_results = results[[1]]
cost_results = results[[2]]

print(beta_results)
min(unlist(list(cost_results)))
length(cost_results)

plot(1:length(cost_results),cost_results,type="b",col="blue",lwd=1,cex.lab=1.5,ylab = "Cost",xlab="iterations",main = "Alpha = 1")

betas_matrix=as.matrix(beta_results)
betas_matrix

## Predicting on the test dataset
y_preds = test_matrix%*%(betas_matrix)

## cost of test error
cost_test=sum((y_preds - average_test)^2)/(2*length(average_test))
cost_test

error = y_preds - average_test
error

###########################
############################
## logistic regression
#################################
rm(list=ls(all=TRUE))

data2 = read.csv("C:\\Users\\19726\\Desktop\\sgemm_product_dataset\\sgemm_product.csv")

str(data2)

data2$avg=rowMeans(data2[,15:18])

head(data2)

data2$Run1..ms.=NULL
data2$Run2..ms.=NULL
data2$Run3..ms.=NULL
data2$Run4..ms.=NULL

require(reshape2)

df.m <- melt(data2, id.var = "avg")
head(df.m)

require(ggplot2)
ggplot(data = df.m, aes(x=variable, y=value)) +
    geom_boxplot(aes(fill=avg))


## Normalizing the data2
for (i in 1:15) {
    data2[,i] <- (data2[,i] - mean(data2[,i])) / sd(data2[,i])
}

##converting our target variable to binary
data2$avg = ifelse(data2$avg>median(data2$avg),1,0)

head(data2)

## splitting the data2 set into test2 and train2
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data2 as sample from total 'n' rows of the data2  
sample <- sample.int(n = nrow(data2), size = floor(.70*nrow(data2)), replace = F)
train2 <- data2[sample, ]
test2  <- data2[-sample, ]
head(data2)

x=as.matrix(train2[,1:14])
x
X=cbind(rep(1,nrow(x)),x)
X
y=c(train2$avg)
y
m=length(y)

beta=c(rep(0,15))
beta

## cost function
Mean_squared_error_cost_function=function(X, y, beta){
    yhat=1/(1+exp(-(X%*%beta)))
    cost = sum((yhat- y)^2)/(2*m)
    return(cost)
}

## gradient descent

gradDescent=function(X, y, beta, alpha, iterations){
    
    m = length(y)
    #cost_hist = rep(0, num_iters)
    cost_hist=list()
    for(i in 1:iterations) {
        yhat=1/(1+exp(-(X%*%beta)))
        
        beta = beta - alpha*(1/m)*(t(X)%*%(yhat - y))
        
        cost_hist[i]  = Mean_squared_error_cost_function(X, y, beta)
        
        
        if((i>=2) && (cost_hist[[i-1]] - cost_hist[[i]]<0.0001) ){
            
            break
        }
        
    }
    
    results=list(beta, cost_hist)
    return(results)
    
}

## Predicting on the test2 data2set
average_test2=test2$avg
test2$avg  = NULL
test2_matrix=as.matrix(test2)
test2_matrix=cbind(rep(1,nrow(test2_matrix)),test2_matrix)

## hyperparameters
alpha = 1
iterations= 10000

results = gradDescent(X, y, beta, alpha, iterations)

beta_results = results[[1]]
cost_results = results[[2]]

print(beta_results)
min(unlist(list(cost_results)))
length(cost_results)
plot(1:length(cost_results),cost_results,type="b",col="blue",lwd=1,cex.lab=1.5,ylab = "Cost",xlab="iterations", main = "Alpha = 5")

betas_matrix=as.matrix(beta_results)
betas_matrix
dim(betas_matrix)

##predicting using logistic
y_preds = 1/(1+exp(-(test2_matrix%*%betas_matrix)))
head(y_preds)

## cost of test2 error
cost_test2=sum((y_preds - average_test2)^2)/(2*length(average_test2))
cost_test2

error = y_preds - average_test2

library(caret)
confusionMatrix(as.factor(ifelse(y_preds>0.5,'1','0')),as.factor(average_test2))  

######################################################
## experiment 2 plot
## plotting Train vs Test error

threshold=c(0.0000000000001,0.00000000001,0.000000001,0.00001,	0.0001,	0.001,	0.1)
train_error =c(0.07310571,	0.07310571,	0.07310571,	0.07313724,	0.07334027,	0.07447203,	0.07703253)
test_error = c(0.07291524,0.07291524,0.07291524,0.07294496,0.0731444,0.07425548,0.07675461)

df.cost_threshold = data.frame(threshold , train_error, test_error)

require("reshape")
require("ggplot2")

mdf <- melt(df.cost_threshold, id="threshold")  # convert to long format
mdf


## plotting train vs test error for different thresholds at same alpha
ggplot(mdf, aes(x=threshold, y=value, colour=variable)) +
    geom_line() + 
    theme_bw()



####################
#### experiment 3
####################

rv=c("MWG","NWG","MDIMA","NDIMB","VWM","VWN","SA","SB","avg")
data_rv=data2[,rv]
head(data_rv)

## splitting the data set into test and train
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample_rv <- sample.int(n = nrow(data_rv), size = floor(.70*nrow(data_rv)), replace = F)
train_rv <- data_rv[sample_rv, ]
test_rv  <- data_rv[-sample_rv, ]

head(train_rv)
head(test_rv)

x=as.matrix(train_rv[,1:8])
head(x)
X=cbind(rep(1,nrow(x)),x)
X
y=c(train_rv$avg)
y
m=length(y)


beta=c(rep(0,9))
beta

average_test=test_rv$avg
test_rv$avg  = NULL
test_matrix=as.matrix(test_rv)
test_matrix=cbind(rep(1,nrow(test_matrix)),test_matrix)


alpha = 5
iterations= 10000

results = gradDescent(X, y, beta, alpha, iterations)

beta_results = results[[1]]
cost_results = results[[2]]

print(beta_results)
min(unlist(list(cost_results)))
length(cost_results)

plot(1:length(cost_results),cost_results,type="b",col="blue",lwd=1,cex.lab=1.5,ylab = "Cost",xlab="iterations",main = "Alpha = 1")

betas_matrix=as.matrix(beta_results)
betas_matrix

## Predicting on the test dataset
y_preds = test_matrix%*%(betas_matrix)

## cost of test error
cost_test=sum((y_preds - average_test)^2)/(2*length(average_test))
cost_test

library(caret)
confusionMatrix(as.factor(ifelse(y_preds>0.5,'1','0')),as.factor(average_test))  

################################
####experiment 4
##################

sv=c("MWG","NWG","KWG","KWI","STRM","STRN","SA","SB","avg")
data_sv=data2[,sv]
head(data_sv)

## splitting the data set into test and train
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample_sv <- sample.int(n = nrow(data_sv), size = floor(.70*nrow(data_sv)), replace = F)
train_sv <- data_sv[sample_sv, ]
test_sv  <- data_sv[-sample_sv, ]

head(train_sv)
head(test_sv)

x=as.matrix(train_sv[,1:8])
head(x)
X=cbind(rep(1,nrow(x)),x)
X
y=c(train_sv$avg)
y
m=length(y)

beta=c(rep(0,9))
beta

average_test=test_sv$avg
test_sv$avg  = NULL
test_matrix=as.matrix(test_sv)
test_matrix=cbind(rep(1,nrow(test_matrix)),test_matrix)

alpha = 5
iterations= 10000

results = gradDescent(X, y, beta, alpha, iterations)

beta_results = results[[1]]
cost_results = results[[2]]

print(beta_results)
min(unlist(list(cost_results)))
length(cost_results)

plot(1:length(cost_results),cost_results,type="b",col="blue",lwd=1,cex.lab=1.5,ylab = "Cost",xlab="iterations",main = "Alpha = 1")

betas_matrix=as.matrix(beta_results)
betas_matrix

## Predicting on the test dataset
y_preds = test_matrix%*%(betas_matrix)

## cost of test error
cost_test=sum((y_preds - average_test)^2)/(2*length(average_test))
cost_test


