#WNS Hackathon code for analyticsvidhya.com

#setting working directory and reading data
setwd('C:\\Users\\G Amit\\Desktop\\hackathons\\wns')
data=read.csv('train_LZdllcl.csv')
test=read.csv('test_2umaH9m.csv')

head(data)
head(test)

#Summary and Basic Exploration of data
summary(data)
str(data)

#checking NA's in the data
sum(is.na(data))

#replacing missing values with mean in data
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))

print(na_count)

#checking mean of a variable
mean(data$previous_year_rating,na.rm = TRUE)

#replacing the variable NA's with mean of variable which is equivalent to 3 3
data$previous_year_rating[is.na(data$previous_year_rating)] <- 3

sum(is.na(data$previous_year_rating))


#replacing missing values in test datset with mean
na_count2 <-sapply(test, function(y) sum(length(which(is.na(y)))))

print(na_count2)

mean(test$previous_year_rating,na.rm = TRUE)

test$previous_year_rating[is.na(test$previous_year_rating)] <- 3

sum(is.na(test$previous_year_rating))


#type conversion of variables
str(data)

data$previous_year_rating=as.factor(data$previous_year_rating)
data$is_promoted=as.factor(data$is_promoted)
data$KPIs_met..80.=as.factor(data$KPIs_met..80.)
data$awards_won.=as.factor(data$awards_won.)

str(test)

test$previous_year_rating=as.factor(test$previous_year_rating)
test$KPIs_met..80.=as.factor(test$KPIs_met..80.)
test$awards_won.=as.factor(test$awards_won.)

#Correlation

#install.packages("corrplot")
head1=c('no_of_trainings','age','length_of_service','avg_training_score')
data2=data[head1]

str(data)
res <- cor(data2)
round(res, 2)

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#### Observation : age and length have high correlation

require(car)
#random forest
library(randomForest)

str(data)

# Fitting model
fit <- randomForest(is_promoted ~ ., data,ntree=300)
summary(fit)
#Predict Output 
predicted=predict(fit,test)

write.csv(predicted,'predict_rf11.csv')
importance(fit)
barplot(fit)


#vif
library(tidyverse)
library(caret)

car::vif(model)
stepVIF(model, threshold = 10, verbose = FALSE)


#confusion matrix
table(data$is_promoted, predict > 0.5)

#logistic regression model
model <- glm (is_promoted ~ .-employee_id, data = data, family = binomial)
summary(model)

predicted <- predict(model,test, type="response")


y_pred_num <- ifelse(predicted > 0.30, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test$is_promoted

#predictions
y_pred

#writing predicted output as csv file
write.csv(y_pred,'predict.csv')


#svm
install.packages('e1071')
require(e1071)
model <- svm(is_promoted~.,data=data,kernel='linear',gamma=0.2,cost=100)

preds <- predict(model,test)
table(preds)

write.csv(y_pred,'predict_svm.csv')

#After submitting predictions Final score in WNS hackathon is 0.4658/1
