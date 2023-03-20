#Installing tidyverse

library(tidyverse)
dataset=read.csv('heart.data.csv')

head(dataset)
glimpse(dataset)
length(dataset)
summary(dataset)

#Find out if there are any missing values in the columns
colSums(is.na(dataset))

#Imputing a value in place of the missing column values (biking,smoking, and heart diease)
biking_median=median(dataset$biking, na.rm=TRUE)
dataset$biking=ifelse(is.na(dataset$biking),
                      biking_median,
                      dataset$biking)

smoking_median=median(dataset$smoking, na.rm=TRUE)
dataset$smoking=ifelse(is.na(dataset$smoking),
                       smoking_median,
                       dataset$smoking)

hdisease_median=median(dataset$heart.disease, na.rm=TRUE)
dataset$heart.disease=ifelse(is.na(dataset$heart.disease),
                             hdisease_median,
                             dataset$heart.disease)
colSums(is.na(dataset))

#Split the data into training/testing set
library(caTools)

split=sample.split(dataset$heart.disease, SplitRatio= 0.8) #80% training 80% testing
training_set=subset(dataset, split=TRUE)
testing_set=subset(dataset, split=FALSE)


#Linear regression
MLR=lm(formula=heart.disease~ ., 
       data=training_set)
summary(MLR)
sum=summary(MLR)
MSE=(mean(sum$residuals^2))
paste('Mean Square Error: ', MSE)

#R-Squared value
summary(MLR)
#Obtained R-squared value of 0.9765

#Predicting the data result
y_pred=predict(MLR, newdata=testing_set)
data=data.frame(testing_set$heart.disease, y_pred)
head(data)


#Comparing prediction with actual results
new=read_csv('Heart_validation.csv')
head(new)
new_x=new[c(1:3)]
new_x
data.frame(new[c(3)], predict(MLR,newdata=new_x))
