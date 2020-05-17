

library(knitr)
library(tree)
baseball<-read.csv(file.choose())
summary(baseball)
sapply(baseball,function(x)sum(is.na(x)))
#Model Building
tree.fit <- tree(Salary~Hits+Years, data=baseball) #tree is in tree package and by default it takes entropy
summary(tree.fit)
plot(tree.fit)
text(tree.fit)
#The argument pretty=0 instructs R to include the category names 
#for any qualitative predictors
text(tree.fit,pretty =0) ###pretty means it will print categorical value 
tree.fit
##Prediction
yhat <- predict(tree.fit, baseball)
Hit<-data.frame(baseball,yhat)
mean((Hit$Salary-Hit$yhat)^2) ##formula for MSE= mean square error

#Another way Manual way 
Hit$residual<-Hit$Salary - Hit$yhat
Hit$residual1<-(Hit$residual)^2
sum(Hit$residual1)/263
mean((Hit$Salary-Hit$yhat)^2)

### how to increase the Accuracy of the model and decrese the mse 
# log transform Salary to make it a bit more normally distributed
baseball<-read.csv(file.choose())
hist(baseball$Salary)
baseball$Salary <- log(baseball$Salary)
hist(baseball$Salary)
tree.fit <- tree(Salary~Hits+Years, data=baseball)
summary(tree.fit)
plot(tree.fit)
text(tree.fit,pretty =0)
tree.fit
##Prediction
yhat <- predict(tree.fit, baseball)
Hit1<-data.frame(baseball,yhat)
mean((Hit1$Salary-Hit1$yhat)^2)
# Another way of doing analysis 
## Tree Pruning 
# log transform Salary to make it a bit more normally distributed
baseball<-read.csv(file.choose())
hist(baseball$Salary)
baseball$Salary <- log(baseball$Salary)
hist(baseball$Salary)
library(caret)
split <- createDataPartition(y=baseball$Salary, p=0.7, list=FALSE)
train <- baseball[split,]
test <- baseball[-split,]
#Create tree model
trees <- tree(Salary~., data=train)
plot(trees)
text(trees, pretty=0)

##Prediction
yhat <- predict(trees, test)
Hit1<-data.frame(test,yhat)
mean((Hit1$Salary-Hit1$yhat)^2)
#Cross validate to see whether pruning the tree will improve performance
cv.trees <- cv.tree(trees)
plot(cv.trees$size,cv.trees$dev,type="b")
prune.trees <- prune.tree(trees, best=8) #best =8 means 8th node is lowest error
plot(prune.trees)
text(prune.trees, pretty=0)
##Prediction
yhat <- predict(prune.trees, test)
Hit2<-data.frame(test,yhat)
mean((Hit2$Salary-Hit2$yhat)^2)


## Tree Pruning 
# log transform Salary to make it a bit more normally distributed
baseball<-read.csv("F:/R and Data Science/Decision tree/baseball.csv")
hist(baseball$Salary)
baseball$Salary <- log(baseball$Salary)
hist(baseball$Salary)
library(caret)
split <- createDataPartition(y=baseball$Salary, p=0.7, list=FALSE)
train <- baseball[split,]
test <- baseball[-split,]
### Model building including all the variable 
cv.trees2 <- tree(Salary~., data=train)
plot(cv.trees2)
text(cv.trees2, pretty=0)
#Cross validate to see whether pruning the tree will improve performance
cv.trees3 <- cv.tree(cv.trees2)
plot(cv.trees3$size,cv.trees3$dev,type="b")
prune.trees <- prune.tree(cv.trees2, best=4)
plot(prune.trees)
text(prune.trees, pretty=0)
##Prediction
yhat <- predict(prune.trees, test)
Hit3<-data.frame(test,yhat)
mean((Hit3$Salary-Hit3$yhat)^2)

