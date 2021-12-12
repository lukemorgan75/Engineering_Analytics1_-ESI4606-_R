# Combined Lecture Notes For Engineering Analytics 1 (ESI 4606) Fall 2021
# Luke H. Morgan
################################################################################

# Lecture 2 - ESI 4606 - 8/30/2021
# draw a pie chart
# movie example
pie(x=c(2,4,2),labels=c("Sci-Fi","Action","Romance"))
help(pie)

# Credit data example
slice=table(Credit$Ethnicity)
slice
group=names(table(Credit$Ethnicity))
pie(x=slice,labels=group)

#draw bar plot
barplot(height = c(2,4,2),names.arg=c("Sci-Fi","Action","Romance"))
#now with redit data
slice=table(Credit$Ethnicity)
barplot(slice,ylim=c(0,300),main = "Bar plot of Ethnicity")

#Lecture 2, 9.1.2021
#Draw a Histogram
score=c(93,65,84,71,74.5,99,85,81,83,81,95.5,87.5)
#Draw a histogram with frequency
hist(score,breaks=4,col = "cyan")
#draw histogram with relative frequency
h=hist(score,breaks=4, col="green")
h$counts=h$counts/sum(h$counts)
plot(h,ylab = "Relative Frequency", col = "aliceblue")

#draw a boxplot for credit data (variable: income)
boxplot(Credit$Income, horizontal = TRUE, col= "goldenrod")
names(Credit)
boxplot(Income~Gender+Student,data=Credit)

# draw a scatter plot between income vs credit limit
names(Credit)
plot(Credit$Income,Credit$Limit,xlab="Income",ylab="Credit Limit",pch = 19)

# Numerical Summary, using aggregate quantities (max,min,avg,etc.)
# CALCULATE SAMPLE MEAN
score=c(93,65,84,71,74.5,99,85,81,83,81,95.5,87.5)
#way 1: use function directly, usually the most efficient
mean(score)
#way 2: using functions as building blocks
build.mean.score=sum(score)/length(score)
print(build.mean.score)
#way 3: start from scratch, using code (logic code)
total=0
for(i in 1:12)
{
  total=total+score[i]
}
total/12

# Find Median for the score data
#way1: median function
median(score)
#More generic function:
quantile(score,probs=.5)


# Luke's post-lecture Practice:
fac.1=factor(c(1))
fac.1
data.frame75=(c(score,fac.1))
data.frame75
mat.75=matrix(data = 1)
mat.75
dframe1=data.frame(c(data.frame75, mat.75))
dframe1

score
dframe2=data.frame(score, score/score)
dframe2
Sum(dframe2.score/score)
sum(score/score) # this works for counting a column of numeric non-zero values 


# 9/8/2021 (Lecture 2 cont)
# ...numerical summary...

# calculate sample variance and standard deviation
score=c(93,65,84,71,74.5,99,85,81,83,81,95.5,87.5)
var(score)
sd(score)
# Alternatively, 
sqrt(sum((score-mean(score))^2)/(12-1))
# multiple lines
total.square=sum((score-mean(score))^2) # computing total sum of squares
variance=total.square/(12-1) #compute variance
std=sqrt(variance)
variance
std

###############################################################################

#Lecture 3

#9/13/2021 (Monday)

# Binomial, X ~Binomial(n=10,p=0.3)
#calculate pmf, Pr(X=x), x=0,1,2,3,4,...,10
x=seq(0,10,1)
pmf=dbinom(x,size=10,prob=.3)
plot(x,pmf,pch=16)

#calculate the cdf values, Pr(X<=x), e.g., Pr(X<=5)
pbinom(q=5,size=10,prob = 0.3)
pbinom(q=x, size=10,prob=.3)

#generating a realization from a binomial r.v. 
set.seed(123) # allows reproduction of same random result
rbinom(n=10,size=10,prob=.3)

# Normal Distribution, X~N(Mean=0.2508, variance = 0.0005^2)
# Compute Pr(0.2485<=X<=0.2515)=Pr(X<=0.2515)-Pr(X<0.2485)
# =
# Recall...
pnorm(0.2515,mean = 0.2508,sd = 0.0005)  #F(0.2515)=Pr(X<=0.2515)
pnorm(0.2485,mean = 0.2508,sd = 0.0005)
pnorm(0.2515,mean = 0.2508,sd = 0.0005)-pnorm(0.2485,mean = 0.2508,sd = 0.0005)
help(pnorm)
rnorm(100,mean=0,sd=1) # generating 100 realization from N(0,1)
qnorm(0.95) # z_alpha, where alpha = 0.05 in z-table
qnorm(0.05,lower.tail = FALSE) # Checking z-table
# you can do this for the other distributions as well, don't forget that you can use the help command for info

#end of class period 9/13/2021

###############################################################################

# Lecture 4, ESI 4606
# 9/15/2021
# {Using Advertising data set}
# hand calculation of simple linear regression
# Use R for intermediate steps.
mean.TV=sum(Advertising$TV)/200          # xbar
mean.sales=sum(Advertising$sales)/200       # ybar
a=sum((Advertising$TV-mean.TV)*(Advertising$sales-mean.sales)) # a is the numerator of the beta1 hat formula, next we will form the denominator and then perform the division on their nicknames
a
b=sum((Advertising$TV-mean.TV)^2)
beta1.hat=a/b

beta0.hat=mean.sales-mean.TV*beta1.hat
beta0.hat

sum((Advertising$sales-(beta0.hat+beta1.hat*Advertising$TV))^2)/(200-2) # sample variance

# 9/20/2021 (Monday)

# Using R function to build simple linear regression model
# for analyzing advertising data with output: sales, input: TV
# Build simple linear regression model
SimpleModel=lm(sales~TV,data=Advertising)
summary(SimpleModel)
help(lm)
coef(SimpleModel) # approximates results according to Least Squares (LS) Method

# critical value for t_0.025,198
qt(0.975,198)
qt(0.025,198,lower.tail=FALSE)

# compute p-value Pr(T<=17.6676), Where T~t(df=198)
pt(17.6676,198)
# based on alpha=0.05, [pvalue: 0] < [alpha: 0.05] ---> reject H0
help(pt)

confint(SimpleModel)

# 9/22/2021 Wednesday

# make prediction, TV = 100, 2000, 5000
predict(SimpleModel,newdata = data.frame(TV=c(100,2000,5000)))
# Plot data and fitted model
plot(Advertising$TV,Advertising$sales,xlab= "TV", ylab="Sales")
abline(SimpleModel,lwd=3)
#end lec4

###############################################################################

# Lecture 5 - Multiple Linear Regression
#9/22/2021

# Using R function to build multiple linear regression model
# for analyzing advertising data with output: sales, input: TV, Radio
# Build multiple linear Regression Model
MultipleModel=lm(sales~TV+radio+newspaper,data=Advertising)
summary(MultipleModel)

# create a data set by removing irrelevant columns, e.g., x
AdData=Advertising[,-1] # removes the first column
MultipleModel2=lm(sales~.,data=AdData)

# 9/27/2021 (Monday)
# Multiple Linear Regression by handling qualitative predictors
names(Credit)
Model=lm(Balance~Income+Gender+Ethnicity,data=Credit)
summary(Model)
# "-24.3396" means average credit balance difference male and female (baseline)
# examining the dummy variable coding
contrasts(as.factor(Credit$Gender))
# "254.3687" estimate beta0: means the average credit card balance of a 
#  female African American individual in the absence of the influence of income.

# Lecture 5 cont.   9/29/2021

#Interaction Term
names(Advertising)
model1=lm(sales~TV*radio*newspaper,data=Advertising)
summary(model1)
model2=lm(sales~TV+radio+TV:radio,data=Advertising)
summary(model2)

# Nonlinear transformation
library(ISLR)
search()
# mpg = beta0 +beta1*HP
lm.fit1=lm(mpg~horsepower,data=AutoH3)
summary(lm.fit1)
# mpg = beta0 + beta1*HP + beta2*HP^2
lm.fit2=lm(mpg~horsepower+I(horsepower^2),data=AutoH3)
summary(lm.fit2)
# plot
x=seq(1,250,1)
pred1=predict(lm.fit1,newdata = data.frame(horsepower=x))
pred2=predict(lm.fit2,newdata = data.frame(horsepower=x))
plot(AutoH3$horsepower,AutoH3$mpg,xlab="Horsepower",ylab="MPG")
lines(x,pred1,lwd=2)
lines(x,pred2,col="red",lwd=2)
#end Lec5

###############################################################################

#ESI 4606 Lecture 6  10/4/2021
#import data
library(ISLR)
names(Default)
mydata=Default
# Simple Logistic Regression with input of balance
model1=glm(default~balance,data=mydata,family = binomial)
plot(model1)
summary(model1)
# multiple logistic regression with inputs balance an income
model2=model1=glm(default~balance+income,data=mydata,family = binomial)
summary(model2)
contrasts(mydata$default)
# multiple logistic regression with all inputs
model3=glm(default~student+balance+income,data=mydata,family=binomial)
summary(model3)
#end of class period

#10/6/2021
# Using Logistic Regression for data analysis of the default data
library(ISLR)
mydata=Default
head(mydata)
# build logistic regression using default as output
model=glm(default~.,data=mydata,family=binomial)
summary(model)
# update the model by including those significant predictors
model.update=glm(default~student+balance, data=mydata, family = binomial)
summary(model.update)
# predicting a specific instance, student=yes, balance = 2500, Pr(Y=1|x1=yes,x2=2500)=0.947
# student = No, balance = 3000
predict(model.update,newdata = data.frame(student=c("Yes", "No"),balance=c(2500,3000)),type="response")
contrasts(mydata$default)

# Predicting all data we have used so far and summarize prediction accuracy
predict1=predict(model.update,type="response") # Predicted probability, still needs to be converted into the binary (yes or no) 
head(predict1)
# confusion matrix
predict1.class=rep("No",nrow(mydata))
predict1.class[predict1>0.5]="Yes"
table(predict1.class, mydata$default)
mean(predict1.class!=mydata$default) # mis-classification error rate
1-mean(predict1.class!=mydata$default) # classification accuracy

# Model prediction of testing data.
# Build model using training data  (e.g., first 1000 observations as training data);
# make prediction using this model on my test data (e.g., last 9000 observations)
train.data=mydata[1:1000,]
test.data=mydata[1001:10000,]
# building a model from training data
model.train=glm(default~balance+student, data=train.data,family = binomial)
predict.test =predict(model.train,newdata = test.data,type="response")
test.response=rep("No",nrow(test.data))
test.response[predict.test>0.5]="Yes"
table(test.response,test.data$default)
mean(test.response!=test.data$default)
#end lecture 6

###############################################################################

# Lecture 7   Monday 10/11/2021
library(ISLR)
library(MASS)
head(Default)
mydata=Default
# Build an LDA using single predictor, Balance
lda.model1=lda(default~balance,data=mydata)
lda.model1
# Build an LDA using more than one predictor, e.g., balance, student
lda.model2=lda(default~balance+student,data=mydata)
lda.model2

# Model Prediction
lda.pred1=predict(lda.model1)
table(lda.pred1$class,mydata$default) # confusion matrix
(24+257)/10000
mean(lda.pred1$balance!=mydata$default) # misclassification error rate
lda.pred2=predict(lda.model2)
table(lda.pred2$class,mydata$default) # confusion matrix
mean(lda.pred2$class!=mydata$default)
# Wednesday 10/13/2021 (wrapped up lec 7 lda prediction and confusion matrix)
#end lec7

###############################################################################

# Lecture 8
# Wednesday 10/13/2021
# QDA for analyzing default data
library(ISLR)
library(MASS)
mydata=Default

#QDA using single predictor, balance
qda.model1=qda(default~balance,data=mydata)
qda.pred1=predict(qda.model1)
table(qda.pred1$class,mydata$default)
mean(qda.pred1$class!=mydata$default)
# Recall our LDA model1 (0.0281), our LDA model2 (0.0275)
# thus, the qda model reduced the misclassification error rate a little bit

# QDA using two predictors, balance, student
qda.model2=qda(default~balance+student,data=mydata)
qda.pred2=predict(qda.model2)
table(qda.pred2$class,mydata$default)
mean(qda.pred2$class!=mydata$default)
#end lec8

###############################################################################

# ESI 4606 - Lecture 9 - K nearest neighbors
# 10/18/2021

library(class)
library(ISLR)
mydata=Default
head(mydata)
# make a prediction for all data
# predicting all data using k=2, input: balance
# knn(input of existing data, input of data you want to make prediction, output of existing data, k = [value of k])
set.seed(123) # enables replication, deals with the random number generators.
pred2=knn(as.matrix(mydata$balance),as.matrix(mydata$balance),mydata$default,k=2)
table(pred2,mydata$default) #confusion matrix
mean(pred2!=mydata$default) # misclassification error rate

#predicting all existing data using k=200
set.seed(111)
pred200=knn(as.matrix(mydata$balance),as.matrix(mydata$balance),mydata$default,k=200)
table(pred200,mydata$default) #confusion matrix
mean(pred200!=mydata$default) # misclassification error rate

# partition the data into training (first 80%) and testing (last 20%) parts
train=mydata[1:8000,]
test=mydata[8001:10000,]
# predict training data and summarize training prediction error (k=2)
# model input: balance
set.seed(123)
pred2.train=knn(as.matrix(train$balance),as.matrix(train$balance),train$default,k=2)
table(pred2.train,train$default)
mean(pred2.train!=train$default)
# predict testing data and summarize testing prediction error (k=2)
set.seed(123)
pred2.test=knn(as.matrix(train$balance),as.matrix(test$balance),train$default,k=2)
table(pred2.test,test$default)
mean(pred2.test!=test$default)
#end lec9

###############################################################################

# Lecture 10: Model selection and Assessment -  11/1/2021
# Using Default data to build KNN models (under different K values: 300, 20, 5, and 1)
# model input: balance,   model output: default
library(ISLR)
data=Default
#Split: First 80% as train data, and last 20% as test data.
train=data[1:8000,]
test=data[8001:10000,]

# classifying training data with different K values, K = 1, 5, 20, 300
library(class)
# K = 1
set.seed(1)
# recall knn(arg1,arg2,arg3,arg4):
# arg1: inputs of train
# arg2: inputs of data you want to predict on.
# arg3: outputs of training data;
# arg4: k value
pred1.train=knn(as.matrix(train$balance),as.matrix(train$balance),train$default,k=1)
table(pred1.train,train$default) # confusion matrix
mean(pred1.train!=train$default) # training error
# K=5
set.seed(1)
pred5.train=knn(as.matrix(train$balance),as.matrix(train$balance),train$default,k=5)
table(pred5.train,train$default) 
mean(pred5.train!=train$default)
# K=20
set.seed(1)
pred20.train=knn(as.matrix(train$balance),as.matrix(train$balance),train$default,k=20)
table(pred20.train,train$default) 
mean(pred20.train!=train$default)
# K=300
set.seed(1)
pred300.train=knn(as.matrix(train$balance),as.matrix(train$balance),train$default,k=300)
table(pred300.train,train$default) 
mean(pred300.train!=train$default)

# classifying the test data with different k values, K = 1, 5, 20, 300
#K=1
set.seed(1)
pred1.test=knn(as.matrix(train$balance),as.matrix(test$balance),train$default, k=1)
table(pred1.test,test$default) # confusion matrix for test data
mean(pred1.test!=test$default) # 0.0465 Test error
#K=5
set.seed(1)
pred5.test=knn(as.matrix(train$balance),as.matrix(test$balance),train$default, k=5)
table(pred5.test,test$default) # confusion matrix for test data
mean(pred5.test!=test$default) # Test error
#K=20
set.seed(1)
pred20.test=knn(as.matrix(train$balance),as.matrix(test$balance),train$default, k=20)
table(pred20.test,test$default) # confusion matrix for test data
mean(pred20.test!=test$default) # Test error
#K=300
set.seed(1)
pred300.test=knn(as.matrix(train$balance),as.matrix(test$balance),train$default, k=300)
table(pred300.test,test$default) # confusion matrix for test data
mean(pred300.test!=test$default) # Test error

# Monday 11/1/2021
# Select polynomials orders using cross-validation
library(ISLR) # we are using the Auto data
library(boot) # contains cross-validation function for polynomial regression
auto.data=Auto # MPG=beta0+beta1*HP+beta2*HP^2+beta3*HP^3+...
# Model 0: MPG=beta0,
# Model 1: MPG=beta0+beta1*HP
# Model 2: MPG=beta0+beta1*HP+beta2*HP^2
# ... Model 14: MPG=beta0+beta1*HP+beta2*HP^2+beta3*HP^3+... beta14*HP^14
# Use CV to select the best model from 15 models (note, the first model is just a constant)
cv.error=rep(0,14)
train.error=rep(0,14)
for(i in 1:14)
{
  poly.fit=glm(mpg~poly(horsepower,i),data=auto.data) # i is the polynomial order, corresponds to each of the models above
  set.seed(i)
  cv.error[i]=cv.glm(auto.data,poly.fit,K=5)$delta[1] # using 5-fold CV
  pred=predict(poly.fit,auto.data)
  train.error[i]=mean(pred-auto.data$mpg)^2  #training MSE
}
cv.error
# Plot cv error, training error v.s. model complexity (i.e., polynomial order)
plot(seq(1,14,1),cv.error,ylim=c(0,50),pch=16,col="red")
points(seq(1,14,1),train.error,pch=16)

# Plot fitted models with orders 1, 2, and 4
# Model 1: mpg=beta0+beta1*HP^1
poly.fit1=glm(mpg~poly(horsepower,1),data=auto.data)
# Model 2: mpg=beta0+beta1*HP^1+beta2*HP^2
poly.fit2=glm(mpg~poly(horsepower,2),data=auto.data)
# Model 4: mpg=beta0+beta1*HP^1+beta2*HP^2+beta3*HP^3+beta4*HP^4
poly.fit4=glm(mpg~poly(horsepower,4),data=auto.data)
x=seq(1,250,1)
pred1=predict(poly.fit1,list(horsepower=x))
pred2=predict(poly.fit2,list(horsepower=x))
pred4=predict(poly.fit4,list(horsepower=x))
plot(auto.data$horsepower,auto.data$mpg,xlab="Horsepower",ylab="MPG")
lines(x,pred1,lwd=2)  # this one looks underfit
lines(x,pred2,col="red",lwd=2) # better, captured more of the pattern
lines(x,pred4,col="blue",lwd=2) # a little more flexible, may have captured noise but 
# may also be better fit than model 2, comparing these 
# models is how we figure out which is best.

# end class period (lec 10)

###############################################################################

# Lecture 11: Variable Selection - 11/3/2021
# Best subset selection
library(leaps)
head(CreditData)
head(Credit)
CreditData=Credit
CreditData=CreditData[,-1]
# Step 1:
lm.exhaustive=regsubsets(Balance~.,data=CreditData,nvmax=11) # nvmax is specify max number of variable on which to build model
# Step 2: list bic values for all candidate models
summary(lm.exhaustive)$bic 
#  find the final best of these models with smallest bic
which.min(summary(lm.exhaustive)$bic) 
# M4 is selected as the final best based on BIC
# M4: y=beta0+beta1*income+beta2*limit+beta3*cards+beta4*student
# plot bic
plot(summary(lm.exhaustive)$bic,type="l")
points(seq(1,11,1),summary(lm.exhaustive)$bic,pch=16)
points(4,summary(lm.exhaustive)$bic[4],col="red",pch=16)

# Forward Stepwise selection (Step 1)
lm.forward=regsubsets(Balance~.,data=CreditData,nvmax=11,method = "forward")
summary(lm.forward)
# Step 2:
summary(lm.forward)$cp
which.min(summary(lm.forward)$cp)

# Backward Stepwise selection
lm.backward=regsubsets(Balance~.,data=CreditData,nvmax=11,method = "backward") 
summary(lm.backward)

# end of class period // end of lecture 11
# 11/8/2021 Modified and appended the above section^

###############################################################################

# Lecture 12   11/8/2021
x=seq(1,10,1)
x
# centering th data
# way 1:
x.center=x-mean(x)
mean(x.center)
# way 2: scale
scale(x,scale=FALSE)

# scaling raw data
# way 1: x/sd(x)
x.scale=x/sd(x)
sd(x.scale)
# way 2: scale()
scale(x,center=FALSE,scale=sd(x))

# standardizing raw data
# way 1: (x-mean(x))/sd(x)
x.s=(x-mean(x))/sd(x)
mean(x.s)
sd(x.s)
# way 2: scale()
scale(x)

# 11/10/2021
# Ridge Regression
library(glmnet)
train.x.scale=scale(train.x)
test.x.scale=scale(test.x)
grid=10^(seq(-1,5,0.1))
ridge.model=glmnet(train.x.scale,train.y,alpha=0,lambda=grid,thresh=1e-12)
# lambda=400
ridge.pred=predict(ridge.model,s=400,newx = test.x.scale)
# summarize test error (MSE)
mean((ridge.pred-test.y)^2)
predict(ridge.model,s=400,type="coefficients")[1:12,]
# Ridge regression with cross-validation to tune the lambda
set.seed(10)
ridge.cv=cv.glmnet(train.x.scale,train.y,alpha=0)
lambda.best=ridge.cv$lambda.1se
lambda.best
# run ridge regression with best lambda (66.17)
ridge.pred.cv=predict(ridge.model,s=lambda.best,newx=test.x.scale)
mean((ridge.pred.cv-test.y)^2)
predict(ridge.model,s=lambda.best,type="coefficients")[1:12,]


# Run LASSO
grid=10^(seq(-1,5,0.1))
lasso.model=glmnet(train.x.scale,train.y,alpha=1,lambda=grid,thresh=1e-12)
# predict test data using LASSO with lambda = 0 
lasso.pred=predict(lasso.model,s=0,newx=test.x.scale)
mean((lasso.pred-test.y)^2)
predict(lasso.model,s=0,type="coefficients")[1:12,]
# Run LASSO with cv
set.seed(11)
lasso.cv=cv.glmnet(train.x.scale,train.y,alpha=1)
lambda.best=lasso.cv$lambda.1se
# Run LASSO with best lambda of 27.98538
lasso.pred.cv=predict(lasso.model,s=lambda.best,newx=test.x.scale)
mean((lasso.pred.cv-test.y)^2)
predict(lasso.model,s=lambda.best,type="coefficients")[1:12,]
# end lec 12

###############################################################################

# Lecture 13 - Principle Component Analysis - 11/15/2021
# PCA for analyzing US-arrest data
data=USArrests
head(data)
pca.obj=prcomp(data,scale=TRUE)
pca.obj$rotation
# Biplot
biplot(pca.obj,scale=0)
# Flip sign and draw the biplot
pca.obj$rotation=-pca.obj$rotation
pca.obj$x=-pca.obj$x
biplot(pca.obj,scale=0)
# Proportion of variability explained by different PCs
pve=pca.obj$sdev^2/sum(pca.obj$sdev^2)
pve
# draw the Scree Plot
plot(pve,xlab="# of PCs",ylab="PVE",ylim=c(0,1),type="b")
# another benefit of PCA is that it reduces colinearity which 
# improves the accuracy of linear regression model

# end lec 13

###############################################################################

# Lecture 14  ***Verify***       11/17/2021
# Decision Trees
# Build classification tree by analyzing "Heart" data
# HeartData=read.csv("/Users/lukemorgan/Desktop/ESI 4606 Analytics 1/Heart.csv")
library(tree)
tree.model=tree(AHD~.,data = HeartData)
plot(tree.model)
text(tree.model)
table(HeartData$Thal)
tree.model

# Pruning tree using cross-validation
set.seed(2)
cv.tree.model=cv.tree(tree.model,FUN=prune.misclass)
cv.tree.model$dev
cv.tree.model$size
prune.tree.model=prune.misclass(tree.model,best=11) # build a subsequent pruned tree
plot(prune.tree.model)
text(prune.tree.model)
# Prediction
pred.prune.tree=predict(prune.tree.model,data=HeartData,type="class")
table(pred.prune.tree,HeartData$AHD)

# Regression tree to analyze "Hitterrs" data
library(ISLR)
data=Hitters
data=na.omit(data)  # removes rows with "na"
data$Salary=log(data$Salary) 
tree.model1=tree(Salary~.,data=data)
plot(tree.model1)
text(tree.model1)
# Pruning tree
set.seed(5)
cv.tree.model1=cv.tree(tree.model1)
cv.tree.model1$size
cv.tree.model1$dev   
prune.tree.model1=prune.tree(tree.model1,best = 9)
pred.prune.tree1=predict(prune.tree.model1,data) # notice that we don't need to declare "type" for regression
mean((pred.prune.tree1-data$Salary)^2) # Training MSE for regression model

# end lec 14

###############################################################################

# Lecture 15 - K-Means Clustering 11/22/2021
# Import data
x=c(1,1.4,3,5,3.5,4.5,3.5)
y=c(1,2,4,7,5,5,4.5)
data=cbind(x,y)   #cbind is column combine into table

# k-means with specified initial centroids
k.obj=kmeans(data,center=matrix(c(2.8,4,3.8,4.75),nrow=2),nstart=1)   
# we used a matrix with two rows to explicitly specify the two centroids we wanted to use.
# nstart is how you specify number of times it runs the operation.
k.obj$cluster
k.obj$centers

# K-means with specified number of clusters
set.seed(1)
k.obj=kmeans(data,center=4,nstart=100) # centroid will be randomly generated
k.obj$cluster
k.obj$centers

# end lec 15

###############################################################################

# Lec 16 - Hierarchical Clustering - 11/24/2021
# Import data
x=c(1,1.4,3,5,3.5,4.5,3.5)
y=c(1,2,4,7,5,5,4.5)
data=cbind(x,y)
# hierarchical clustering with complete linkage
hc.complete=hclust(dist(data),method="complete") # dist(data) # running this by itself will show dissimilarity matrix
plot(hc.complete,xlab="",sub="",main="Complete Linkage Cluster Dendrogram")

# Determine the number of clusters by specifying cluster number
cutree(hc.complete,k=3)

# Determine the number of clusters by specifying height at which to cut the tree.
cutree(hc.complete,h=1)

# end lec 16

###############################################################################

# Selected Topic - Ensemble Learning - ESI 4606 - 11/29/2021
# Load data file “EL.RData”
# CART tree
library(tree)
tree.model=tree(AHD~.,data=data.train)
set.seed(2)
cv.tree.model=cv.tree(tree.model,FUN=prune.misclass)
cv.tree.model$size
# [1] 1310 8 5 4 3 2 1
cv.tree.model$dev
# [1] 37 39 37 32 35 38 36 70
prune.tree.model=prune.misclass(tree.model,best=5)
pred.prune.tree=predict(prune.tree.model,data.test,type="class")
xx=table(pred.prune.tree,data.test$AHD)
(xx[1,2]+xx[2,1])/(xx[1,1]+xx[1,2]+xx[2,1]+xx[2,2])
# [1] 0.3061224

#######################
library(randomForest)
# Bagging
set.seed(2)
bag.model=randomForest(AHD~.,data=data.train,mtry=13,importance=TRUE)  
bag.pred=predict(bag.model,newdata=data.test)
xx=table(bag.pred,data.test$AHD)
(xx[1,2]+xx[2,1])/(xx[1,1]+xx[1,2]+xx[2,1]+xx[2,2])  # [1] 0.244898
# Importance
varImpPlot(bag.model)
# Random Forests
set.seed(2)
forest.model=randomForest(AHD~.,data=data.train,mtry=4, importance=TRUE) 
forest.pred=predict(forest.model,newdata=data.test)
xx=table(forest.pred,data.test$AHD)
(xx[1,2]+xx[2,1])/(xx[1,1]+xx[1,2]+xx[2,1]+xx[2,2])
# [1] 0.2040816

# Boosting
library(gbm)
# Converting factor response “Yes” and “No” into numerical response “1” and # # “0”, which is supported by boosting tree pakcage
data.train[,14]=as.numeric(data.train[,14])-1
data.test[,14]=as.numeric(data.test[,14])-1
set.seed(2)
boost.model=gbm(AHD~.,data=data.train,distribution="bernoulli", n.trees=2000,interaction.depth=3,shrinkage=0.15)
pred.boost=predict(boost.model,newdata=data.test,n.trees=2000, type="response")
pred.boost=ifelse(pred.boost<0.5,0,1)
xx=table(pred.boost,data.test$AHD)
(xx[1,2]+xx[2,1])/(xx[1,1]+xx[1,2]+xx[2,1]+xx[2,2])
# [1] 0.1904762
#importance
summary(boost.model)

# end selected topics lecture (Ensemble Learning)

###############################################################################










