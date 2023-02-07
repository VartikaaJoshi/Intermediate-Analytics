install.packages("ISLR")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("psych")
install.packages("caret")
install.packages("glmnet")
install.packages("Metrics")
library(caret)
library(ISLR)
library(dplyr)
library(tidyverse)
library(psych)
library(glmnet)
library(Metrics)
df <- College
options(scipen = 999)

# Load the library
library(glmnet)

# Load the data
data("College")
View(College)

#1. Split the data
set.seed(123)
trainIndex <- sample(x=nrow(df), size=nrow(df)*0.8)
train <- df[trainIndex,]
test <- df[-trainIndex,]
train_x <- model.matrix(Grad.Rate~.,train)[,-19]
test_x <- model.matrix(Grad.Rate~., test)[,-19]
train_y <- train$Grad.Rate
test_y <- test$Grad.Rate

##Regularization
#1. Split the data
set.seed(123)
trainIndex <- sample(x=nrow(df), size=nrow(df)*0.8)
train <- df[trainIndex,]
test <- df[-trainIndex,]
train_x <- model.matrix(Grad.Rate~.,train)[,-19]
test_x <- model.matrix(Grad.Rate~., test)[,-19]
train_y <- train$Grad.Rate
test_y <- test$Grad.Rate
####RIDGE#2. Find best values of lambda
#lambda min: minimizes out of sample loss
#lambda 1se: largest value of lambda within 1 standard error of lambdamin
#Find the best lambda using cross-validation
set.seed(123)
cv.ridge <- cv.glmnet(train_x, train_y,alpha=0, nfolds=10)
#lambda min
cv.ridge$lambda.min
#lambda.1se
cv.ridge$lambda.1se
#3. Plot the results
plot(cv.ridge)


#4. Fit Ridge regression model against the training set
model.ridge.min <- glmnet(train_x, train_y, alpha = 0, lambda =cv.ridge$lambda.min) 
#lambda min
model.ridge.min
coef(model.ridge.min)# ridge regression coefficients for lambda min
model.ridge.1se <- glmnet(train_x, train_y, alpha = 0, lambda =cv.ridge$lambda.1se) 
#lambda 1se
model.ridge.1se
coef(model.ridge.1se)
#ridge regression coefficients for lambda 1se

#5. Train set prediction of RIDGE model by calculating RMSE
#lamda min
pred.ridge.min <- predict(model.ridge.min, newx = train_x)
train.ridge.rmse.min <- rmse(train_y, pred.ridge.min)
train.ridge.rmse.min
#lambda 1se
pred.ridge.1se <- predict(model.ridge.1se, newx = train_x)
train.ridge.rmse.1se <- rmse(train_y, pred.ridge.1se)
train.ridge.rmse.1se
#6. Test set prediction of ridge model using RMSE
#lambda min
pred.ridge.min.test <- predict(model.ridge.min, newx = test_x)
test.ridge.rmse.min <- rmse(test_y, pred.ridge.min.test)
test.ridge.rmse.min
#lambda 1se
pred.ridge.1se.test <- predict(model.ridge.1se, newx = test_x)
test.ridge.rmse.1se <- rmse(test_y, pred.ridge.1se.test)
test.ridge.rmse.1se

##LASSO
#7. Find best values of lambda
cv.lasso <- cv.glmnet(train_x, train_y, nfolds=10)
#lambda min
cv.lasso$lambda.min
#lambda.1se
cv.lasso$lambda.1se

#8. Plot the results
plot(cv.lasso)

#9. fit LASSO regression model against the training set
#fit lasso regression model against the training set
model.lasso.min <- glmnet(train_x, train_y, alpha=1, lambda =cv.lasso$lambda.min)
#lambda min
model.lasso.min
coef(model.lasso.min)
model.lasso.1se <- glmnet(train_x, train_y, alpha = 1, lambda =cv.lasso$lambda.1se)
#lambda 1se
model.lasso.1se
coef(model.lasso.1se)
##regression coefficients using lambda.1se

#10. Train set prediction of LASSO model by calculating RMSE
#lambda min
pred.lasso.min <- predict(model.lasso.min, newx = train_x)
train.lasso.rmse <- rmse(train_y, pred.lasso.min)
train.lasso.rmse
#lambda 1se
pred.lasso.1se <- predict(model.lasso.1se, newx = train_x)
train.lasso.rmse.1se <- rmse(train_y, pred.lasso.1se)
train.lasso.rmse.1se

#11. Test set prediction of lasso model using RMSE
#lambda min
pred.lasso.min.test <- predict(model.lasso.min, newx = test_x)
test.lasso.rmse.min <- rmse(test_y, pred.lasso.min.test)
test.lasso.rmse.min
#lambda 1se
pre.lasso.1se.test <- predict(model.lasso.1se, newx = test_x)
test.lasso.rmse.1se <- rmse(test_y, pre.lasso.1se.test)
test.lasso.rmse.1se

#13. Stepwise selection and regularization
#forward selection
step(lm(Grad.Rate ~ 1, data = train), direction = 'forward', scope = ~Private+Apps+Accept+Enroll+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books+Personal+PhD+Terminal+S.F.Ratio+perc.alumni+Expend)
#backward selection
step(lm(Grad.Rate ~ ., data = train), direction = 'backward')
#comparring regression models
fit1 <- lm(formula = Grad.Rate ~ Outstate + Top25perc + perc.alumni +Room.Board + P.Undergrad + Apps + Expend +as.factor(Private) + Personal +Accept, data = train) 
#regression model using forwardselection
fit2 <- lm(formula = Grad.Rate ~ as.factor(Private) + Apps + Accept +Top25perc +P.Undergrad + Outstate + Room.Board + Personal + PhD +Terminal +perc.alumni + Expend, data = train) 
#regression modelusing backward selection
fit3 <- lm(formula = Grad.Rate ~ Apps +Top10perc + Top25perc+perc.alumni +Room.Board + P.Undergrad + Outstate+ as.factor(Private)+ Personal, data = train)
#regression model using regularization
AIC(fit1, fit2, fit3)%>%write.csv("~/Downloads/ALY6015/Module4/AIC.csv")
BIC(fit1,fit2, fit3)%>%write.csv("~/Downloads/ALY6015/Module4/BIC.csv")
