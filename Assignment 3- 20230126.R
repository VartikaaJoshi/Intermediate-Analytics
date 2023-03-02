print("Vartika Joshi")

install.packages("ISLR")
library(ISLR)

data(College)
View(College)
summary(College)

install.packages("moments")
library(moments)

sd(College$Apps)
sd(College$Accept)
sd(College$Enroll)
sd(College$Top10perc)
sd(College$Top25perc)
sd(College$Outstate)
sd(College$Grad.Rate)
sd(College$PhD)

mean(College$Apps)
mean(College$Accept)
mean(College$Enroll)
mean(College$Top10perc)
mean(College$Top25perc)
mean(College$Outstate)
mean(College$Grad.Rate)
mean(College$PhD)

skewness(College$Apps)
skewness(College$Accept)
skewness(College$Enroll)
skewness(College$Top10perc)
skewness(College$Top25perc)
skewness(College$Outstate)
skewness(College$Grad.Rate)
skewness(College$PhD)

kurtosis(College$Apps)
kurtosis(College$Accept)
kurtosis(College$Enroll)
kurtosis(College$Top10perc)
kurtosis(College$Top25perc)
kurtosis(College$Outstate)
kurtosis(College$Grad.Rate)
kurtosis(College$PhD)

library(ggplot2)
ggplot(College) + 
  geom_histogram(mapping = aes(x = Accept)) 
  ggtitle("Histogram of Accepted Students")
  
  ggplot(College) + 
    geom_boxplot(mapping = aes(x = Private, y = Accept)) +
    ggtitle("Boxplot of Accepted Students by Private/Public Universities")
  


library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Split data into train and test sets
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(College), replace=TRUE, prob=c(0.7,0.3))
train_data <- College[sample,]
test_data <- College[!sample, ]

head(train_data)

# check the dimension of the train and test data

model <- glm(Private~., family="binomial", data=train_data)
options(scipen=999)
summary(model)

dim(train_data)
dim(test_data)

install.packages("tidyselect")
install.packages("caret")
library(tidyselect)
library(ggplot2)
library(lattice)
library(caret)
index <- createDataPartition(College$Outstate, p = .7, list = FALSE)
train <- College[ index,]
test <- College[-index,]


library(stats)

#using 'Outstate' and 'Top10perc' as predictors
predictors <- c("Outstate", "Top10perc")

College$Private <- factor(College$Private)
str(College)
# Fit the model
model <- glm(Private ~ ., data = College, family = binomial(link = "logit"), subset = c("Private", predictors))

# View the model summary
summary(model)

#confusion matrix
train_data$pred <- predict(model,train_data, type="response") 
train_data$pred_label <- as.factor(ifelse(train_data$pred >= 0.5, "Yes","No"))

confusionMatrix(train_data$Private, train_data$pred_label)

#Result of the findings
test_data$pred <- predict(model,test_data, type="response") 
test_data$pred_label <- as.factor(ifelse(test_data$pred >= 0.5, "Yes","No"))
confusionMatrix(test_data$Private, test_data$pred_label)

#Roc curve
rocobj <- roc(as.ordered(test_data$Private), as.ordered(test_data$pred_label), ordered = TRUE)

library(pROC)
rocobj <- roc(as.factor(test_data$Private), test_data$pred_label)
plot(rocobj)
