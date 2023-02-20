library(dplyr)
install.packages("moments")
library(moments)
library(dplyr)
library(tidyverse)
library(readr)
library(corrplot)
#load Dataset
setwd("C:/Users/Sayali/Downloads/")
getwd()
my_data <- read.csv("FARS_crash_level_new.csv")
my_data
str(my_data)
summary(my_data)
dim(my_data)
glimpse(my_data)
colnames(my_data)
#Checking null values of the data set
colSums(is.na(my_data))

#Head and Tail of the data set
head(my_data)
tail(my_data)

#Descriptive statistics
#Mean
my_data %>% summarize(Mean_fatals = mean(fatals),
                   Mean_drunk_dr = mean(drunk_dr),
                   Mean_total_numoccs = mean(total_numoccs),
                   Mean_total_hit_run = mean(total_hit_run),
                   Mean_total_valid_license = mean(total_valid_license))

#Standard Deviation
my_data %>% summarize(sd_fatals = sd(fatals),
                      sd_drunk_dr = sd(drunk_dr),
                      sd_total_numoccs = sd(total_numoccs),
                      sd_total_hit_run = sd(total_hit_run),
                      sd_total_valid_license = sd(total_valid_license))

#median
my_data %>% summarize(Median_fatals = median(fatals),
                      Median_drunk_dr = median(drunk_dr),
                      Median_total_numoccs = median(total_numoccs),
                      Median_total_hit_run = median(total_hit_run),
                      Median_total_valid_license = median(total_valid_license))
#Mode
my_data %>% summarize(Mode_fatals = mode(fatals),
                      Mode_drunk_dr = mode(drunk_dr),
                      Mode_total_numoccs = mode(total_numoccs),
                      Mode_total_hit_run = mode(total_hit_run),
                      Mode_total_valid_license = mode(total_valid_license))

#Variance
my_data %>% summarize(Variance_fatals = var(fatals),
                      Variance_drunk_dr = var(drunk_dr),
                      Variance_total_numoccs = var(total_numoccs),
                      Variance_total_hit_run = var(total_hit_run),
                      Variance_total_valid_license = var(total_valid_license))

#Skewness
my_data %>% summarize(skewness_fatals = skewness(fatals),
                      skewness_drunk_dr = skewness(drunk_dr),
                      skewness_total_numoccs = skewness(total_numoccs),
                      skewness_total_hit_run = skewness(total_hit_run),
                      skewness_total_valid_license = skewness(total_valid_license))

#Kurtosis
my_data %>% summarize(kurtosis_fatals = kurtosis(fatals),
                      kurtosis_drunk_dr = kurtosis(drunk_dr),
                      kurtosis_total_numoccs = kurtosis(total_numoccs),
                      kurtosis_total_hit_run = kurtosis(total_hit_run),
                      kurtosis_total_valid_license = kurtosis(total_valid_license))

my_data %>% group_by(year) %>% summarize(Mean_fatals = mean(fatals),
                                         Mean_drunk_dr = mean(drunk_dr),
                                         Mean_total_numoccs = mean(total_numoccs),
                                         Mean_total_hit_run = mean(total_hit_run),
                                         Mean_total_valid_license = mean(total_valid_license))


my_data %>% group_by(dr_male,dr_female) %>% summarize(Mean_fatals = mean(fatals),
                                         Mean_drunk_dr = mean(drunk_dr),
                                         Mean_total_numoccs = mean(total_numoccs),
                                         Mean_total_hit_run = mean(total_hit_run),
                                         Mean_total_valid_license = mean(total_valid_license))



#Plotting frequency table for male
dr_male_Freq <- my_data %>%
  select(dr_male) %>%
  group_by(dr_male) %>%
  dplyr:: summarize(dr_male_Count = n())
dr_male_Freq

#Plotting frequency table for female
dr_female_Freq <- my_data %>%
  select(dr_female) %>%
  group_by(dr_female) %>%
  dplyr:: summarize(dr_female_Count = n())
dr_female_Freq

#Highest fatals occuring year
my_data %>% 
  arrange(desc(fatals)) %>% 
  head(25) %>% 
  ggplot(aes(y= reorder(year,fatals),
             x=fatals,
             fill= year))+
  geom_col()+
  theme(legend.position = "none")


#Highest fatals occuring states
my_data %>% 
  arrange(desc(fatals)) %>% 
  head(14) %>% 
  ggplot(aes(y= reorder(state,fatals),
             x=fatals,
             fill= state))+
  geom_col()+
  theme(legend.position = "none")


#Question1:What factors contribute to the number of fatalities in car accidents? 
#H0:u1=u2=u3
#H1:At least one mean is different from the others
anova<-aov(data$st_case~data$drunk_dr,data=data)
summary(anova)
a.summary<- summary(anova)
df.numerator<-a.summary[[1]][1,"Df"]
df.numerator
df.denominator<-a.summary[[1]][2,"Df"]
df.denominator
F.value<-a.summary[[1]][[1,"F value"]]
F.value
p.value<-a.summary[[1]][[1,"Pr(>F)"]]
p.value
ifelse(p.value>0.01,"Fail to reject the null hypothesis","Reject the null hypothesis")

#Question2:What is the frequency of hit-and-run accidents in the dataset?
hist(data$total_hit_run,breaks = 2, labels = c("no","yes"), col = rainbow(2),data=data)

#Question3:Is there a correlation between the number of previous accidents and the likelihood of a driver being involved in a fatal accident?
cor1<-cor.test(x = data[,"two_prev_acc"], y = data[,"fatals"])
cor1$p.value
cor1$conf.int
cor2<-cor.test(x = data[,"one_prev_acc"], y = data[,"fatals"])
cor2$p.value
cor2$conf.int
pairs.panels(data[c("two_prev_acc","one_prev_acc","fatals")])

#Question5:How does the number of accidents vary by time of day, day of the week, and month of the year? 
lm1<-lm(data$st_case~data$day + data$month + data$year)
summary(lm1)
#chi-squared
alpha<-0.05
observed1<-data$st_case

p1<-data$drunk_dr

result1<-chisq.test(x=observed1,p=p1)

result1$statistic
result1$p.value
result1$parameter
result1
ifelse(result1$p.value>alpha,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Load the data into R
getwd()
data <- read.csv("FARS_crash_level_compile.csv")

# Split the data into training and testing sets
library(caret)
set.seed(123)
data_clean <- na.omit(data)
splitIndex <- createDataPartition(data_clean$fatals, p = 0.7, list = FALSE)

training <- data_clean[ splitIndex,]
testing <- data_clean[-splitIndex,]

# Fit a random forest model to the training data
install.packages("randomForest")
library(randomForest)
fit <- randomForest(fatals ~ total_hit_run + total_registered_owner + total_not_registered + total_other_owner, data = training)

# Predict on the testing data
predictions <- predict(fit, newdata = testing)

# Convert the predictions to a binary outcome
predictions[predictions > 0.5] <- 1
predictions[predictions <= 0.5] <- 0

# Calculate the accuracy of the model
accuracy <- mean(predictions == testing$fatals)
accuracy


#Linear Regression
# Load the FARS dataset
fars <- data

# Fit a linear regression model to the data
lm_model <- lm(fatals ~ ve_total + ve_forms + hour + minute + dr_age_lower16 + dr_age_65over, data = fars)

# Print the model summary
summary(lm_model)

#multiple regression
# Load the necessary libraries
install.packages("dplyr")
install.packages("tidyverse")
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

# Remove unnecessary columns
data <- select(data, ve_total, fatals, drunk_dr, hour, weather, nm_alcohol_drug_med, nm_other_impair)


# Fit a multiple linear regression model
model <- lm(fatals ~ ve_total + drunk_dr + hour + weather + nm_alcohol_drug_med + nm_other_impair, data = training)

# Make predictions on the testing set
predictions <- predict(model, newdata = testing)

# Calculate the RMSE
rmse <- sqrt(mean((predictions - testing$fatals)^2))

# Print the RMSE
print(rmse)







