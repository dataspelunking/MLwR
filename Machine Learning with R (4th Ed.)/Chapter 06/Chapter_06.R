##### Chapter 6: Regression Methods -------------------

#### Part 1: Linear Regression -------------------

## Understanding regression ----
## Example: Space Shuttle Launch Data ----
launch <- read.csv("challenger.csv")

# estimate beta manually
b <- cov(launch$temperature, launch$distress_ct) / var(launch$temperature)
b

# estimate alpha manually
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a

# calculate the correlation of launch data
r <- cov(launch$temperature, launch$distress_ct) /
       (sd(launch$temperature) * sd(launch$distress_ct))
r
cor(launch$temperature, launch$distress_ct)

# creating a simple multiple regression function
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}

# examine the launch data
str(launch)

# test regression model with simple linear regression
reg(y = launch$distress_ct, x = launch[2])

# use regression model with multiple regression
reg(y = launch$distress_ct, x = launch[2:4])

# if desired, you can confirming the custom multiple regression function works
# correctly by comparing to the result using R's built-in lm function (not in text)
model <- lm(distress_ct ~ temperature + field_check_pressure + flight_num, data = launch)
model

## Example: Predicting Medical Expenses ----
## Step 2: Exploring and preparing the data ----
insurance <- read.csv("autoinsurance.csv", stringsAsFactors = TRUE)
str(insurance)

# summarize the charges variable
summary(insurance$expenses)

# histogram of insurance charges
hist(insurance$expenses)

# tables for categorical features
table(insurance$geo_area)
table(insurance$vehicle_type)

# exploring relationships among features: correlation matrix
cor(insurance[c("age", "est_value", "miles_driven", "expenses")])

# visualing relationships among features: scatterplot matrix
pairs(insurance[c("age", "est_value", "miles_driven",
                  "expenses")], pch = ".")

# more informative scatterplot matrix
library(psych)
pairs.panels(insurance[c("age", "est_value", "miles_driven",
                         "expenses")], pch = ".")

## Step 3: Training a model on the data ----
ins_model <- lm(expenses ~ age + geo_area + vehicle_type +
                  est_value + miles_driven +
                  college_grad_ind + speeding_ticket_ind +
                  hard_braking_ind + late_driving_ind +
                  clean_driving_ind,
                data = insurance)

ins_model <- lm(expenses ~ ., data = insurance) # this is equivalent to above

# see the estimated beta coefficients
options(scipen = 999) # turn off scientific notation
ins_model

## Step 4: Evaluating model performance ----
# see more detail about the estimated beta coefficients
summary(ins_model)

## Step 5: Improving model performance ----

# add a higher-order "age" term
insurance$age2 <- insurance$age^2

# create final model
ins_model2 <- lm(expenses ~ . + hard_braking_ind:late_driving_ind,
                 data = insurance)

summary(ins_model2)

# making predictions with the regression model
insurance$pred <- predict(ins_model2, insurance)
cor(insurance$pred, insurance$expenses)

plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, geo_area = "rural", 
                   vehicle_type = "truck", est_value = 25000,
                   miles_driven = 14000, college_grad_ind = 0,
                   speeding_ticket_ind = 0, hard_braking_ind = 0,
                   late_driving_ind = 0, clean_driving_ind = 1))

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, geo_area = "rural", 
                   vehicle_type = "truck", est_value = 25000,
                   miles_driven = 14000, college_grad_ind = 0,
                   speeding_ticket_ind = 0, hard_braking_ind = 0,
                   late_driving_ind = 0, clean_driving_ind = 0))

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, geo_area = "rural", 
                   vehicle_type = "truck", est_value = 25000,
                   miles_driven = 24000, college_grad_ind = 0,
                   speeding_ticket_ind = 0, hard_braking_ind = 0,
                   late_driving_ind = 0, clean_driving_ind = 0))

## Predicting Insurance Policyholder Churn with Logistic Regression ----

churn_data <- read.csv("insurance_churn.csv")

prop.table(table(churn_data$churn)) # see the % of churn

# create the logistic regression model
churn_model <- glm(churn ~ . -member_id, data = churn_data,
                   family = binomial(link = "logit"))

# examine the logistic regression model parameter estimates
summary(churn_model)

# read the test set
churn_test <- read.csv("insurance_churn_test.csv")

# make predictions on the test set
churn_test$churn_prob <- predict(churn_model, churn_test,
                                 type = "response")

# examine the predicted values
summary(churn_test$churn_prob)

# provide the members most likely to churn
churn_order <- order(churn_test$churn_prob, decreasing = TRUE)
head(churn_test[churn_order, c("member_id", "churn_prob")], n = 5)

#### Part 2: Regression Trees and Model Trees -------------------

## Understanding regression trees and model trees ----
## Example: Calculating SDR ----
# set up the data
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)

# compute the SDR
sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) + length(at2) / length(tee) * sd(at2))
sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) + length(bt2) / length(tee) * sd(bt2))

# compare the SDR for each split
sdr_a
sdr_b

## Example: Estimating Wine Quality ----
## Step 2: Exploring and preparing the data ----
wine <- read.csv("whitewines.csv")

# examine the wine data
str(wine)

# the distribution of quality ratings
hist(wine$quality)

# summary statistics of the wine data
summary(wine)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

## Step 3: Training a model on the data ----
# regression tree using rpart
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)

# get basic information about the tree
m.rpart

# get more detailed information about the tree
summary(m.rpart)

# use the rpart.plot package to create a visualization
library(rpart.plot)

# a basic decision tree diagram
rpart.plot(m.rpart, digits = 3)

# a few adjustments to the diagram
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

## Step 4: Evaluate model performance ----

# generate predictions for the testing dataset
p.rpart <- predict(m.rpart, wine_test)

# compare the distribution of predicted values vs. actual values
summary(p.rpart)
summary(wine_test$quality)

# compare the correlation
cor(p.rpart, wine_test$quality)

# function to calculate the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# mean absolute error between predicted and actual values
MAE(p.rpart, wine_test$quality)

# mean absolute error between actual values and mean value
mean(wine_train$quality) # result = 5.87
MAE(5.87, wine_test$quality)

## Step 5: Improving model performance ----
# train a Cubist Model Tree
library(Cubist)
m.cubist <- cubist(x = wine_train[-12], y = wine_train$quality)

# display basic information about the model tree
m.cubist

# display the tree itself
summary(m.cubist)

# generate predictions for the model
p.cubist <- predict(m.cubist, wine_test)

# summary statistics about the predictions
summary(p.cubist)

# correlation between the predicted and true values
cor(p.cubist, wine_test$quality)

# mean absolute error of predicted and true values
# (uses a custom function defined above)
MAE(wine_test$quality, p.cubist)
