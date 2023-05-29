##### Chapter 14: Building Better Learners -------------------

# load the credit dataset
credit <- read.csv("credit.csv")
library(caret)

## Creating a simple tuned model ----

# look up the tuning parameters for C5.0
modelLookup("C5.0")

# automated parameter tuning of C5.0 decision tree 
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")

# summary of tuning results
m

# apply the best C5.0 candidate model to make predictions
p <- predict(m, credit)
table(p, credit$default)

# obtain predicted classes
head(predict(m, credit))

# obtain predicted probabilities
head(predict(m, credit, type = "prob"))

## Customizing the tuning process ----
# use trainControl() to alter resampling strategy
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")

# use expand.grid() to create grid of tuning parameters
grid <- expand.grid(model = "tree",
                    trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    winnow = FALSE)

# look at the result of expand.grid()
grid

# customize train() with the control list and grid of parameters 
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)

# see the results
m

## Bagging ----
# Using the ipred bagged decision trees
library(ipred)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

# estimate performance of ipred bagged trees
library(caret)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",
      trControl = ctrl)

## Boosting ----

## Using C5.0 Decision Tree (not shown in book)
library(C50)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
m_c50_bst <- C5.0(default ~ ., data = credit, trials = 100)

# create a Adaboost.M1 model
library(adabag)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
m_adaboost <- boosting(default ~ ., data = credit)
p_adaboost <- predict(m_adaboost, credit)
head(p_adaboost$class)
p_adaboost$confusion

# create and evaluate an Adaboost.M1 model using 10-fold-CV
set.seed(300)
adaboost_cv <- boosting.cv(default ~ ., data = credit)
adaboost_cv$confusion

# calculate kappa
library(vcd)
Kappa(adaboost_cv$confusion)

## Random Forests ----
# random forest with default settings
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf

# calculate kappa on the out-of-bag estimate
library(vcd)
Kappa(rf$confusion[1:2,1:2])

# ranger is a faster implementation of the random forest algorithm
library(ranger)
set.seed(300)
m_ranger <- ranger(default ~ ., data = credit)
m_ranger

# calculate kappa
Kappa(m_ranger$confusion.matrix)

## Gradient Boosting Machines (GBM) ----

# prepare the data for gbm()
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
credit$default <- ifelse(credit$default == "yes", 1, 0)

# create a random train and test split
set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# create a GBM model with default parameters
library(gbm)
set.seed(300)
m_gbm <- gbm(default ~ ., data = credit_train)
m_gbm

# evaluate the simple GBM model
p_gbm <- predict(m_gbm, credit_test, type = "response")
p_gbm_c <- ifelse(p_gbm > 0.50, 1, 0)
table(credit_test$default, p_gbm_c)

# compute kappa 
library(vcd)
Kappa(table(credit_test$default, p_gbm_c))

# create a tuned gbm() model using caret
# start by creating the tuning grid
grid_gbm <- expand.grid(
  n.trees = c(100, 150, 200),
  interaction.depth = c(1, 2, 3),
  shrinkage = c(0.01, 0.1, 0.3),
  n.minobsinnode = 10
)

# define the experiment's parameters
library(caret)
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "best")

# run the caret experiment
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
m_gbm_c <- train(default ~ ., data = credit, method = "gbm",
                 trControl = ctrl, tuneGrid = grid_gbm,
                 metric = "Kappa",
                 verbose = FALSE)

# see the results
m_gbm_c

## Extreme Gradient Boosting (XGB) ----

# Format the credit data as a sparse matrix
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
library(Matrix)
credit_matrix <- sparse.model.matrix(~ . -default, data = credit)

# examine the sparse credit_matrix
dim(credit_matrix)
print(credit_matrix[1:5, 1:15])

# remove the intercept
credit_matrix <- credit_matrix[, -1] 

# split the matrix into train and test
set.seed(12345)
train_ids <- sample(1000, 900)
credit_train <- credit_matrix[train_ids, ]
credit_test <- credit_matrix[-train_ids, ]

# check that the rows are 900 vs. 100 and the cols are 35 vs. 35
dim(credit_train)
dim(credit_test)

# create 1/0 vectors for train and test data indicating loan default
credit_train_labels <-
  ifelse(credit[train_ids, c("default")] == "yes", 1, 0)
credit_test_labels <-
  ifelse(credit[-train_ids, c("default")] == "yes", 1, 0)

# build the xgboost model
library(xgboost)

# set XGB hyperparameters
params.xgb <- list(objective   = "binary:logistic",
                   max_depth   = 6,
                   eta         = 0.3,
                   gamma       = 0,
                   colsample_bytree = 1,
                   min_child_weight = 1,
                   subsample = 1)

set.seed(555)
xgb_credit <- xgboost(params  = params.xgb,
                      data    = credit_train,
                      label   = credit_train_labels, 
                      nrounds = 100,
                      print_every_n = 10,
                      verbose = 1)

# make predictions
prob_default <- predict(xgb_credit, credit_test)
pred_default <- ifelse(prob_default > 0.50, 1, 0)

# create a confusion matrix
table(pred_default, credit_test_labels)

# compute kappa
library(vcd)
Kappa(table(pred_default, credit_test_labels))

# create a tuned xgboost() model using caret
# start by creating the tuning grid
grid_xgb <- expand.grid(
  eta = c(0.3, 0.4),
  max_depth = c(1, 2, 3),
  colsample_bytree = c(0.6, 0.8),
  subsample = c(0.50, 0.75, 1.00),
  nrounds = c(50, 100, 150),
  gamma = c(0, 1),
  min_child_weight = 1
)

# define the control object
library(caret)
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "best")

# run the caret experiment
set.seed(300)
m_xgb <- train(default ~ ., data = credit, method = "xgbTree",
                    trControl = ctrl, tuneGrid = grid_xgb,
                    metric = "Kappa", verbosity = 0)

# see the results of all models (not shown in book due to size of output)
m_xgb

# see the hyperparameters for the best performing model
m_xgb$bestTune

# get the best kappa out of the 216 models tested
max(m_xgb$results["Kappa"])
