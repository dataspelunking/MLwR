##### Chapter 11: Improving Model Performance -------------------

# load the credit dataset
credit <- read.csv("credit.csv")
library(caret)

## Creating a simple tuned model ----
# automated parameter tuning of C5.0 decision tree 
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")

# summary of tuning results
m

# apply the best C5.0 candidate model to make predictions
p <- predict(m, credit)
table(p, credit$default)

# obtain predicted classes
head(predict(m, credit, type = "raw"))

# obtain predicted probabilities
head(predict(m, credit, type = "prob"))

## Customizing the tuning process ----
# use trainControl() to alter resampling strategy
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")

# use expand.grid() to create grid of tuning parameters
grid <- expand.grid(.model = "tree",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")

# look at the result of expand.grid()
grid

# customize train() with the control list and grid of parameters 
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
m

## Bagging ----
# Using the ipred bagged decision trees
library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

# estimate performance of ipred bagged trees
library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",
      trControl = ctrl)

# Using caret's more general bagging function
# create a bag control object using svmBag
str(svmBag)
svmBag$fit

bagctrl <- bagControl(fit = svmBag$fit,
                      predict = svmBag$pred,
                      aggregate = svmBag$aggregate)

# fit the bagged svm model
set.seed(300)
svmbag <- train(default ~ ., data = credit, "bag",
                trControl = ctrl, bagControl = bagctrl)

svmbag

## Boosting ----
# Using AdaBoost.M1
# (please note that this example is not in the text) 
library(adabag)
set.seed(300)
bst <- boosting.cv(default ~ ., data = credit, mfinal = 50)
bst$confusion
bst$error

## Random Forests ----
# random forest with default settings
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf

library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

# auto-tune a random forest
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)
m_rf

# auto-tune a boosted C5.0 decision tree
grid_c50 <- expand.grid(.model = "tree",
                        .trials = c(10, 20, 30, 40),
                        .winnow = "FALSE")

set.seed(300)
m_c50 <- train(default ~ ., data = credit, method = "C5.0",
                metric = "Kappa", trControl = ctrl,
               tuneGrid = grid_c50)
m_c50