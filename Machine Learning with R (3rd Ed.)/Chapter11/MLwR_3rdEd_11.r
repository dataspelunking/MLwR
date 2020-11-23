##### Chapter 11: Improving Model Performance -------------------

# load the credit dataset
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
library(caret)

## Creating a simple tuned model ----
# automated parameter tuning of C5.0 decision tree 
RNGversion("3.5.2") # use an older random number generator to match the book
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
grid <- expand.grid(model = "tree",
                    trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    winnow = FALSE)

# look at the result of expand.grid()
grid

# customize train() with the control list and grid of parameters 
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
m

## Bagging ----
# Using the ipred bagged decision trees
library(ipred)
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

# estimate performance of ipred bagged trees
library(caret)
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",
      trControl = ctrl)

## Boosting ----

## Using C5.0 Decision Tree (not shown in book)
library(C50)
m_c50_bst <- C5.0(default ~ ., data = credit, trials = 100)

## Using AdaBoost.M1
library(adabag)

# create a Adaboost.M1 model
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(300)
m_adaboost <- boosting(default ~ ., data = credit)
p_adaboost <- predict(m_adaboost, credit)
head(p_adaboost$class)
p_adaboost$confusion

# create and evaluate an Adaboost.M1 model using 10-fold-CV
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(300)
adaboost_cv <- boosting.cv(default ~ ., data = credit)
adaboost_cv$confusion

# calculate kappa
library(vcd)
Kappa(adaboost_cv$confusion)

## Random Forests ----
# random forest with default settings
library(randomForest)
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf

# calculate kappa on the out-of-bag estimate
library(vcd)
Kappa(rf$confusion[1:2,1:2])


## Simulate a data mining competition
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10,
                     selectionFunction = "best",
                     savePredictions = TRUE,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

# auto-tune a random forest
grid_rf <- expand.grid(mtry = c(2, 4, 8, 16))

# test a random forest with the above settings
# note: this may take a long time to run (~10 minutes)
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf",
              metric = "ROC", trControl = ctrl,
              tuneGrid = grid_rf)
m_rf

# auto-tune a boosted C5.0 decision tree
grid_c50 <- expand.grid(model = "tree",
                        trials = c(10, 25, 50, 100),
                        winnow = FALSE)

RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(300)
m_c50 <- train(default ~ ., data = credit, method = "C5.0",
               metric = "ROC", trControl = ctrl,
               tuneGrid = grid_c50)
m_c50

# compare their ROC curves
library(pROC)
roc_rf <- roc(m_rf$pred$obs, m_rf$pred$yes)
roc_c50 <- roc(m_c50$pred$obs, m_c50$pred$yes)

plot(roc_rf, col = "red", legacy.axes = TRUE)
plot(roc_c50, col = "blue", add = TRUE)
