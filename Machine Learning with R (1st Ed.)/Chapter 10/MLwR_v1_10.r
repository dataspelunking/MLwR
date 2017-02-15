##### Chapter 10: Evaluating Model Performance -------------------

## Confusion matrixes in R ----
sms_results <- read.csv("sms_results.csv")
head(sms_results)
head(subset(sms_results, actual_type != predict_type))

# specifying vectors
table(sms_results$actual_type, sms_results$predict_type)
# using the formula interface
xtabs(~ actual_type + predict_type, sms_results)
# using the CrossTable function
library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type)

# accuracy and error rate calculation --
# accuracy
(154 + 1202) / (154 + 1202 + 5 + 29)
# error rate
(5 + 29) / (154 + 1202 + 5 + 29)
# error rate = 1 - accuracy
1 - 0.9755396

## Beyond accuracy: other performance measures ----
library(caret)
confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# Kappa statistic
# example using SMS classifier
pr_a <- 0.865 + 0.111
pr_a

pr_e <- 0.868 * 0.886 + 0.132 * 0.114
pr_e

k <- (pr_a - pr_e) / (1 - pr_e)
k

library(vcd)
Kappa(table(sms_results$actual_type, sms_results$predict_type))

library(irr)
kappa2(sms_results[1:2])

# Sensitivity and specificity
# example using SMS classifier
sens <- 154 / (154 + 29)
sens

spec <- 1202 / (1202 + 5)
spec

# example using the caret package
library(caret)
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type, negative = "ham")

# Precision and recall
prec <- 154 / (154 + 5)
prec

rec <- 154 / (154 + 29)
rec

# example using the caret package
library(caret)
posPredValue(sms_results$predict_type, sms_results$actual_type, positive = "spam")
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# F-measure
f <- (2 * prec * rec) / (prec + rec)
f

f2 <- (2 * 154) / (2 * 154 + 5 + 29)
f2

## Visualizing Performance Tradeoffs ----
library(ROCR)
pred <- prediction(predictions = sms_results$prob_spam,
                   labels = sms_results$actual_type)

# ROC curves
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve for SMS spam filter", col = "blue", lwd = 2)

# add a reference line to the graph
abline(a = 0, b = 1, lwd = 2, lty = 2)

# calculate AUC
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
as.numeric(perf.auc@y.values)

# partitioning data
library(caret)
credit <- read.csv("credit.csv")

# Holdout method
# using random IDs
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500],]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

# using caret function
in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
credit_train <- credit[in_train, ]
credit_test <- credit[-in_train, ]

# 10-fold CV
folds <- createFolds(credit$default, k = 10)
str(folds)
credit01_train <- credit[folds$Fold01, ]
credit01_test <- credit[-folds$Fold01, ]

## Automating 10-fold CV for a C5.0 Decision Tree using lapply() ----
library(caret)
library(C50)
library(irr)

set.seed(123)
folds <- createFolds(credit$default, k = 10)

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[x, ]
  credit_test <- credit[-x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)
mean(unlist(cv_results))
