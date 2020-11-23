##### Chapter 10: Evaluating Model Performance -------------------

## Create the predicted probabilities from the SMS classifier built in Chapter 4.
## NOTE: THIS SECTION WILL NOT RUN WITHOUT RUNNING THE CHAPTER 4 CODE TO CREATE THE SMS CLASSIFIER!

# obtain the predicted probabilities
sms_test_prob <- predict(sms_classifier, sms_test, type = "raw")
head(sms_test_prob)

# combine the results into a data frame
sms_results <- data.frame(actual_type = sms_test_labels,
                          predict_type = sms_test_pred,
                          prob_spam = round(sms_test_prob[ , 2], 5),
                          prob_ham = round(sms_test_prob[ , 1], 5))

# uncommenet this line to output the sms_results to CSV
# write.csv(sms_results, "sms_results.csv", row.names = FALSE)

## Confusion matrixes in R ----
sms_results <- read.csv("sms_results.csv", stringsAsFactors = TRUE)

# the first several test cases
head(sms_results)

# test cases where the model is less confident
head(subset(sms_results, prob_spam > 0.40 & prob_spam < 0.60))

# test cases where the model was wrong
head(subset(sms_results, actual_type != predict_type))

# specifying vectors
table(sms_results$actual_type, sms_results$predict_type)

# alternative solution using the formula interface (not shown in book)
xtabs(~ actual_type + predict_type, sms_results)

# using the CrossTable function
library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type)

# accuracy and error rate calculation --
# accuracy
(152 + 1203) / (152 + 1203 + 4 + 31)
# error rate
(4 + 31) / (152 + 1203 + 4 + 31)
# error rate = 1 - accuracy
1 - 0.9748201

## Beyond accuracy: other performance measures ----
library(caret)
confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# Kappa statistic
# example using SMS classifier
pr_a <- 0.865 + 0.109
pr_a

pr_e <- 0.868 * 0.888 + 0.132 * 0.112
pr_e

k <- (pr_a - pr_e) / (1 - pr_e)
k

# calculate kappa via the vcd package
library(vcd)
Kappa(table(sms_results$actual_type, sms_results$predict_type))

# calculate kappa via the irr package
library(irr)
kappa2(sms_results[1:2])

# Sensitivity and specificity
# example using SMS classifier
sens <- 152 / (152 + 31)
sens

spec <- 1203 / (1203 + 4)
spec

# example using the caret package
library(caret)
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type, negative = "ham")

# Precision and recall
prec <- 152 / (152 + 4)
prec

rec <- 152 / (152 + 31)
rec

# example using the caret package
library(caret)
posPredValue(sms_results$predict_type, sms_results$actual_type, positive = "spam")
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# F-measure
f <- (2 * prec * rec) / (prec + rec)
f

f <- (2 * 152) / (2 * 152 + 4 + 31)
f

## Visualizing Performance Tradeoffs ----
library(pROC)
sms_roc <- roc(sms_results$actual_type, sms_results$prob_spam)

# ROC curve for Naive Bayes
plot(sms_roc, main = "ROC curve for SMS spam filter", col = "blue", lwd = 2, legacy.axes = TRUE)

# compare to kNN 
sms_results_knn <- read.csv("sms_results_knn.csv")
sms_roc_knn <- roc(sms_results$actual_type, sms_results_knn$p_spam)
plot(sms_roc_knn, col = "red", lwd = 2, add = TRUE)

# calculate AUC for Naive Bayes and kNN
auc(sms_roc)
auc(sms_roc_knn)

## Estimating Future Performance ----

# partitioning data
library(caret)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)

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
credit01_test <- credit[folds$Fold01, ]
credit01_train <- credit[-folds$Fold01, ]

## Automating 10-fold CV for a C5.0 Decision Tree using lapply() ----
library(caret)
library(C50)
library(irr)

credit <- read.csv("credit.csv", stringsAsFactors = TRUE)

RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(123)
folds <- createFolds(credit$default, k = 10)

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[-x, ]
  credit_test <- credit[x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)
mean(unlist(cv_results))
