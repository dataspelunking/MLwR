##### Chapter 7: Neural Networks and Support Vector Machines -------------------

##### Part 1: Neural Networks -------------------
## Example: Modeling the Strength of Concrete  ----

## Step 2: Exploring and preparing the data ----
# read in data and examine structure
concrete <- read.csv("concrete.csv")
str(concrete)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete, normalize))

# confirm that the range is now between zero and one
summary(concrete_norm$strength)

# compared to the original minimum and maximum
summary(concrete$strength)

# create training and test data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

## Step 3: Training a model on the data ----
# train the neuralnet model
library(neuralnet)

# simple ANN with only a single hidden neuron
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(12345) # to guarantee repeatable results
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                              data = concrete_train)

# visualize the network topology
plot(concrete_model)

## Step 4: Evaluating model performance ----
# obtain model results
model_results <- compute(concrete_model, concrete_test[1:8])
# obtain predicted strength values
predicted_strength <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)

## Step 5: Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(12345) # to guarantee repeatable results
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                               data = concrete_train, hidden = 5)

# plot the network
plot(concrete_model2)

# evaluate the results as we did before
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

# an EVEN MORE complex neural network topology with two hidden layers and custom activation function

# create a custom softplus activation function
softplus <- function(x) { log(1 + exp(x)) }

RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(12345) # to guarantee repeatable results
concrete_model3 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = c(5, 5), act.fct = softplus)

# plot the network
plot(concrete_model3)

# evaluate the results as we did before
model_results3 <- compute(concrete_model3, concrete_test[1:8])
predicted_strength3 <- model_results3$net.result
cor(predicted_strength3, concrete_test$strength)

# note that the predicted and actual values are on different scales
strengths <- data.frame(
  actual = concrete$strength[774:1030],
  pred = predicted_strength3
)

head(strengths, n = 3)

# this doesn't change the correlations (but would affect absolute error)
cor(strengths$pred, strengths$actual)

# create an unnormalize function to reverse the normalization
unnormalize <- function(x) { 
  return((x * (max(concrete$strength)) -
          min(concrete$strength)) + min(concrete$strength))
}

strengths$pred_new <- unnormalize(strengths$pred)
strengths$error <- strengths$pred_new - strengths$actual

head(strengths, n = 3)

cor(strengths$pred_new, strengths$actual)

##### Part 2: Support Vector Machines -------------------
## Example: Optical Character Recognition ----

## Step 2: Exploring and preparing the data ----
# read in data and examine structure
letters <- read.csv("letterdata.csv", stringsAsFactors = TRUE)
str(letters)

# divide into training and test data
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

## Step 3: Training a model on the data ----
# begin by training a simple linear SVM
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")

# look at basic information about the model
letter_classifier

## Step 4: Evaluating model performance ----
# predictions on testing dataset
letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions)

table(letter_predictions, letters_test$letter)

# look only at agreement vs. non-agreement
# construct a vector of TRUE/FALSE indicating correct/incorrect predictions
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

## Step 5: Improving model performance ----

# change to a RBF kernel
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(12345)
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

# test various values of the cost parameter
cost_values <- c(1, seq(from = 5, to = 40, by = 5))

RNGversion("3.5.2") # use an older random number generator to match the book
accuracy_values <- sapply(cost_values, function(x) {
  set.seed(12345)
  m <- ksvm(letter ~ ., data = letters_train,
            kernel = "rbfdot", C = x)
  pred <- predict(m, letters_test)
  agree <- ifelse(pred == letters_test$letter, 1, 0)
  accuracy <- sum(agree) / nrow(letters_test)
  return (accuracy)
})

plot(cost_values, accuracy_values, type = "b")
