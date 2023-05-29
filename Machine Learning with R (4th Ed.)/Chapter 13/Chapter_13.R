##### Chapter 13: Challenging Data --------------------

## Stepwise regression ----

# read the data and do some simple data preparation
library(tidyverse)
titanic_train <- read_csv("titanic_train.csv") |>
  mutate(
    Age_MVI = if_else(is.na(Age), 1, 0),
    Age = if_else(is.na(Age), mean(Age, na.rm = TRUE), Age),
    Cabin = if_else(is.na(Cabin), "X", Cabin),
    Embarked = factor(if_else(is.na(Embarked), "X", Embarked)),
    Sex = factor(Sex)
  )

# specify the simplest logistic regression model
simple_model <- glm(Survived ~ 1, family = binomial, data = titanic_train)
  
# specify the full logistic regression model
full_model <- glm(Survived ~ Age + Age_MVI + Embarked + Sex + Pclass + SibSp + Fare,
                  family = binomial, data = titanic_train)

# forward stepwise regression
sw_forward <- stats::step(simple_model, scope = formula(full_model),
                          direction = "forward")

# obtain the formula for the final model
formula(sw_forward)

# the final model's regression coefficients
sw_forward$coefficients

# backward stepwise
sw_backward <- stats::step(full_model, direction = "backward")

## Feature selection with Boruta ----

set.seed(12345) # set the random seed to ensure results match
# create a feature with random values to demonstrate a useless feature
titanic_train$rand_vals <- runif(n = 891, min = 1, max = 100)

# run Boruta on the Titanic dataset (this can take a long time for larger datasets)
library(Boruta)
titanic_boruta <- Boruta(Survived ~ PassengerId + Age + 
                           Sex + Pclass + SibSp + rand_vals,
                         data = titanic_train, doTrace = 1)
# check the result
titanic_boruta

# plot the feature importance
plot(titanic_boruta)

## Principal Component Analysis (PCA) ----

library(tidyverse) # load the tidyverse suite of packages

sns_data <- read_csv("snsdata.csv") # read the teenage social media data

# select only the 36 columns from the column named 'basketball' through the one named 'drugs'
# each column contains the count of times each social media profile used the respective term
sns_terms <- sns_data |> select(basketball:drugs)

# the irlba library provides a more efficient PCA function than R's built-in prcomp()
library(irlba)

# run the PCA - note that we center and re-scale the data here
set.seed(2023) # to ensure the results match the book
sns_pca <- sns_terms |> 
  prcomp_irlba(n = 10, center = TRUE, scale = TRUE) # find first 10 principal components of the SNS data

# create scree plot of the SNS data PCA
screeplot(sns_pca, npcs = 10, type = "lines",
          main = "Scree Plot of SNS Data Principal Components")

# use summary to see the components and the proportion of variance explained
summary(sns_pca)

# examine the PCA object -- we care most about the $x and $rotation components
str(sns_pca)

# the $x component is the transformed version of the original data
str(sns_pca$x)

# the $x is our original data transformed to have new "features" -- the principal components
nrow(sns_pca$x) # should have 30,000 rows
head(sns_pca$x) # should have ten columns

# create a "long" version of the PCA dataset for visualization
sns_pca_long <- tibble(SNS_Term = colnames(sns_terms), as_tibble(sns_pca$rotation)) |> # add the row labels
  pivot_longer(PC1:PC10, names_to = "PC", values_to = "Contribution") # go from a wide to long dataset

# use ggplot to visualize the terms that are important for PC4
library(ggplot2)

sns_pca_long |>
  filter(PC == "PC3") |>
  top_n(15, abs(Contribution)) |>
  mutate(SNS_Term = reorder(SNS_Term, Contribution)) |>
  ggplot(aes(SNS_Term, Contribution, fill = SNS_Term)) +
    geom_col(show.legend = FALSE, alpha = 0.8) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
          axis.ticks.x = element_blank()) + 
    labs(x = "Social Media Term",
         y = "Relative Importance to Principal Component",
         title = "Top 15 Contributors to PC3")

# create a function to visualize the four other components
plot_pca <- function(component) {
  sns_pca_long |>
    filter(PC == component) |>
    top_n(15, abs(Contribution)) |>
    mutate(SNS_Term = reorder(SNS_Term, Contribution)) |>
    ggplot(aes(SNS_Term, Contribution, fill = SNS_Term)) +
    geom_col(show.legend = FALSE, alpha = 0.8) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
          axis.ticks.x = element_blank()) + 
    labs(x = "Social Media Term",
         y = "Relative Importance to Principal Component",
         title = paste("Top 15 Contributors to", component))
}

# use the function
plot_pca("PC1")
plot_pca("PC2")
plot_pca("PC4")
plot_pca("PC5")

# we can use the principal components to predict number of friends
sns_data_pca <- cbind(sns_data[1:4], sns_pca$x) # join the principal components to the original data

# create a linear regression model predicting friends from the principal components
m <- lm(friends ~ PC1 + PC2 + PC3 + PC4 + PC5, data = sns_data_pca)

m # show the model coefficients

## Remapping sparse categorical data ----

library(tidyverse)

# read the Titanic dataset and create Title feature (from Chapter 12)
titanic_train <- read_csv("titanic_train.csv") |>
  mutate(Title = str_extract(Name, ", [A-z]+\\.")) |>
  mutate(Title = str_replace_all(Title, "[, \\.]", ""))

# the Title feature has a large number of categories
table(titanic_train$Title, useNA = "ifany")

# group categories with similar real-world meaning
titanic_train <- titanic_train |>
  mutate(TitleGroup = fct_collapse(Title, 
    Mr = "Mr",
    Mrs = "Mrs",
    Master = "Master",
    Miss = c("Miss", "Mlle", "Mme", "Ms"),
    Noble = c("Don", "Sir", "Jonkheer", "Lady"),
    Military = c("Capt", "Col", "Major"),
    Doctor = "Dr",
    Clergy = "Rev",
    other_level = "Other")
  ) |>
  mutate(TitleGroup = fct_na_value_to_level(TitleGroup,
                                            level = "Unknown"))

# examine the recoding
table(titanic_train$TitleGroup)

# look at the counts and proportions of all levels, sorted largest to smallest
fct_count(titanic_train$Title, sort = TRUE, prop = TRUE)

# lump together everything outside of the top three levels
table(fct_lump_n(titanic_train$Title, n = 3))

# lump together everything with less than 1%
table(fct_lump_prop(titanic_train$Title, prop = 0.01))

# lump together everything with fewer than 5 observations
table(fct_lump_min(titanic_train$Title, min = 5))

## Binning sparse numeric data ----

# examine the Titanic fare data
head(titanic_train$Fare)
summary(titanic_train$Fare)

# create a binary variable for first/second class
titanic_train <- titanic_train |> mutate(
  fare_firstclass = if_else(Fare >= 31, 1, 0, missing = 0)
)

# tabulate the binary values
table(titanic_train$fare_firstclass)

# create a three-level feature using case_when()
titanic_train <- titanic_train |>
  mutate(
    fare_class = case_when(
      Fare >= 31 ~ "1st Class",
      Fare >= 15 ~ "2nd Class",
      TRUE ~ "3rd Class"
    )
  )

# examine the result
table(titanic_train$fare_class)

# the cut() function can accomplish the same as the above case_when()
table(cut(titanic_train$Fare, breaks = c(-Inf, 15, 31, Inf),
          right = FALSE))

# use cut() with seq() to generate evenly-sized break points
table(cut(titanic_train$Fare, right = FALSE,
          breaks = seq(from = 0, to = 550, by = 50)))

# use cut() with quantiles() and seq() to create bins with equal numbers of examples
table(cut(titanic_train$Fare, right = FALSE,
          breaks = quantile(titanic_train$Fare,
                            probs = seq(0, 1, 0.20))))

# use the tidyverse ntile() function to create five bins
table(ntile(titanic_train$Fare, n = 5))

# convert the ntile() groups to a factor
titanic_train <- titanic_train |>
  mutate(fare_level = factor(ntile(Fare, n = 11)))

table(titanic_train$fare_level)

## Performing missing value imputation ----

library(readr)
titanic_train <- read_csv("titanic_train.csv")

# impute arbitrary text strings for missing categorical data
titanic_train <- titanic_train |>
  mutate(
    Cabin = if_else(is.na(Cabin), "X", Cabin),
    Embarked = if_else(is.na(Embarked), "Unknown", Embarked)
  )

# impute mean value and create missing value indicator for age
titanic_train <- titanic_train |>
  mutate(
    Age_MVI = if_else(is.na(Age), 1, 0),
    Age = if_else(is.na(Age), mean(Age, na.rm = TRUE), Age)
  )

## Simple strategies for rebalancing data ----

# load and prepare the teenage social media data
library(tidyverse)

snsdata <- read_csv("snsdata.csv") |>
  mutate(
    gender = fct_recode(gender, Female = "F", Male = "M"),
    gender = fct_na_value_to_level(gender, level = "Unknown"),
    age = ifelse(age < 13 | age > 20, NA, age) # replace age outliers
  ) |>
  group_by(gradyear) |>
  mutate(age_imp = if_else(is.na(age), median(age, na.rm = TRUE), age)) |>
  ungroup() |>
  select(gender, friends, gradyear, age_imp, basketball:drugs)

# examine the initial class imbalance
fct_count(snsdata$gender, prop = TRUE)

# undersample the majority classes
library(caret)
sns_undersample <- downSample(x = snsdata[2:40], y = snsdata$gender, yname = "gender")
fct_count(sns_undersample$gender, prop = TRUE)

# oversample the minority classes
library(caret)
sns_oversample <- upSample(x = snsdata[2:40], y = snsdata$gender, yname = "gender")
fct_count(sns_oversample$gender, prop = TRUE)

## Generating a synthetic balanced dataset with SMOTE ----

# create a gender balanced dataset using SMOTE
library(themis)
sns_balanced <- snsdata |> smote("gender") # simple syntax (without normalization)

# check that the dataset is now gender balanced
table(sns_balanced$gender)

# for a better SMOTE, create a normalize function (introduced in Chapter 3)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# an unnormalize function returns data to original scale (introduced in Chapter 7)
unnormalize <- function(norm_vals, col_name) {
  old_vals <- snsdata[col_name]
  unnormalized_vals <- norm_vals * (max(old_vals) - min(old_vals)) + min(old_vals)
  
  # round all columns to integers except age_imp
  rounded_vals <- if(col_name != "age_imp") { round(unnormalized_vals) }
                  else {unnormalized_vals}
  
  return (rounded_vals)
}

# more advanced smote() process with normalized data
snsdata_balanced <- snsdata |>
  mutate(across(where(is.numeric), normalize)) |> # normalize the numeric data
  smote("gender") |>
  mutate(across(where(is.numeric), ~unnormalize(.x, cur_column()))) # unnormalize the data

# confirm that the rebalanced dataset worked correctly
table(snsdata$gender)
table(snsdata_balanced$gender)
