##### Chapter 15: Making Use of Big Data -------------------

## Classifying images using a pre-trained CNN in R ----
## IMPORTANT: some of the following steps are only necessary one time.

# this package allows you to install packages from GitHub
library(devtools)

# install the latest tensorflow package
devtools::install_github("rstudio/tensorflow") # only necessary the first time!

# install all of the dependencies for tensorflow (e.g., Python)
library(tensorflow)
install_tensorflow() # only necessary the first time!

# install the latest keras package
devtools::install_github("rstudio/keras") # only necessary the first time!

# Begin the Example
# load the ResNet50 model with weights trained on the ImageNet
library(keras)
m_resnet50 <- application_resnet50(weights = 'imagenet')

# load the image and convert to array
img <- image_load("ice_cream.jpg", target_size = c(224,224))
x <- image_to_array(img)

# x is now a 3d tensor: y, x, and color channel (r, g, b)
# note: (y, x) begins counting with (1, 1) in top-left and (1, 224) in top-right
dim(x)
str(x)

# example pixels
x[1, 224, 1:3] # a white pixel in the upper-right
x[40, 145, 1:3] # a red pixel in the ice cream

# create a 4-dimension tensor with a constant value of '1' in the first dimension
# (the first dimension is the 'batch' and is used for multiple images)
x <- array_reshape(x, c(1, dim(x)))
dim(x) # shows the new dimensions

#  do some color conversions and zero-centering as pre-processing for ResNet-50
x <- imagenet_preprocess_input(x)
x[1, 40, 145, 1:3] # look at the red ice cream pixel again

# use the ResNet-50 model to make a prediction for the image data
p_resnet50 <- predict(m_resnet50, x)
c_resnet50 <- imagenet_decode_predictions(p_resnet50, top = 10)

# see the predictions
c_resnet50

# lapply the image processing steps on the other two images
img_list <- list("cat.jpg", "pizza.jpg")
img_data <- lapply(img_list, image_load, target_size = c(224,224))
img_arrs <- lapply(img_data, image_to_array)
img_resh <- lapply(img_arrs, array_reshape, c(1, 224, 224, 3))
img_prep <- lapply(img_resh, imagenet_preprocess_input)
img_prob <- lapply(img_prep, predict, object = m_resnet50)

# sapply the decode function to get the final predictions
img_classes <- sapply(img_prob, imagenet_decode_predictions, top = 3)
img_classes

## Using word2vec for understanding text in R ----

# note: the code in this section requires the word embedding that was trained
# on the Google News archive. Download the GoogleNews-vectors-negative300.bin.gz
# file from https://code.google.com/archive/p/word2vec/ and unzip it to your R
# project folder before proceeding.

library(word2vec)

# load the Google-trained 300-dimension word2vec embedding
m_w2v <- read.word2vec(file = "GoogleNews-vectors-negative300.bin",
                       normalize = TRUE)

# examine the structure of the model
str(m_w2v)

# obtain the vector for a few terms
foods <- predict(m_w2v, c("cereal", "bacon", "eggs", "sandwich", "salad", "steak", "spaghetti"), type = "embedding")
meals <- predict(m_w2v, c("breakfast", "lunch", "dinner"), type = "embedding")

# examine a single word vector
head(foods["cereal", ])

# examine the first few columns
foods[, 1:5]

# compute the similarity between the foods and meals
word2vec_similarity(foods, meals)

# can also use cosine similarity (not shown in book)
word2vec_similarity(foods, meals, type = "cosine")

# create vector of hypothetical social media posts
user_posts = c(
  "I eat bacon and eggs in the morning for the most important meal of the day!",
  "I am going to grab a quick sandwich this afternoon before hitting the gym.",
  "Can anyone provide restaurant recommendations for my date tonight?"
)

library(tm) # use tm package for removeWords() and stopwords() functions

user_posts_clean <- removeWords(user_posts, stopwords())
user_posts_clean <- txt_clean_word2vec(user_posts_clean)
user_posts_clean[1] # look at the first cleaned user post

# get the doc2vec vectors for the user posts
post_vectors <- doc2vec(m_w2v, user_posts_clean)
str(post_vectors)

# get the word2vec vectors for the meal terms
meals <- predict(m_w2v, c("breakfast", "lunch", "dinner"), type = "embedding")

# compare the similarity of the posts and terms
word2vec_similarity(post_vectors, meals)

## Visualizing highly dimensional data ----

library(tidyverse)
sns_terms <- read_csv("snsdata.csv") |>
  select(basketball:drugs)

# find the first two principal components
library(irlba)
set.seed(123456)
sns_pca <- sns_terms |>
  prcomp_irlba(n = 2, center = TRUE, scale = TRUE) 

# create a scatterplot
library(ggplot2)
as.data.frame(sns_pca$x) |>
  ggplot(aes(PC1, PC2)) + geom_point(size = 1, shape = 1)

## Visualizing data's natural clusters with t-SNE ----

# take a random sample of 5,000 users
library(tidyverse)
set.seed(123)
sns_sample <- read_csv("snsdata.csv") |>
  slice_sample(n = 5000)

# run t-SNE with default parameters
library(Rtsne)
set.seed(123)
sns_tsne <- sns_sample |>
  select(basketball:drugs) |>
  Rtsne(check_duplicates = FALSE)

# visualize the t-SNE result
library(ggplot2)
data.frame(sns_tsne$Y) |>
  ggplot(aes(X1, X2)) + geom_point(size = 2, shape = 1)

# create a categorical feature for the number of terms used
sns_sample_tsne <- sns_sample |>
  bind_cols(data.frame(sns_tsne$Y)) |> # add the t-SNE data
  rowwise() |> # work across rows rather than columns
  mutate(n_terms = sum(c_across(basketball:drugs))) |>
  ungroup() |> # remove rowwise behavior
  mutate(`Terms Used` = if_else(n_terms > 0, "1+", "0"))

# visualize the t-SNE result by number of terms used
sns_sample_tsne |>
  ggplot(aes(X1, X2, shape = `Terms Used`, color = `Terms Used`)) +
  geom_point(size = 2) +
  scale_shape(solid = FALSE)

## working with SQL databases ----

# a tidy approach to database connections
library(DBI)
library(RSQLite)

# creates a connection to the credit SQLite database
con <- dbConnect(RSQLite::SQLite(), "credit.sqlite3")
dbListTables(con)

# create a data frame from the result
res <- dbSendQuery(con, "SELECT * FROM credit WHERE age >= 45")
credit_age45 <- dbFetch(res)
summary(credit_age45$age)

# disconnect from the database
dbClearResult(res)
dbDisconnect(con)

# connecting to databases with the odbc package
# (note: this is an example for illustration only and will need to be modified for your DB)
library(DBI)
con <- dbConnect(odbc:odbc(), "my_data_source_name")

library(DBI)
con <- dbConnect(odbc::odbc(),
                 database = "my_database",
                 uid = "my_username",
                 pwd = "my_password",
                 host = "my.server.address",
                 port = 1234)

# using a database backend with dplyr
library(DBI)
library(dplyr)
con <- dbConnect(RSQLite::SQLite(), "credit.sqlite3")
credit_tbl <- con |> tbl("credit")

# compute summary statistics on age values filtered from the database
credit_tbl |>
  filter(age >= 45) |>
  select(age) |>
  collect() |>
  summary()

# show the average loan amount by loan default status, age 45+
credit_tbl |>
  filter(age >= 45) |>
  group_by(default) |>
  summarize(mean_amount = avg(amount))

# show the SQL used for the prior analysis
credit_tbl |>
  filter(age >= 45) |>
  group_by(default) |>
  summarize(mean_amount = avg(amount)) |>
  show_query()

## Measuring execution time ----

system.time(rnorm(1000000))

## Working in parallel ----

library(parallel)
detectCores()

# note: the following will only work on non-Windows systems (i.e., MacOSX or Unix/Linux)
# you will also need enough cores to complete each task!

# random number generation using multicore
# one core
system.time(l1 <- unlist(mclapply(1:10, function(x) {
  rnorm(10000000)}, mc.cores = 1)))

# two cores
system.time(l2 <- unlist(mclapply(1:10, function(x) {
  rnorm(10000000)}, mc.cores = 2)))

# four cores
system.time(l4 <- unlist(mclapply(1:10, function(x) {
  rnorm(10000000) }, mc.cores = 4)))

# eight cores
system.time(l8 <- unlist(mclapply(1:10, function(x) {
  rnorm(10000000) }, mc.cores = 8)))

# creating a 4-node cluster with snow
cl1 <- makeCluster(4)

# confirm that the cluster is functioning
clusterCall(cl1, function() { Sys.info()["nodename"] })

# running the same function on each node (not shown in book)
clusterCall(cl1, function() { print("ready!") })

# running a different operation on each node
clusterApply(cl1, c('A', 'B', 'C', 'D'),
             function(x) { paste("Cluster", x, "ready!") })

# close the cluster (IMPORTANT STEP!)
stopCluster(cl1)

## Parallel loops with foreach ----

# generate 100 million random numbers
system.time(l1 <- rnorm(100000000))

# combine four sets of 25 million random numbers
library(foreach)
system.time(l4 <- foreach(i = 1:4, .combine = 'c')
            %do% rnorm(25000000))

# confirm the number of cores
detectCores()

# parallel the above foreach loop
library(doParallel)
registerDoParallel(cores = 4)
system.time(l4p <- foreach(i = 1:4, .combine = 'c')
            %dopar% rnorm(25000000))

stopImplicitCluster()

## Parallel processing with caret ----

# training a random forest without allowing parallel computing
library(caret)
credit <- read.csv("credit.csv")
system.time(train(default ~ ., data = credit, method = "rf",
                  trControl = trainControl(allowParallel = FALSE)))

# training the same random forest in parallel (8 cores)
library(doParallel)
registerDoParallel(cores = 8)
system.time(train(default ~ ., data = credit, method = "rf"))
stopImplicitCluster()

## Parallel cloud computing with Apache Spark

library(sparklyr)
spark_install() # only need to run this the first time using Spark
spark_cluster <- spark_connect(master = "local")

credit_spark <- spark_read_csv(spark_cluster, "credit.csv")

# split the credit_spark dataset into training and testing
splits <- sdf_random_split(credit_spark,
                           train = 0.75, test = 0.25,
                           seed = 123)

# build a random forest model using Spark
credit_rf <- splits$train |>
  ml_random_forest(default ~ .)

# make predictions on the test set
pred <- ml_predict(credit_rf, splits$test)

ml_binary_classification_evaluator(pred, metric_name = "areaUnderROC")

spark_disconnect(spark_cluster)

## Faster modeling with h2o

library(h2o)
h2o_instance <- h2o.init()
credit.hex <- h2o.uploadFile("credit.csv")

h2o.randomForest(y = "default",
                 training_frame = credit.hex,
                 ntrees = 500,
                 seed = 123)
