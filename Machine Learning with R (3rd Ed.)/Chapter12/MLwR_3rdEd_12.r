#### Chapter 12: Specialized Machine Learning Topics -------------------

## Making data "tidy" with the tidyverse packages ----

# using the tibble package to create tibbles from data frames
library(tibble)
credit <- read.csv("credit.csv")
credit_tbl <- as_tibble(credit)
credit_tbl

# using dplyr to prepare tibbles
library(dplyr)
credit <- as_tibble(read.csv("credit.csv"))
credit %>%
  filter(age >= 21) %>%
  mutate(years_loan_duration =
           months_loan_duration / 12) %>%
  select(default, years_loan_duration) %>%
  group_by(default) %>%
  summarize(mean_duration = mean(years_loan_duration))

## reading CSV files into tibbles with readr
library(readr)
credit <- read_csv("credit.csv")

## working with data from proprietary sources with rio ----

library(rio)
credit <- import("credit.csv")
export(credit, "credit.xlsx")
convert("credit.csv", "credit.dta")

## working with SQL databases ----

# a tidy approach to database connections
library(DBI)
library(SQLite)

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
con <- dbConnect(RSQLite::SQLite(), "credit.sqlite3")
credit_tbl <- con %>% tbl("credit")

library(dplyr)
credit_tbl %>%
  filter(age >= 45) %>%
  select(age) %>%
  collect() %>%
  summary()

# a traditional approach to SQL connectivity with RODBC
# (note: this is an example for illustration only and will need to be modified for your DB)

library(RODBC)
my_db <- odbcConnect("my_dsn") # no DB password
my_db <- odbcConnect("my_dsn", uid = "my_username", pwd = "my_password") # DB requires username and password

# query the DB
my_query <- "select * from my_table where my_value = 1"
results_df <- sqlQuery(channel = my_db, query = my_query, stringsAsFactors = FALSE)

odbcClose(my_db) # close the DB connection

# using dplyr to work with a database
# ...creating the sqlite database
library(dplyr)
credit <- read_csv("credit.csv")
credit_db_conn <- src_sqlite("credit.sqlite3", create = TRUE)
copy_to(credit_db_conn, credit, temporary = FALSE)

# ...accessing the sqlite database
credit_db_conn <- src_sqlite("credit.sqlite3")
credit_tbl <- tbl(credit_db_conn, "credit")

# querying tbl objects
select(credit_tbl, amount)

## Getting data from the web ----
# (note: the next three are examples for illustration only)

# example using R and read.csv()
mydata <- read.csv("http://www.mysite.com/mydata.csv")

# example using readLines()
mytext <- readLines("http://www.mysite.com/myfile.txt")

# example using download.file()
download.file("http://www.mysite.com/myfile.zip", "myfile.zip")

# using RCurl
library(RCurl)
packt_page <- getURL("https://www.packtpub.com")
str(packt_page, nchar.max = 200)

# using httr
library(httr)
packt_page <- GET("https://www.packtpub.com")
str(packt_page, max.level = 1)
str(content(packt_page, type = "text"), nchar.max = 200)

# web scraping using rvest ---

# simple example
library(rvest)
packt_page <- read_html("https://www.packtpub.com")
html_node(packt_page, "title") %>% html_text()

# more realistic example
library(rvest)
cran_ml <- read_html("http://cran.r-project.org/web/views/MachineLearning.html")
cran_ml

ml_packages <- html_nodes(cran_ml, "li a")
head(ml_packages, n = 5)
ml_packages %>% html_text() %>% head()

# reading XML
library(XML)

library(xml2)

## Reading and writing JSON ----

# simple example using jsonlite
library(jsonlite)
ml_book <- list(book_title = "Machine Learning with R", author = "Brett Lantz")
toJSON(ml_book)

ml_book_json <- "{
  \"title\": \"Machine Learning with R\",
  \"author\": \"Brett Lantz\",
  \"publisher\": {
    \"name\": \"Packt Publishing\",
    \"url\": \"https://www.packtpub.com\"
  },
  \"topics\": [\"R\", \"machine learning\", \"data mining\"],
  \"MSRP\": 54.99
}"

ml_book_r <- fromJSON(ml_book_json)
str(ml_book_r)

# example using the httr package to query the Apple iTunes API
library(httr)
music_search <- GET("https://itunes.apple.com/search",
                    query = list(term = "Beatles",
                                 media = "music",
                                 entity = "album",
                                 limit = 10))

music_search

library(jsonlite)
music_results <- fromJSON(content(music_search))
str(music_results)

music_results$results$collectionName

## working with social network and graph data ----

library(igraph)
karate <- read.graph("karate.txt", "edgelist", directed = FALSE)
plot(karate)
degree(karate)
betweenness(karate)

## Managing very large datasets ----

## with data.table

library(data.table)
credit <- fread("credit.csv")

credit[credit_history == "good", mean(amount)]
credit[, mean(amount), by=.(credit_history)]

## with ffdf

library(ff)
credit <- read.csv.ffdf(file = "credit.csv", header = TRUE)

mean(credit$amount) # this results in an error

library(ffbase)
mean(credit$amount) # this works

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
  rnorm(1000000)}, mc.cores = 1)))

# two cores
system.time(l2 <- unlist(mclapply(1:10, function(x) {
  rnorm(1000000)}, mc.cores = 2)))

# four cores
system.time(l4 <- unlist(mclapply(1:10, function(x) {
  rnorm(1000000) }, mc.cores = 4)))

# eight cores
system.time(l8 <- unlist(mclapply(1:10, function(x) {
  rnorm(1000000) }, mc.cores = 8)))

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

library(foreach)

system.time(l1 <- rnorm(100000000))
system.time(l4 <- foreach(i = 1:4, .combine = 'c')
                    %do% rnorm(25000000))

library(doParallel)
detectCores()
registerDoParallel(cores = 4)
system.time(l4p <- foreach(i = 1:4, .combine = 'c')
                     %dopar% rnorm(25000000))

stopImplicitCluster()

## Parallel processing with caret ----

library(caret)
credit <- read.csv("credit.csv",, stringsAsFactors = TRUE)

# training a random forest without allowing parallel computing
system.time(train(default ~ ., data = credit, method = "rf",
                  trControl = trainControl(allowParallel = FALSE)))

# training the same random forest in parallel (4 cores)
library(doParallel)
registerDoParallel(cores = 8)
system.time(train(default ~ ., data = credit, method = "rf"))

## Parallel cloud computing with Apache Spark

library(sparklyr)
spark_install(version = "2.1.0")
spark_cluster <- spark_connect(master = "local")

credit_spark <- spark_read_csv(spark_cluster, "credit.csv")

splits <- sdf_partition(credit_spark,
                        train = 0.75, test = 0.25,
                        seed = 123)

credit_rf <- splits$train %>%
  ml_random_forest(default ~ .)

pred <- ml_predict(credit_rf, splits$test)

ml_binary_classification_evaluator(pred, metric_name = "areaUnderROC")

## Faster random forests with ranger
library(ranger)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)

m <- ranger(default ~ ., data = credit,
            num.trees = 500,
            mtry = 4)

p <- predict(m, credit)

# note that the predictions are stored as p$predictions
head(p$predictions)

## Faster modeling with h2o

library(h2o)
h2o_instance <- h2o.init()
credit.hex <- h2o.uploadFile("credit.csv")

h2o.randomForest(y = "default",
                 training_frame = credit.hex,
                 ntrees = 500,
                 seed = 123)
