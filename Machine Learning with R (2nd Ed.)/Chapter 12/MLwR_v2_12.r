##### Chapter 12: Specialized Machine Learning Topics -------------------

## working with data from proprietary sources ----

library(rio)
credit <- import("credit.csv")
export(credit, "credit.xlsx")
convert("credit.csv", "credit.dta")

## working with SQL databases ----

# (note: this is an example for illustration only; will need to be modified
#  with your specific DB settings)

library(RODBC)
my_db <- odbcConnect("my_dsn") # no DB password
my_db <- odbcConnect("my_dsn", uid = "my_username", pwd = "my_password") # DB requires username and password

# query the DB
my_query <- "select * from my_table where my_value = 1"
results_df <- sqlQuery(channel = my_db, query = my_query, stringsAsFactors = FALSE)

odbcClose(my_db) # close the DB connection

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
str(packt_page, nchar.max=200)

# using httr
library(httr)
packt_page <- GET("https://www.packtpub.com")
str(packt_page, max.level = 1)
str(content(packt_page, type="text"), nchar.max=200)

# web scraping using rvest ---

# simple example
library(rvest)
packt_page <- html("https://www.packtpub.com")
html_node(packt_page, "title")
html_node(packt_page, "title") %>% html_text()

# more realistic example
library(rvest)
cran_ml <- html("http://cran.r-project.org/web/views/MachineLearning.html")
cran_ml

ml_packages <- html_nodes(cran_ml, "a")
head(ml_packages, n = 7)

# reading XML
library(XML)

library(xml2)

## Reading and writing JSON ----

# example using Google Maps API and httr packages
library(httr)
map_search <- GET("https://maps.googleapis.com/maps/api/geocode/json",
                       query = list(address = "Eiffel Tower"))
map_search
content(map_search)
content(map_search)$results[[1]]$formatted_address
content(map_search)$results[[1]]$geometry$location$lat
content(map_search)$results[[1]]$geometry$location$lng

# simple example using rjson
library(rjson)
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

## working with social network and graph data ----

library(igraph)
karate <- read.graph("karate.txt", "edgelist", directed = FALSE)
plot(karate)
degree(karate)
betweenness(karate)

## Managing very large datasets ----

## with dplyr
library(dplyr)
credit <- read.csv("credit.csv")
credit_tbl <- as.tbl(credit)
credit_tbl

# using dplyr to work with a database
# ...creating the sqlite database
credit_db_conn <- src_sqlite("credit.sqlite3", create = TRUE)
copy_to(credit_db_conn, credit_tbl, temporary = FALSE)

# ...accessing the sqlite database
credit_db_conn <- src_sqlite("credit.sqlite3")
credit_tbl <- tbl(credit_db_conn, "credit_tbl")

# querying tbl objects
select(credit_tbl, amount)

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
# with enough cores to complete the task

# random number generation using multicore
# non-parallel
system.time(l1 <- rnorm(1000000))

# two cores
system.time(l2 <- unlist(mclapply(1:2, function(x) {
  rnorm(500000)}, mc.cores = 2)))

# four cores
system.time(l4 <- unlist(mclapply(1:4, function(x) {
  rnorm(250000) }, mc.cores = 4)))

# eight cores
system.time(l8 <- unlist(mclapply(1:8, function(x) {
  rnorm(125000) }, mc.cores = 8)))

# creating a 4-node cluster with snow
library(snow)
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

system.time(l1 <- rnorm(1000000))
system.time(l4 <- foreach(i = 1:4, .combine = 'c')
                    %do% rnorm(250000))

library(doParallel)
detectCores()
registerDoParallel(cores=4)
system.time(l4p <- foreach(i = 1:4, .combine = 'c')
                     %dopar% rnorm(250000))

stopImplicitCluster()

## Parallel processing with caret ----

library(caret)
credit <- read.csv("credit.csv")

# training a random forest without allowing parallel computing
system.time(train(default ~ ., data = credit, method = "rf",
                  trControl = trainControl(allowParallel = FALSE)))

# training the same random forest in parallel (4 cores)
library(doParallel)
registerDoParallel(cores=4)
system.time(train(default ~ ., data = credit, method = "rf"))
