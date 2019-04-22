##### Chapter 2: Managing and Understanding Data -------------------

##### R data structures --------------------

## Vectors -----

# create vectors of data for three medical patients
subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

# access the second element in body temperature vector
temperature[2]

## examples of accessing items in vector
# include items in the range 2 to 3
temperature[2:3]

# exclude item 2 using the minus sign
temperature[-2]

# use a vector to indicate whether to include item
temperature[c(TRUE, TRUE, FALSE)]

## Factors -----

# add gender factor
gender <- factor(c("MALE", "FEMALE", "MALE"))
gender

# add blood type factor
blood <- factor(c("O", "AB", "A"),
                levels = c("A", "B", "AB", "O"))
blood

# add ordered factor
symptoms <- factor(c("SEVERE", "MILD", "MODERATE"),
                   levels = c("MILD", "MODERATE", "SEVERE"),
                   ordered = TRUE)
symptoms

# check for symptoms greater than moderate
symptoms > "MODERATE"

## Lists -----

# display information for a patient
subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]
symptoms[1]

# create list for a patient
subject1 <- list(fullname = subject_name[1], 
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1],
                 symptoms = symptoms[1])

# display the patient
subject1

## methods for accessing a list

# get a single list value by position (returns a sub-list)
subject1[2]

# get a single list value by position (returns a numeric vector)
subject1[[2]]

# get a single list value by name
subject1$temperature

# get several list items by specifying a vector of names
subject1[c("temperature", "flu_status")]

## access a list like a vector
# get values 2 and 3
subject1[2:3]

## Data frames -----

# create a data frame from medical patient data

pt_data <- data.frame(subject_name, temperature, flu_status, gender,
                      blood, symptoms, stringsAsFactors = FALSE)

# display the data frame
pt_data

## accessing a data frame

# get a single column
pt_data$subject_name

# get several columns by specifying a vector of names
pt_data[c("temperature", "flu_status")]

# this is the same as above, extracting temperature and flu_status
pt_data[2:3]

# accessing by row and column
pt_data[1, 2]

# accessing several rows and several columns using vectors
pt_data[c(1, 3), c(2, 4)]

## Leave a row or column blank to extract all rows or columns

# column 1, all rows
pt_data[, 1]
# row 1, all columns
pt_data[1, ]
# all rows and all columns
pt_data[ , ]

# the following are equivalent
pt_data[c(1, 3), c("temperature", "gender")]
pt_data[-2, c(-1, -3, -5, -6)]

# creating a Celsius temperature column
pt_data$temp_c <- (pt_data$temperature - 32) * (5 / 9)

# comparing before and after
pt_data[c("temperature", "temp_c")]

## Matrixes -----

# create a 2x2 matrix
m <- matrix(c(1, 2, 3, 4), nrow = 2)
m

# equivalent to the above
m <- matrix(c(1, 2, 3, 4), ncol = 2)
m

# create a 2x3 matrix
m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
m

# create a 3x2 matrix
m <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
m

# extract values from matrixes
m[1, 1]
m[3, 2]

# extract rows
m[1, ]

# extract columns
m[, 1]

##### Managing data with R ------------

## saving, loading, and removing R data structures

# show all data structures in memory
ls()

# remove the m and subject1 objects
rm(m, subject1)
ls()

rm(list=ls())

##### Exploring and understanding data --------------------

## data exploration example using used car data
usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)

# get structure of used car data
str(usedcars)

## Exploring numeric variables -----

# summarize numeric variables
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])

# calculate the mean income
(36000 + 44000 + 56000) / 3
mean(c(36000, 44000, 56000))

# the median income
median(c(36000, 44000, 56000))

# the min/max of used car prices
range(usedcars$price)

# the difference of the range
diff(range(usedcars$price))

# IQR for used car prices
IQR(usedcars$price)

# use quantile to calculate five-number summary
quantile(usedcars$price)

# the 99th percentile
quantile(usedcars$price, probs = c(0.01, 0.99))

# quintiles
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

# boxplot of used car prices and mileage
boxplot(usedcars$price, main="Boxplot of Used Car Prices",
      ylab="Price ($)")

boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage",
      ylab="Odometer (mi.)")

# histograms of used car prices and mileage
hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price ($)")

hist(usedcars$mileage, main = "Histogram of Used Car Mileage",
     xlab = "Odometer (mi.)")

# variance and standard deviation of the used car data
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

## Exploring numeric variables -----

# one-way tables for the used car data
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

# compute table proportions
model_table <- table(usedcars$model)
prop.table(model_table)

# round the data
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

## Exploring relationships between variables -----

# scatterplot of price vs. mileage
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")

# new variable indicating conservative colors
usedcars$conservative <-
  usedcars$color %in% c("Black", "Gray", "Silver", "White")

# checking our variable
table(usedcars$conservative)

# Crosstab of conservative by model
library(gmodels)
CrossTable(x = usedcars$model, y = usedcars$conservative)
