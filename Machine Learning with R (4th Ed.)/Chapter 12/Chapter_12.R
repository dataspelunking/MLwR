##### Chapter 12: Advanced Data Preparation --------------------

## Exploring R's tidyverse ----

library(tidyverse) # load all tidyverse packages

# convert the Titanic training dataset into a tibble
library(tibble) # not necessary if tidyverse is already loaded
titanic_csv <- read.csv("titanic_train.csv")
titanic_tbl <- as_tibble(titanic_csv)
titanic_tbl

# read the titanic training dataset using readr
library(readr) # not necessary if tidyverse is already loaded
titanic_train <- read_csv("titanic_train.csv")

# read the titanic training dataset using readxl
library(readxl)
titanic_train <- read_excel("titanic_train.xlsx")

# preparing and piping data with dplyr
library(dplyr)

# filter for female rows only
titanic_train |> filter(Sex == "female")

# select only name, sex, and age columns
titanic_train |> select(Name, Sex, Age)

# combine multiple dplyr verbs and save output to a tibble
titanic_women <- titanic_train |>
  filter(Sex == "female") |>
  select(Name, Sex, Age) |>
  arrange(Name)

# create a new feature indicating elderly age
titanic_train |>
  mutate(elderly = if_else(Age >= 65, 1, 0))

# create multiple features within the same mutate command
titanic_train |>
  mutate(
    elderly = if_else(Age >= 65, 1, 0),
    child = if_else(Age < 18, 1, 0)
  )

# compute survival rate by gender
titanic_train |>
  group_by(Sex) |>
  summarize(survival_rate = mean(Survived))

# compute average survival rate for children vs. non-children
titanic_train |>
  filter(!is.na(Age)) |>
  mutate(child = if_else(Age < 18, 1, 0)) |>
  group_by(child) |>
  summarize(survival_rate = mean(Survived))

# transform the dataset and pipe into a decision tree
library(rpart)
m_titanic <- titanic_train |>
  filter(!is.na(Age)) |>
  mutate(AgeGroup = if_else(Age < 18, "Child", "Adult")) |>
  select(Survived, Pclass, Sex, AgeGroup) |>
  rpart(formula = Survived ~ ., data = _)

library(rpart.plot)
rpart.plot(m_titanic)

## Transforming text with stringr ----
library(readr)
titanic_train <- read_csv("titanic_train.csv")

library(stringr)

# examine cabin prefix code
titanic_train <- titanic_train |>
  mutate(CabinCode = str_sub(Cabin, start = 1, end = 1))

# compare cabin prefix to passenger class
table(titanic_train$Pclass, titanic_train$CabinCode,
      useNA = "ifany")

# plot of survival probability by cabin code
library(ggplot2)
titanic_train |> ggplot() +
  geom_bar(aes(x = CabinCode, y = Survived),
             stat = "summary", fun = "mean") +
  ggtitle("Titanic Survival Rate by Cabin Code")

# look at the first few passenger names
head(titanic_train$Name)

# create a title / salutation feature
titanic_train <- titanic_train |>
  # use regular expressions to find the characters between the comma and period
  mutate(Title = str_extract(Name, ", [A-z]+\\."))

# look at the first few examples
head(titanic_train$Title)

# clean up the title feature
titanic_train <- titanic_train |>
  mutate(Title = str_replace_all(Title, "[, \\.]", ""))

# examine output
table(titanic_train$Title)

# group titles into related categories
titanic_train <- titanic_train |>
  mutate(TitleGroup = recode(Title,
    # the first few stay the same
    "Mr" = "Mr", "Mrs" = "Mrs", "Master" = "Master",
    "Miss" = "Miss",
    # combine  variants of "Miss"
    "Ms" = "Miss", "Mlle" = "Miss", "Mme" = "Miss",
    # anything else will be "Other"
    .missing = "Other",
    .default = "Other"
    )
  )

# examine output
table(titanic_train$TitleGroup)

# plot of survival probability by title group
library(ggplot2)
titanic_train |> ggplot() +
  geom_bar(aes(x = TitleGroup, y = Survived),
           stat = "summary", fun = "mean") +
  ggtitle("Titanic Survival Rate by Salutation")

## Cleaning dates with lubridate ----

library(lubridate)

# reading in Machine Learning with R publication dates in different formats
mdy(c("October 25, 2013", "10/25/2013"))
dmy(c("25 October 2013", "25.10.13"))
ymd("2013-10-25")

# construct MLwR publication dates
MLwR_1stEd <- mdy("October 25, 2013")
MLwR_2ndEd <- mdy("July 31, 2015")
MLwR_3rdEd <- mdy("April 15, 2019")

# compute differences (returns a difftime object)
MLwR_2ndEd - MLwR_1stEd
MLwR_3rdEd - MLwR_2ndEd

# convert the differences to durations
as.duration(MLwR_2ndEd - MLwR_1stEd)
as.duration(MLwR_3rdEd - MLwR_2ndEd)

# convert the duration to years
dyears()
as.duration(MLwR_2ndEd - MLwR_1stEd) / dyears()
as.duration(MLwR_3rdEd - MLwR_2ndEd) / dyears()

# easier-to-remember version of the above:
time_length(MLwR_2ndEd - MLwR_1stEd, unit = "years")
time_length(MLwR_3rdEd - MLwR_2ndEd, unit = "years")

# compute age (in duration)
USA_DOB <- mdy("July 4, 1776") # USA's Date of Birth
time_length(mdy("July 3 2023") - USA_DOB, unit = "years")
time_length(mdy("July 5 2023") - USA_DOB, unit = "years")

# compute age (using intervals)
interval(USA_DOB, mdy("July 3 2023")) / years()
interval(USA_DOB, mdy("July 5 2023")) / years()

# compute age (using integer divison)
USA_DOB %--% mdy("July 3 2023") %/% years()
USA_DOB %--% mdy("July 5 2023") %/% years()

# function to compute calendar age
age <- function(birthdate) {
  birthdate %--% today() %/% years()
}

# compute age of celebrities
age(mdy("Jan 12, 1964")) # Jeff Bezos
age(mdy("June 28, 1971")) # Elon Musk
age(mdy("Oct 28, 1955")) # Bill Gates
