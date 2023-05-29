##### Chapter 11: Being Successful with Machine Learning --------------------

## Example: Using ggplot2 for visual data exploration ----

# read the titanic training dataset
titanic_train <- read.csv("titanic_train.csv")

# examine the features
str(titanic_train)

# load ggplot2 package and apply to Titanic dataset
library(ggplot2)
p <- ggplot(data = titanic_train)
p # creates an empty gray plot

# compare built-in boxplot to ggplot2 boxplot
boxplot(titanic_train$Age) # use R's built-in boxplot()
p + geom_boxplot(mapping = aes(y = Age)) # use ggplot2 boxplot

# boxplot examining relationship between age and survival
p + geom_boxplot(mapping = aes(x = Age, y = as.factor(Survived)))

# compare built-in histogram to ggplot2 histogram
hist(titanic_train$Age) # use R's built-in hist()
p + geom_histogram(aes(x = Age)) # use ggplot2 histogram

# overlapping histograms
p + geom_histogram(aes(x = Age, fill = as.factor(Survived))) +
  ggtitle("Distribution of Age by Titanic Survival Status")

# side-by-side histograms
p + geom_histogram(aes(x = Age)) +
  facet_grid(cols = vars(Survived)) +
  ggtitle("Distribution of Age by Titanic Survival Status")

# overlapping density plots
p + geom_density(aes(x = Age,
                     color = as.factor(Survived),
                     fill = as.factor(Survived)),
                 alpha = 0.25) +
  ggtitle("Density of Age by Titanic Survival Status")

# bar chart of passenger counts by sex
p + geom_bar(aes(x = Sex)) +
  ggtitle("Titanic Passenger Counts by Gender")

# bar chart of survival probability by sex
p + geom_bar(aes(x = Sex, y = Survived),
             stat = "summary", fun = "mean") +
    ggtitle("Titanic Survival Rate by Gender")

# bar chart of survival probability by passenger class
p + geom_bar(aes(x = Pclass, y = Survived),
             stat = "summary", fun = "mean") +
    ggtitle("Titanic Survival Rate by Passenger Class")

# stacked bar chart of survival by passenger class
p + geom_bar(aes(x = Pclass,
                 fill = factor(Survived,
                               labels = c("No", "Yes")))) +
  labs(fill = "Survived") +
  ylab("Number of Passengers") +
  ggtitle("Titanic Survival Counts by Passenger Class")

# survivival status by passenger class
p + geom_bar(aes(x = Pclass,
                 fill = factor(Survived,
                               labels = c("No", "Yes"))),
             position = "fill") +
  labs(fill = "Survived") +
  ylab("Proportion of Passengers") +
  ggtitle("Titanic Survival by Passenger Class")

# bar chart of interaction between class and sex
p + geom_bar(aes(x = Pclass, y = Survived, fill = Sex),
           position = "dodge", stat = "summary", fun = "mean") +
  ylab("Survival Proportion") +
  ggtitle("Titanic Survival Rate by Class and Sex")

